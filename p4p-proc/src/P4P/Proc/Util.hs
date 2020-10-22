{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module P4P.Proc.Util
  ( knot2ReactM
  , knot2UReactM
  , Can(..)
  )
where

-- external
import qualified Data.Sequence                    as Seq

--import Data.Void (Void)
import           Control.Applicative              (liftA2)
import           Control.Lens                     ((%%=), (%=), (^.), _1, _2,
                                                   _3)
import           Control.Monad.Extra              (whileJustM)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State.Strict (StateT (..), execStateT)
import           Data.Bifunctor                   (Bifunctor (..))
import           Data.Can                         (Can (..))
import           Data.Foldable                    (toList, traverse_)
import           Data.Sequence                    (Seq (..), (|>))
import           Data.Traversable                 (for)

-- internal
import           P4P.Proc


-- state of knot2ReactM as it runs - queue in, queue out
type Knot2State p1 p2 o
  = (Seq (Either (ProcMsgI p1) (ProcMsgI p2)), Int, Seq o)

-- https://github.com/emilypi/smash/pull/6
canWithMerge :: c -> (a -> c) -> (b -> c) -> (c -> c -> c) -> Can a b -> c
canWithMerge c _ _ _ Non       = c
canWithMerge _ f _ _ (One a  ) = f a
canWithMerge _ _ g _ (Eno b  ) = g b
canWithMerge _ f g m (Two a b) = m (f a) (g b)

canEach :: Monoid c => (a -> c) -> (b -> c) -> Can a b -> c
canEach f g = canWithMerge mempty f g (<>)

canEachA
  :: (Applicative m, Monoid c) => (a -> m c) -> (b -> m c) -> Can a b -> m c
canEachA f g = canWithMerge (pure mempty) f g (liftA2 (<>))

{- | Knot 2 processes - react to a combined input giving combined outputs.

The 2 processes can communicate both with the outside world, and with each
other. The creation / selection parameters passed into this function controls
how and to what degree this might happen.

It is possible that the two processes keep generating inputs to send to each
other, and never fully-consume them. In this case, the call to this function
will never finish. The caller is responsible for ensuring that this either (a)
never happens (e.g. because the processes are programmed "correctly"), or (b)
is handled appropriately by the @watchRun@ parameter.

@
                               +------+
                   +---->[mk]->|  P1  |->[sel]->---+
                  /   +->[I1]  +------+  [-O1]->+   \
                i1    |                         |   o1
                ^     +--<-o2i1-<-+ +-<-o1i2-<--+     v
   >--i-->[selIn]                  X                  [mkOut]>--o-->
                v     +--<-o1i2-<-+ +-<-o2i1-<--+     ^
                i2    |                         |   o2
                  \   +->[mk]  +------+  [sel]->+   /
                   +---->[I2]->|  P2  |->[-O2]->---+
                               +------+
@

-}
knot2ReactM
  :: forall p1 p2 i1 o1 o1i2 i2 o2 o2i1 i o m1 m2 m
   . Process p1
  => Process p2
  => Ctx p1 m1
  => Ctx p2 m2
  => Monad m
  => (i -> Can i1 i2)
  -- ^ Selects which process should receive the overall input.
  -- If both are selected, then WLOG p1 is run first.
  -> (Either o2i1 i1 -> ProcMsgI p1)
  -- ^ Creates the input to p1, from i1 and o2i1
  -> (Either o1i2 i2 -> ProcMsgI p2)
  -- ^ Creates the input to p2, from i2 and o1i2
  -> (ProcMsgO p1 -> Can o1 o1i2)
  -- ^ Selects where the output from p1 should be sent
  -> (ProcMsgO p2 -> Can o2 o2i1)
  -- ^ Selects where the output from p2 should be sent
  -> (Either o1 o2 -> o)
  -- ^ Creates the overall output
  -> (Int -> Int -> m ())
  {- ^ Peek into metadata about how the operation is running.

  This is run whenever a new input is generated internally from one process
  to another. The first argument is the current size of the queue, the second
  argument is the number of inputs generated so far. If either number goes
  higher than "reasonable", you may have an infinite loop on your hands.
  -}
  -> (forall a . m1 a -> m a)
  -> (forall a . m2 a -> m a)
  -> p1
  -> p2
  -> i
  -> m [o]
knot2ReactM selIn mkI1 mkI2 selO1 selO2 mkOut watchRun run1 run2 p1 p2 input =
  fmap (toList . (^. _3))
    $ flip execStateT (qIn, Seq.length qIn, Seq.empty)
    $ whileJustM runQ
 where
  qIn = canEach (pure . Left . mkI1 . Right)
                (pure . Right . mkI2 . Right)
                (selIn input)
  pushQ
    :: Either (ProcMsgI p1) (ProcMsgI p2) -> StateT (Knot2State p1 p2 o) m ()
  pushQ el = do
    size <- _1 %%= (\q -> (succ (Seq.length q), q |> el))
    incs <- _2 %%= (\i -> (succ i, succ i))
    lift $ watchRun size incs
  runQ :: StateT (Knot2State p1 p2 o) m (Maybe ())
  runQ = do
    maybeIn <- _1 %%= \case
      Seq.Empty -> (Nothing, Seq.Empty)
      h :<| tl  -> (Just h, tl)
    for maybeIn $ \case
      Left ui1 -> (lift (run1 (reactM p1 ui1)) >>=) $ traverse_ $ do
        canEachA (\o -> _3 %= (|> mkOut (Left o))) (pushQ . Right . mkI2 . Left)
          . selO1
      Right ui2 -> (lift (run2 (reactM p2 ui2)) >>=) $ traverse_ $ do
        canEachA (\o -> _3 %= (|> mkOut (Right o))) (pushQ . Left . mkI1 . Left)
          . selO2

{- | Knot 2 processes - react to a combined input giving combined outputs.

Like 'knot2ReactM' except it only affects the 'MsgUser' parts of the messages.

The process-to-process message type is assumed to be 'Void', i.e. the combined
process is a local-only process.

The auxiliary message output type is assumed to be 'Void' for process 2.
-}
-- brittany-disable-next-binding
-- https://github.com/lspitzner/brittany/issues/299
knot2UReactM
  :: forall p1 p2 i1 o1 o1i2 i2 o2 o2i1 ui uo m1 m2 m
   . Process p1
  => PMsg (State p1) ~ Void
  => Process p2
  => PMsg (State p2) ~ Void
  => AuxO (State p2) ~ Void
  => Ctx p1 m1
  => Ctx p2 m2
  => Monad m
  => (ui -> Can i1 i2)
  -- ^ Selects which process should receive the overall input.
  -- If both are selected, then WLOG p1 is run first.
  -> (Either o2i1 i1 -> UserI (State p1))
  -- ^ Creates the input to p1, from i1 and o2i1
  -> (Either o1i2 i2 -> UserI (State p2))
  -- ^ Creates the input to p2, from i2 and o1i2
  -> (UserO (State p1) -> Can o1 o1i2)
  -- ^ Selects where the output from p1 should be sent
  -> (UserO (State p2) -> Can o2 o2i1)
  -- ^ Selects where the output from p2 should be sent
  -> (Either o1 o2 -> uo)
  -- ^ Creates the overall output
  -> (Int -> Int -> m ())
  -> (forall a . m1 a -> m a)
  -> (forall a . m2 a -> m a)
  -> p1
  -> p2
  -> GMsg (RuntimeI ()) ui Void Void
  -> m [GMsg (RuntimeO Void) uo Void (AuxO (State p1))]
knot2UReactM selInU mkI1U mkI2U selO1U selO2U mkOutU =
  knot2ReactM @_ @_ @(GMsg (RuntimeI ()) i1 Void Void)
    @(GMsg (RuntimeO Void) o1 Void (AuxO (State p1)))
    @o1i2
    @(GMsg (RuntimeI ()) i2 Void Void)
    @(GMsg (RuntimeO Void) o2 Void Void)
    @o2i1
    selIn
    mkI1
    mkI2
    selO1
    selO2
    mkOut
 where
  selIn = \case
    MsgRT   r -> Two (MsgRT r) (MsgRT r)
    MsgUser u -> bimap MsgUser MsgUser (selInU u)
  mkI1 = \case
    Left  o2i1         -> MsgUser (mkI1U (Left o2i1))
    Right (MsgRT   r ) -> MsgRT r
    Right (MsgUser o2) -> MsgUser (mkI1U (Right o2))
  mkI2 = \case
    Left  o1i2         -> MsgUser (mkI2U (Left o1i2))
    Right (MsgRT   r ) -> MsgRT r
    Right (MsgUser o1) -> MsgUser (mkI2U (Right o1))
  selO1 = \case
    MsgRT   r -> One (MsgRT r)
    MsgUser u -> first MsgUser (selO1U u)
    MsgAux  a -> One (MsgAux a)
  selO2 = \case
    MsgRT   r -> One (MsgRT r)
    MsgUser u -> first MsgUser (selO2U u)
  mkOut = \case
    Left  (MsgRT   r) -> MsgRT r
    Left  (MsgUser u) -> MsgUser (mkOutU (Left u))
    Left  (MsgAux  a) -> MsgAux a
    Right (MsgRT   r) -> MsgRT r
    Right (MsgUser u) -> MsgUser (mkOutU (Right u))
