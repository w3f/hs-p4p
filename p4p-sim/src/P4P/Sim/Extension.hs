{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module P4P.Sim.Extension where

-- external
import           Control.Lens.Mutable.Extra       (Const (..), FakeAlloc2 (..),
                                                   SNat, newFakeAlloc2)
import           Control.Monad.Trans.Class        (MonadTrans (..))
import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import           Data.Functor                     (($>))
import           Data.Schedule                    (Tick)
import           Data.Void                        (absurd)
import           P4P.Proc                         (PRef, Proc (..),
                                                   Process (..), Protocol (..))
import           P4P.Proc.Util                    (Can (..), knot2UReactM)

-- internal
import           P4P.Sim.Internal
import           P4P.Sim.Types


data PSimX ref st pid p x = PSimX !(PSim ref st pid p) !x
type SimXProcess pid p x = (Process x, SimXProtocol pid (State p) (State x))

instance (
  Process (PSim ref st pid p),
  SimXProcess pid p x
 ) => Process (PSimX ref st pid p x) where
  type State (PSimX ref st pid p x) = SimFullState pid (State p) (State x)
  type Ctx (PSimX ref st pid p x) m = (Ctx (PSim ref st pid p) m, Ctx x m)

  proceed (SimFullState sp ss x) =
    PSimX <$> proceed (SimFullState sp ss ()) <*> proceed x

  suspend (PSimX s x) = ($>) <$> suspend s <*> suspend x

  getAddrsM _ = pure []

  localNowM (PSimX s x) = localNowM s

  reactM (PSimX s x) =
    knot2UReactM @_ @_ @(SimUserI pid (State p)) @(SimUserO pid (State p))
      @(UserI (State x))
      @(UserI (State x))
      @(XUserO (State x))
      @(SimUserI pid (State p))
      @(SimXUserI pid (State p) (State x))
      @(SimXUserO pid (State p) (State x))
      selInU
      mkI1U
      mkI2U
      selO1U
      selO2U
      mkOutU
      watchRun
      id
      id
      s
      x
   where
    selInU = \case
      SimExtensionI xi ->
        maybe Non Eno $ toUserI @pid @(State p) @(State x) (Right xi)
      i -> One (i $> error "unreachable, fmap into Void")
    mkI1U = either id id
    mkI2U = either id id
    selO1U o = case toUserI @_ @_ @(State x) (Left o) of
      Just xsi -> Two o xsi
      Nothing  -> One o
    selO2U xso = case fromUserO @_ @_ @(State x) xso of
      Left  i -> Eno i
      Right o -> One o
    mkOutU = \case
      Left  o  -> absurd <$> o
      Right xo -> SimExtensionO xo
    watchRun _ _ = pure () -- TODO: for now, don't try to detect loops

-- | Run a sim with an extension 'Process'.
runSimX
  :: forall pid p x xs_ m
   . SimProcess pid p
  => Ctx p m
  => Monad m
  => SimXProcess pid p x
  => Ctx x (StateT (FakeAlloc2 (SimRunState pid p) xs_) m)
  => m (Maybe (SimXI pid (State p) (State x)))
  -> (Tick -> [SimXO pid (State p) (State x)] -> m ())
  -> SimFullState pid (State p) (State x)
  -> m (SimFullState pid (State p) (State x))
{- API note:

This is fugly:

    Ctx x (StateT (FakeAlloc2 (SimRunState pid p) xs_) m)

It's due to our implementation of "instance Process (PSim ..)" in Internal.hs,
we can solve it in two ways:

1. Force the extension to reuse the StateT monad that PSim runs in, the current way
2. Hide the StateT monad that PSim runs in; this is also possible via UnMonadTrans:

   type Ctx (PSimX ref st pid p x) m = (Ctx (PSim ref st pid p) m, Ctx x (UnMonadTrans m))

   then sprinkle a few 'lift' into the implementation.

The downside with (2) is that the extension monad is then unable to opt-into
the StateT monad if it wants to also use it. Which we do, for the important
special case of Proc (see runSimXS). OTOH, with (1) the extension monad can
(largely) opt-out, just by lifting all its operations. However, it would then
need to use UnMonadTrans in its own Ctx, which is a bit ugly, and exposes more
of how PSim is implemented... ugh

Happily, runSimXS itself doesn't contain this fugliness and its API is clean.
However it only works on extension processes implemented as a pure Proc.
-}
runSimX simRunI simRunO s0 =
  simulate
      @(PSimX (Const (SNat 1)) (FakeAlloc2 (SimRunState pid p) xs_) pid p x)
      (lift simRunI)
      ((lift .) . simRunO)
      s0
    `evalStateT` newFakeAlloc2

-- | Run a sim with an extension 'Proc'.
runSimXS
  :: forall pid p xs m
   . SimProcess pid p
  => Ctx p m
  => Monad m
  => SimXProtocol pid (State p) xs
  => Proc xs
  => m (Maybe (SimXI pid (State p) xs))
  -> (Tick -> [SimXO pid (State p) xs] -> m ())
  -> SimFullState pid (State p) xs
  -> m (SimFullState pid (State p) xs)
runSimXS =
  runSimX @pid @p
    @(PRef (Const (SNat 2)) (FakeAlloc2 (SimRunState pid p) xs) xs)
    @xs
    @m
