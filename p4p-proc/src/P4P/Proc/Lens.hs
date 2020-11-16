{-# LANGUAGE TemplateHaskell #-}

module P4P.Proc.Lens where

-- external
import           Control.Lens.TH   (makePrisms)

-- internal
import           P4P.Proc.Internal
import           P4P.Proc.Protocol


makePrisms ''GMsg

makePrisms ''Observation

makePrisms ''UMsg
