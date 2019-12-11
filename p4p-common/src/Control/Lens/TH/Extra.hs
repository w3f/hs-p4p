module Control.Lens.TH.Extra where

import           Control.Lens        (set)
import           Control.Lens.TH     (FieldNamer, LensRules, defaultFieldRules,
                                      lensField, makeLensesWith, mappingNamer)
import           Language.Haskell.TH (DecsQ, Name)

underscorePrefixer :: FieldNamer
underscorePrefixer = mappingNamer (pure . ("_" <>))

underscorePrefixerRules :: LensRules
underscorePrefixerRules = set lensField underscorePrefixer defaultFieldRules

makeLenses_ :: Name -> DecsQ
makeLenses_ = makeLensesWith underscorePrefixerRules
