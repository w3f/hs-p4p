module Data.Text.Extra where

-- external
import qualified Data.Text as T

import           Data.Text (Text)


show :: Show a => a -> Text
show = T.pack . Prelude.show
