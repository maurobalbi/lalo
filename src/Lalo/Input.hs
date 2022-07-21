module Lalo.Input
  where

import Data.Text (Text)

data Input
  = Path FilePath
  -- ^ The path to the code
  | Code String Text
  -- ^ Source code: @Code name content@
  deriving (Eq, Show)
