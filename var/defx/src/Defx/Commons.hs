module Defx.Commons where

import           Data.Char (toLower)
import qualified Data.Text as T


-- | Type definition for currency codes.
type Currency = T.Text


lowerFirst :: String -> String
lowerFirst []       = []
lowerFirst (x : xs) = toLower x : xs
