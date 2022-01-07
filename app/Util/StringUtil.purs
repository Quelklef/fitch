module Fitch.Util.StringUtil where

import Prelude
import Data.Maybe (Maybe (..))
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as String
import Data.Monoid (power)

get :: Int -> String -> Maybe CodePoint
get n = String.drop n >>> String.uncons >>> map _.head

padTo :: CodePoint -> Int -> String -> String
padTo pad len str = str <> (String.singleton pad) `power` (len - String.length str)
