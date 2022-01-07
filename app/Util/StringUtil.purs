module Fitch.Util.StringUtil where

import Prelude
import Data.Maybe (Maybe (..))
import Data.String.CodePoints (CodePoint)
import Data.String.CodePoints as String
import Data.Monoid (power)

head :: String -> Maybe CodePoint
head = String.uncons >>> map _.head

drop :: Int -> String -> String
drop = String.drop

take :: Int -> String -> String
take = String.take

takeWhile :: (CodePoint -> Boolean) -> String -> String
takeWhile pred = String.uncons >>> case _ of
  Nothing -> ""
  Just { head, tail } ->
    if pred head
    then String.singleton head <> takeWhile pred tail
    else ""

dropWhile :: (CodePoint -> Boolean) -> String -> String
dropWhile pred = String.uncons >>> case _ of
  Nothing -> ""
  Just { head, tail } ->
    if pred head
    then dropWhile pred tail
    else tail

get :: Int -> String -> Maybe CodePoint
get n = drop n >>> head

if_ :: Boolean -> String -> String
if_ cond s = if cond then s else ""

padTo :: CodePoint -> Int -> String -> String
padTo pad len str = str <> (String.singleton pad) `power` (len - String.length str)
