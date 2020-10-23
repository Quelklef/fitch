module StringUtil exposing (..)

import String exposing (..)

head : String -> Maybe Char
head = uncons >> Maybe.map (\(char, rest) -> char)

drop : Int -> String -> String
drop n string = string |> slice n (length string)

take : Int -> String -> String
take n string = string |> slice 0 n

takeWhile : (Char -> Bool) -> String -> String
takeWhile pred string = case uncons string of
  Nothing -> ""
  Just (char, rest) ->
    if pred char
    then String.cons char (takeWhile pred rest)
    else ""

dropWhile : (Char -> Bool) -> String -> String
dropWhile pred string = case uncons string of
  Nothing -> ""
  Just (char, rest) ->
    if pred char
    then dropWhile pred rest
    else rest

get : Int -> String -> Maybe Char
get idx string = case String.uncons string of
  Just (char, rest) -> if idx == 0 then Just char else get (idx - 1) rest
  Nothing -> Nothing

if_ : Bool -> String -> String
if_ bool str = if bool then str else ""

padTo : Char -> Int -> String -> String
padTo padding length str = str ++ String.repeat (length - String.length str) (String.fromChar padding)
