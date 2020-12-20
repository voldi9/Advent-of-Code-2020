{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Aoc(module Aoc, module Prelude, module Data.List, module Data.Maybe, module Data.Bits) where

import Data.Bits
import Data.List
import Data.Maybe
import qualified Text.Parsec as Parsec

splitOn :: Char -> String -> [String]
splitOn c s = filter (not . null) . foldr f [""] $ s where
  f c' (x:xs)
    | c' == c   = "":x:xs
    | otherwise = (c':x):xs

splitOnNewline :: String -> [String]
splitOnNewline s = splitOn '\n' s

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)
