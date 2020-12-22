{-# LANGUAGE ScopedTypeVariables, TypeFamilies, OverloadedStrings #-}
module Aoc(module Aoc, module Prelude, module Data.List, module Data.List.Split, module Data.Maybe, module Data.Bits, module Data.Char, module Text.Read, module Text.Regex) where

import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read
import Text.Regex
import qualified Text.Parsec as Parsec

splitOnNewline :: String -> [String]
splitOnNewline s = splitOn "\n" s

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)
