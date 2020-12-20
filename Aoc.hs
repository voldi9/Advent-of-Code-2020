{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Aoc(module Aoc, module Prelude, module Data.List, module Data.Maybe) where

import Data.List
import Data.Maybe

splitOnNewline :: String -> [String]
splitOnNewline s = filter (not . null) . foldr f [""] $ s where
  f c (x:xs)
    | c == '\n' = "":x:xs
    | otherwise = (c:x):xs
