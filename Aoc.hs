{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Aoc(module Aoc, module Prelude, module Data.List, module Data.Maybe, module Data.Bits) where

import Data.Bits
import Data.List
import Data.Maybe

splitOn :: Char -> String -> [String]
splitOn c s = filter (not . null) . foldr f [""] $ s where
  f c' (x:xs)
    | c' == c   = "":x:xs
    | otherwise = (c':x):xs

splitOnNewline :: String -> [String]
splitOnNewline s = splitOn '\n' s

countItem :: Eq a => a -> [a] -> Int
countItem x s = length . filter (==x) $ s
