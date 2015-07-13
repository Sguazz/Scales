module Lib.Menu where

import Data.List
import Data.Maybe

import Lib.Layout

-- Menu Controls

spinWheel :: (Show a, Eq a) => [a] -> a -> [String]
spinWheel xs s = columnLayout " " cursor elements
  where elements = coloredElements $ spinElements xs s

cursor = [" ", ">", " "]

coloredElements :: [String] -> [String]
coloredElements [s1,s2,s3] = [ grey  ++ s1 ++ clear
                           , clear ++ s2 ++ clear
                           , grey  ++ s3 ++ clear ]

spinElements :: (Show a, Eq a) => [a] -> a -> [String]
spinElements xs s = take 3 . drop n $ xs'
  where xs' = padList $ [""] ++ map show xs ++ [""]
        n = fromJust $ s `elemIndex` xs
