module Lib.Utils where

-- Defining a couple of useful thins for later

type MarkedList a = [(a, Bool)]

type Map k v = [(k, v)]

-- MarkedList stuff

getMarked :: MarkedList a -> [a]
getMarked = map fst . filter snd

marks :: Eq a => [a] -> [a] -> [Bool]
marks as bs = map (`elem` bs) as

markList :: Eq a => [a] -> [a] -> MarkedList a
markList as bs = zip as (marks as bs)

-- Pretend we have maps and we never ask for bad keys

val :: Eq k => k -> Map k v -> v
val k = snd . head . filter ((== k) . fst)

dropKeys :: Eq k => k -> Map k v -> Map k v
dropKeys k = dropWhile ((/= k) . fst)

dropValues :: Eq v => v -> Map k v -> Map k v
dropValues v = dropWhile ((/= v) . snd)

keys :: Map k v -> [k]
keys = map fst

vals :: Map k v -> [v]
vals = map snd
