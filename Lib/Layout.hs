module Lib.Layout where

import Data.List
import System.Process

import Lib.DataTypes
import Lib.Utils

--------------------
-- Printing stuff --
--------------------

-- Whatever

clear = "\x1b[39m"
grey  = "\x1b[37m"
red   = "\x1b[31m"
blue  = "\x1b[34m"

hr    = "---------------"

padWith :: a -> Int -> [a] -> [a]
padWith p n s = s ++ replicate (n - length s) p

pad  = padWith ' '
pad' = padWith ""

padListWith :: (Int -> [a] -> [a]) -> [[a]] -> [[a]]
padListWith p ss = map (p l) ss
  where l = maximum . map length $ ss

padList  = padListWith pad
padList' = padListWith pad'

-- Such layout very impress wow

mapWithHeader :: (Show k, Show v, Eq k, Eq v) => String -> Map k v -> [String]
mapWithHeader t m = t : hr : showMap (nub . tops $ m)

showMap :: (Show k, Show v) => Map k v -> [String]
showMap m = columnLayout " - " ks vs
  where ks = map show (keys m)
        vs = map show (vals m)

rows :: [[String]] -> [String]
rows = intercalate [""]

columns :: [[String]] -> [String]
columns = foldl1 (columnLayout "     ")

columnLayout :: String -> [String] -> [String] -> [String]
columnLayout s c1 c2 = zipWith layout (padList c1') c2'
  where layout l1 l2 = l1 ++ s ++ l2
        [c1', c2'] = padList' [c1, c2]

fullScreenDisplay :: [String] -> IO ()
fullScreenDisplay ss = do
    system "clear"
    putStrLn $ unlines ss
