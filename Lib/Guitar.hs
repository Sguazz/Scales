module Lib.Guitar where

import Data.List

import Lib.DataTypes
import Lib.Layout
import Lib.Stuff
import Lib.Utils

-- Setup

guitarStrings = [E, B, G, D, A, E]
neckDots      = [3, 5, 7, 9]
neckLength    = 13

-- Guitar strings stuff

stringMarks :: Scale -> Mode -> Key -> Note -> [Bool]
stringMarks s m k n = map snd . dropKeys n $ markNotes s m k

allStrings :: Scale -> Mode -> Key -> [Note] -> [[Bool]]
allStrings s m k = map (stringMarks s m k)

-- Guitar neck

bar   = grey ++ "|"
on    = red  ++ "x"
off   = blue ++ "-"

fret :: Bool -> String
fret True  = on
fret False = off

guitarNeck :: Scale -> Mode -> Key -> [String]
guitarNeck s m k = columnLayout " " ("" : captions) (neckHeader : strings)
  where captions = map show guitarStrings
        strings = allGuitarStrings $ allStrings s m k guitarStrings

allGuitarStrings :: [[Bool]] -> [String]
allGuitarStrings = map guitarString

guitarString :: [Bool] -> String
guitarString = (++ clear) . intercalate bar . map fullFret . take neckLength
    where fullFret f = off ++ fret f ++ off

neckHeader :: String
neckHeader = unwords . take neckLength $ frets
  where frets = cycle . tops . map fret $ [0..]
        fret n | n == 0            = " : "
        fret n | n `elem` neckDots = " " ++ show n ++ " "
        fret _                     = "   "
