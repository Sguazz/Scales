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

allStrings :: Scale -> Mode -> Key -> [Note] -> [[Maybe Interval]]
allStrings s m k = map (stringNotes s m k)

stringNotes :: Scale -> Mode -> Key -> Note -> [Maybe Interval]
stringNotes s m k n = map interval (chromatic n)
  where scale = tops' $ scaleWithIntervals s m k
        interval c = safeVal c scale

-- Guitar neck

bar   = grey ++ "|"
off   = blue ++ "-"

fret :: Maybe Interval -> String
fret Nothing             = off
fret (Just Root)         = cyan   ++ "X"
fret (Just MinorThird)   = yellow ++ "t"
fret (Just Third)        = yellow ++ "T"
fret (Just PerfectFifth) = green  ++ "F"
fret _                   = red    ++ "+"

guitarNeck :: Scale -> Mode -> Key -> [String]
guitarNeck s m k = columnLayout " " ("" : captions) (neckHeader : strings)
  where captions = map show guitarStrings
        strings = allGuitarStrings $ allStrings s m k guitarStrings

allGuitarStrings :: [[Maybe Interval]] -> [String]
allGuitarStrings = map guitarString

guitarString :: [Maybe Interval] -> String
guitarString = (++ clear) . intercalate bar . map fullFret . take neckLength
    where fullFret f = off ++ fret f ++ off

neckHeader :: String
neckHeader = unwords . take neckLength $ frets
  where frets = cycle . tops . map fret $ [0..]
        fret n | n == 0            = " : "
        fret n | n `elem` neckDots = " " ++ show n ++ " "
        fret _                     = "   "
