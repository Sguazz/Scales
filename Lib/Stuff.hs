module Lib.Stuff where

import Lib.DataTypes
import Lib.Utils

-- Scales stuff

markNotes :: Scale -> Mode -> Key -> MarkedList Note
markNotes s m k = zip (chromatic k) (modeMarks s m)

chromatic :: Key -> [Note]
chromatic k = dropWhile (/= k) notes

allScales :: Scale -> Mode -> [[Note]]
allScales s m = map (scaleNotes s m) notes

scaleNotes :: Scale -> Mode -> Key -> [Note]
scaleNotes s m k = getMarked $ markNotes s m k

scaleIntervals :: Scale -> Mode -> [Interval]
scaleIntervals s m = getMarked $ markIntervals s m

-- Intervals stuff

markIntervals :: Scale -> Mode -> MarkedList Interval
markIntervals s m = zip intervals (modeMarks s m)

intervalMarks :: Scale -> [Bool]
intervalMarks s = marks intervals (scale s)

-- Modes stuff

modeList :: Mode -> [Mode]
modeList m = dropWhile (/= m) modes

modeMarks :: Scale -> Mode -> [Bool]
modeMarks s m = drop (modeGrade m) (intervalMarks s)

modeGrade :: Mode -> Int
modeGrade m = val m . zip modes . getMarked . zip [0..] $ intervalMarks Major

findModulation :: Mode -> Key -> Mode -> Key
findModulation m k t = head $ drop grade (chromatic k)
  where grade = noteCount + modeGrade m - modeGrade t

-- Putting the pieces together

scaleWithIntervals :: Scale -> Mode -> Key -> Map Note Interval
scaleWithIntervals s m k = zip (scaleNotes s m k) (scaleIntervals s m)

relativeModes :: Mode -> Key -> Map Note Mode
relativeModes m k = dropValues Ionian $ zip (scaleNotes Major m k) (modeList m)

modulations :: Mode -> Key -> Map Note Mode
modulations m k = dropValues Ionian $ zip ns (modeList m)
  where ns = map (findModulation m k) (modeList m)
