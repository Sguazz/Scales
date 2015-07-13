module Lib.DataTypes where

------------------
-- Data & Types --
------------------

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Eq, Ord, Show, Read, Enum)

data Scale = Major | Minor | Harmonic | Pentatonic | Blues
    deriving (Eq, Show, Read, Enum)

data Interval = Root       | MinorSecond   | Second          | MinorThird   |
                Third      | PerfectFourth | DiminishedFifth | PerfectFifth |
                MinorSixth | Sixth         | MinorSeventh    | Seventh
    deriving (Eq, Ord, Show, Read, Enum)

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
    deriving (Eq, Ord, Show, Read, Enum)

type Key = Note

-----------
-- Setup --
-----------

scale Major      = [Root, Second, Third, PerfectFourth, PerfectFifth, Sixth, Seventh]
scale Minor      = [Root, Second, MinorThird, PerfectFourth, PerfectFifth, MinorSixth, MinorSeventh]
scale Harmonic   = [Root, Second, MinorThird, PerfectFourth, PerfectFifth, MinorSixth, Seventh]
scale Pentatonic = [Root, Second, Third, PerfectFifth, Sixth]
scale Blues      = [Root, Second, MinorThird, Third, PerfectFifth, Sixth]

-- Infinite lists are cool

notes      = cycle [C ..]
intervals  = cycle [Root ..]
modes      = cycle [Ionian ..]

noteCount = length [C ..]

tops :: Eq a => [a] -> [a]
tops = take noteCount
