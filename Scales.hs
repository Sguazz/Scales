module Scales where

import System.Environment

import Lib.DataTypes
import Lib.Guitar
import Lib.Layout
import Lib.Menu
import Lib.Stuff
import Lib.Utils

-- Things we want to see

scaleColumn :: Scale -> Mode -> Key -> [String]
scaleColumn s m k = mapWithHeader title (scaleWithIntervals s m k)
  where title = show k ++ " " ++ show s ++ " " ++ show m

relativeColumn :: Mode -> Key -> [String]
relativeColumn m k = mapWithHeader title (relativeModes m k)
  where title = "Same as..."

modulationColumn :: Mode -> Key -> [String]
modulationColumn m k = mapWithHeader title (modulations m k)
  where title = "Play these " ++ show m ++ " modes to get..."

-- Layout

layout :: Scale -> Mode -> Key -> [String]
layout s m k =
    rows [ columns [ scaleColumn s m k
                   , relativeColumn m k
                   , modulationColumn m k ]
         , guitarNeck s m k ]

everything :: Scale -> Mode -> Key -> [String]
everything s m k = rows [ menuLayout s m k
                        , ["- - - - - - - - - - - - - - - - - -"]
                        , layout s m k ]

-- main = forever (printMenu >> readChoice >>= menuAction)

main = do
    [key, scale, mode] <- getArgs
    display $ everything (read scale) (read mode) (read key)
