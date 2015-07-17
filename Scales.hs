module Scales where

import System.Environment

import Lib.DataTypes
import Lib.Guitar
import Lib.Layout
import Lib.Menu
import Lib.Stuff
import Lib.Utils

-- Things we want to see

colWidth = 25

scaleColumn :: Scale -> Mode -> Key -> [String]
scaleColumn s m k = mapWithHeader title (scaleWithIntervals s m k)
  where title = pad colWidth $ show k ++ " " ++ show s ++ " " ++ show m

relativeColumn :: Mode -> Key -> [String]
relativeColumn m k = mapWithHeader title (relativeModes m k)
  where title = pad colWidth $ "Same as..."

modulationColumn :: Mode -> Key -> [String]
modulationColumn m k = mapWithHeader title (modulations m k)
  where title = pad colWidth $ "Play these " ++ show m ++ " modes to get..."

-- Layout

layout :: (Scale, Mode, Key) -> [String]
layout (s, m, k) = rows [ columns [ scaleColumn s m k
                                  , relativeColumn m k
                                  , modulationColumn m k ]
                        , guitarNeck s m k ]

printEveryting :: MenuState -> IO ()
printEveryting s = fullScreenDisplay $ rows [ menu s
                                            , ["- - - - - - - - - - - - - - -"]
                                            , layout (parseState s) ]

app :: MenuState -> IO MenuState
app s = printEveryting s >> getAction >>= doStuff s

loop :: MenuState -> IO MenuState
loop s = app s >>= loop

main = do
    [key, scale, mode] <- getArgs
    loop ("Key", scale, mode, key)
