module Lib.Menu where

import Control.Monad.State
import Data.List
import Data.Maybe
import System.IO

import Lib.DataTypes
import Lib.Layout

-----------
-- State --
-----------

type MenuState = (String, String, String, String)
--                Wheel   Scale   Mode    Key

makeState :: Scale -> Mode -> Key -> MenuState
makeState s m k = (head $ list "Wheel", show s, show m, show k)

parseState :: MenuState -> (Scale, Mode, Key)
parseState (_, s, m, k)= (read s, read m, read k)

getWheel :: MenuState -> String
getWheel (w, _, _, _) = w

setWheel :: MenuState -> String -> MenuState
setWheel (_, s, m, k) w = (w, s, m, k)

getStateValue :: String -> MenuState -> String
getStateValue "Scale" (_, s, _, _) = s
getStateValue "Mode"  (_, _, m, _) = m
getStateValue "Key"   (_, _, _, k) = k

setStateValue :: String -> MenuState -> String -> MenuState
setStateValue "Scale" (w, _, m, k) s = (w, s, m, k)
setStateValue "Mode"  (w, s, _, k) m = (w, s, m, k)
setStateValue "Key"   (w, s, m, _) k = (w, s, m, k)

list :: String -> [String]
list "Wheel" = ["Key", "Scale", "Mode"]
list "Key"   = map show [C ..]
list "Mode"  = map show [Ionian ..]
list "Scale" = map show [Major ..]

wheels = list "Wheel"

-- Controls

doStuff :: MenuState -> Char -> IO MenuState
doStuff s@(w,_,_,_) 'D' = return $ setWheel s $ menuStep wheels w (-1)
doStuff s@(w,_,_,_) 'C' = return $ setWheel s $ menuStep wheels w   1
doStuff s@(w,_,_,_) 'B' = return $ setStateValue w s $ menuStep (list w) (getStateValue w s)   1
doStuff s@(w,_,_,_) 'A' = return $ setStateValue w s $ menuStep (list w) (getStateValue w s) (-1)
doStuff s            c  = return s

getAction :: IO Char
getAction = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getChar

menuStep :: [String] -> String -> Int -> String
menuStep m i s = m !! index
  where n = fromJust $ i `elemIndex` m
        index = min (max (n+s) 0) (length m - 1)

---------------
-- Interface --
---------------

menuLayout :: Scale -> Mode -> Key -> [String]
menuLayout s m k = menu $ makeState s m k

menu :: MenuState -> [String]
menu s = columns $ map wheel (list "Wheel")
  where wheel w = spinWheel (getWheel s == w) (list w) (getStateValue w s)

spinWheel :: Bool -> [String] -> String -> [String]
spinWheel f xs s = columnLayout " " (cursor f) elements
  where elements = coloredElements $ spinElements xs s
        cursor True  = [" ", ">", " "]
        cursor False = [" ", " ", " "]

spinElements :: [String] -> String -> [String]
spinElements xs s = take 3 . drop n $ xs'
  where xs' = padList $ [""] ++ xs ++ [""]
        n = fromJust $ s `elemIndex` xs

coloredElements :: [String] -> [String]
coloredElements [s1,s2,s3] = [ grey  ++ s1 ++ clear
                             , clear ++ s2 ++ clear
                             , grey  ++ s3 ++ clear ]
