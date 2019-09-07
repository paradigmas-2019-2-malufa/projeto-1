module Common (
    myClearScreen,
    readChar,
    lowerString,
    lowerChar,
    takeName1,
    takeName2,
    pause,
    notelem
) where

import System.IO
import System.Process
import Control.Monad
import System.Console.ANSI -- for clearScreen
import Data.Char


myClearScreen = do system "clear || cls"

readChar = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getChar

lowerString = map toLower
lowerChar char = toLower char

takeName1 (name1, _) = name1
takeName2 (_, name2) = name2

pause = putStrLn "Press any key to continue..." >> readChar

notelem :: (Eq a) => a -> [a] -> Bool
notelem a list = ((a `elem` list)==False)
