--Jokenpo--

import System.Process
import System.Random
import Data.List
import Data.Char


optionList = ["rock", "paper", "scissors"]
victoryCombinations = ["rock->scissors", "scissors->paper", "paper->rock"]


main :: IO ()
main = do system "clear || cls"
          putStrLn "\nChoose your play"
          putStrLn "1 - Rock"
          putStrLn "2 - Paper"
          putStrLn "3 - Scissors"
          putStrLn "YOUR PLAY: "
          op <- getChar
          getChar
          let myPlay = getOption (digitToInt op)
          runGame myPlay
          putStrLn "\n\nPlay again? (y/n)"
          replay <- getChar
          getChar
          if replay == 'y' 
            then main
          else putStrLn "Thanks for playing :)"


getOption :: Int -> String
getOption x = optionList !! (x-1)

runGame myOption = do computerOption <- selectRandomElement optionList
                      putStrLn ("Your choice: "++ myOption)
                      putStrLn ("Computer's choice: "++ computerOption)
                      if myOption == computerOption 
                        then putStrLn "Draw!"
                      else if elem (myOption ++ "->" ++ computerOption) victoryCombinations
                        then putStrLn "You Won!!!"
                      else putStrLn "You Lose :("

selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list = randomIntWithinRange >>=
  \r -> return $ list !! r
  where
  randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)