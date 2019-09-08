module RockPaperScissors (
    optionList,
    victoryCombinations,
    startRockPaperScissors,
    getOption,
    runGame,
    selectRandomElement
) where


import System.Process
import System.Random
import Data.List
import Data.Char
import Common
import PlayerModule

optionList = ["rock", "paper", "scissors"]
victoryCombinations = ["rock->scissors", "scissors->paper", "paper->rock"]


-- startRockPaperScissors :: IO ()
startRockPaperScissors twoPlayers = do 
    myClearScreen
    let playerIO = loadPlayer (takeName1 twoPlayers)
    player <- playerIO
    putStrLn "\nChoose your play"
    putStrLn "1 - Rock"
    putStrLn "2 - Paper"
    putStrLn "3 - Scissors"
    putStrLn "YOUR PLAY: "
    op <- readChar
    let myPlay = getOption (digitToInt op)
    runGame myPlay player
    putStrLn "\n\nPlay again? (y/n)"
    replay <- readChar
    if (lowerChar replay) == 'y' 
    then startRockPaperScissors twoPlayers
    else putStrLn "Thanks for playing :)"


getOption :: Int -> String
getOption x = optionList !! (x-1)

runGame myOption player = do 
    computerOption <- selectRandomElement optionList
    myClearScreen
    
    if myOption == computerOption 
    then do
        putStrLn "Draw!"
    else if elem (myOption ++ "->" ++ computerOption) victoryCombinations
        then do
            savePlayerToFile (addScorePlayer 1 player)
            putStrLn ("Your choice: "++ myOption)
            putStrLn ("Computer's choice: "++ computerOption)
            putStrLn "You Win!!! you got 1 point"
            -- pause
            -- putStr ""
            -- return ()
    else do
        savePlayerToFile (addScorePlayer (-1) player)
        putStrLn ("Your choice: "++ myOption)
        putStrLn ("Computer's choice: "++ computerOption)
        putStrLn "You Lose :( you lost 1 point"
        -- pause
        -- putStr ""
        -- return ()

selectRandomElement :: [a] -> IO a
selectRandomElement [] = error "Cannot select an element from an empty list."
selectRandomElement list = randomIntWithinRange >>=
    \r -> return $ list !! r
    where
        randomIntWithinRange = getStdRandom $ randomR (0, length list - 1)
