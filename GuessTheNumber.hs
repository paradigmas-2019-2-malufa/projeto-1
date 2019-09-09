module GuessTheNumber(
    genSecret,
    getGuess,
    nextTurn,
    turn,
    startGuessTheNumber
) where

import System.Random
import PlayerModule
import Common

genSecret :: Int -> IO Int
getGuess :: (Int, Int) -> IO Int
nextTurn :: Int -> Int -> (Int, Int) -> Int -> IO Int--IO ()
turn :: Int -> (Int, Int) -> String -> Int -> IO Int--IO ()

-- Function responsable for generate the secret number.
genSecret max = do
    secret <- randomRIO (0, max)
    return secret

-- Constant for the maximum number of the list.
limit = 100

-- Function responsable for displaying the choose option for the player.
getGuess (low, high) = do
    putStrLn $ "> Choose a number between " ++ show low ++ " and " ++ show high
    guess <- getLine
    return $ read guess

-- Function responsable for decide the situation of the player in the game.
nextTurn secret guess (low, high) remainingAttempts
    | remainingAttempts == 0 = return 0
    | guess < low = turn secret (low, high) (show(guess) ++ "? Oh no, your guess is too low...") remainingAttempts
    | guess > high = turn secret (low, high) (show(guess) ++ "? Ouch! your guess is too high!") remainingAttempts
    | guess > secret = turn secret (low, guess - 1) (show(guess) ++ "? Try a lower number") remainingAttempts
    | guess < secret = turn secret (guess + 1, high) (show(guess) ++ "? Try a higher number") remainingAttempts
    | otherwise = return remainingAttempts --putStrLn ("VOCE ACERTOU! o numero secreto era " ++ (show secret))

-- Function to create the step by step of the current turn.
turn secret limits prompt remaingTurns = do
    myClearScreen
    putStrLn (show(remaingTurns) ++ " remaining attempts\n" ++ prompt)
    guess <- getGuess limits
    -- putStrLn ("Number typed: " ++ show(guess))
    nextTurnIO <- nextTurn secret guess limits (remaingTurns-1)
    -- nextTurnReturn <- nextTurnIO
    return nextTurnIO

-- Main function, responsable for make the quick start of the program.
startGuessTheNumber twoPlayers = do
    myClearScreen
    let playerIO = loadPlayer (takeName1 twoPlayers)
    player <- playerIO
    secret <- genSecret limit
    let remturnsIO = turn secret (1, limit) "Let's begin!" 6
    remainingAttempts <- remturnsIO
    myClearScreen
    if (remainingAttempts > 0)
    then do
        -- print "tu ganhou"
        putStrLn ("You win! The number was " ++ show(secret))
        putStrLn ("You got " ++ show(remainingAttempts) ++ " points")
        pause
        savePlayerToFile (addScorePlayer remainingAttempts player)
    else do
        putStrLn "You lose because you ran out of attempts! You lost 1 point"
        putStrLn ("The secret number is " ++ show(secret))
        pause
        savePlayerToFile (addScorePlayer (-1) player)
    return ()
