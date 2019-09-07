module PlayerOptions (
    printOptions,
    optionsAction,
    playerOptions
) where

import PlayerModule
import System.IO
import Common

printOptions :: IO ()
printOptions = do
    myClearScreen
    putStr "Choose an option:\n"
    putStr "1) Show players hanking\n"
    putStr "2) Create player account\n"
    putStr "e) exit to menu\n"

optionsAction '1' = do
    let playersIO = loadPlayers
    players <- playersIO
    if players == [] then do
        myClearScreen
        putStr "There aren't players registered\nLet's create an account: \n"
        pause
        createAccount
    else do
        putStrLn "\n  << Players hanking >>\n"
        showPlayers (sortPlayersByScore players)
        putStrLn "\n"
        pause
        playerOptions

optionsAction '2' = do createAccount

optionsAction 'e' = do
    putStrLn "going back to menu..."

optionsAction _ = do
    putStrLn "invalid option"
    playerOptions

playerOptions = do
    printOptions
    choice <- readChar
    optionsAction choice
