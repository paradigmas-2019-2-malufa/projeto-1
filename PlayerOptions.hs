module PlayerOptions (
    printOptions,
    optionsAction,
    playerOptions
) where

import PlayerModule
import System.IO
import Common

printOptions :: Player j -> IO ()
printOptions player1 = do
    myClearScreen
    putStrLn "Player 1 info:"
    showPlayer player1
    putStr "\nChoose an option:\n"
    putStr "1) Show players hanking\n"
    putStr "2) Create player account\n"
    putStr "e) exit to menu\n"

optionsAction '1' twoPlayers = do
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
        playerOptions twoPlayers

optionsAction '2' twoPlayers = do createAccount

optionsAction 'e' twoPlayers = do
    putStrLn "going back to menu..."

optionsAction _ twoPlayers = do
    putStrLn "invalid option"
    playerOptions twoPlayers

playerOptions twoPlayers = do
    let playerIO = loadPlayer (takeName1 twoPlayers)
    player <- playerIO
    printOptions player

    choice <- readChar
    optionsAction choice twoPlayers
