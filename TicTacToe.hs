-- startTicTacToe, [Player j], Player
module TicTacToe (
    startTicTacToe,
   menu,runOption,validPlayer,
    showRanking,order,getScore,getName,refreshScore,
    winPlayer1,winPlayer2,runGame,prepareGame,registerPlayer,
    newTable,newGame
) where

import Control.Exception
import System.Process
import System.IO.Error
import System.IO
import Data.Function
import Data.List --intersect
import PlayerModule
import Common

-- type [Player j] = [Player j]
type Turn = Int
-- type Name = String
-- type Score = Int
type GameTable = [Char]

-- data Player = Player Name Score
--     deriving (Show, Read, Eq)

startTicTacToe :: IO() 
startTicTacToe = do
    {catch (readFile) treat_error;}
    where
        readFile = do
        {
            file <- openFile "option.txt" ReadMode;
            option <- hGetLine file;
            hClose file;
            menu (read option);
            return ()
        }

        treat_error err = if isDoesNotExistError err then do
        {
            file <- openFile "option.txt" WriteMode;
            hPutStrLn file "[]";
            hClose file;
            menu [];
            return ();
        }
        else
            ioError err


menu :: [Player j] -> IO [Player j]
menu option = do
                system "clear || cls"
                putStrLn "::::::::::::::::::::::::::::::::::::::: Tic Tac Toe :::::::::::::::::::::::::::::::::::::::"
                putStrLn "\n1 - REGISTER A PLAYER"
                putStrLn "2 - PLAY"
                putStrLn "e - EXIT"
                putStrLn "OPTION: "
                op <- readChar
                runOption option op

runOption :: [Player j] -> Char -> IO [Player j]
runOption option '1' = registerPlayer option
runOption option '2' = prepareGame option
runOption option 'e' = do
    putStrLn("\nThanks for playing!\n")
    return option
    --return to selection game

runOption option _ = do
    putStrLn("\nInvalid option! Try again!")
    putStrLn ("\n Press enter to continue....")
    aux <- readChar
    menu option


registerPlayer :: [Player j] -> IO [Player j]
registerPlayer option = do
    putStrLn("\nEnter a user name: ")
    name <- getLine
    let playerIO = loadPlayer name
    player <- playerIO
    if((validPlayer option name) || (player /= nullPlayer)) then do
        putStrLn ("This player "++ (show name) ++" already exists, try another...")
        pause
        menu option
    else do
        file <- openFile "option.txt" WriteMode
        let newPlayer = newPlayerData name 0
        hPutStrLn file (show (newPlayer:option))
        hClose file
        savePlayerToFile newPlayer
        putStrLn("Player " ++ name ++ " successfully registered!!")
        pause
        menu (newPlayer:option)

validPlayer :: [Player j] -> Name -> Bool
validPlayer [] _ = False
validPlayer (headPlayer:xs) name --s:xs
                |((getName headPlayer) == name) = True
                |otherwise = validPlayer xs name

prepareGame :: [Player j] -> IO [Player j]
prepareGame option = do 
    myClearScreen
    putStrLn "What is your name: "
    player1 <- getLine
    let playerIO1 = loadPlayer player1
    playerl1 <- playerIO1
    let playerT1exists = validPlayer option player1
    
    if (playerl1 /= nullPlayer) then do
        if not(playerT1exists) then do
            file <- openFile "option.txt" WriteMode
            let newPlayer = newPlayerData player1 0
            hPutStrLn file (show (newPlayer:option))
            hClose file
            prepareGamePart2 (newPlayer:option) player1
        else do
            prepareGamePart2 option player1
    else do
        putStrLn ("\n"++ (show player1) ++ " doesn't exist!")
        pause
        menu option

prepareGamePart2 option player1 = do
    myClearScreen
    putStrLn "And the name of your rival: "
    player2 <- getLine
    let playerIO2 = loadPlayer player2
    playerl2 <- playerIO2
    let playerT2exists = validPlayer option player2

    if (playerl2 /= nullPlayer) then do
        if not(playerT2exists) then do
            file <- openFile "option.txt" WriteMode
            let newPlayer = newPlayerData player2 0
            hPutStrLn file (show (newPlayer:option))
            hClose file
            newGame (newPlayer:option) player1 player2
        else do
            newGame option player1 player2
    else do
        putStrLn( "\n"++ (show player2) ++ " doesn't exist!")
        pause
        menu option


newGame :: [Player j] -> Name -> Name -> IO [Player j]
newGame option player1 player2 = do
    -- myClearScreen
    let playerIO1 = loadPlayer player1
    playerl1 <- playerIO1
    let playerIO2 = loadPlayer player2
    playerl2 <- playerIO2
    putStrLn ("\nBegin the game: " ++  (show player1) ++ " vs "++ player2)
    putStrLn ("\n" ++ player1 ++ " will be \'X\' and "++ player2 ++ " will be \'O\'")
    runGame option ['1'..'9'] player1 player2 0 playerl1 playerl2

newTable ::GameTable -> Turn -> Char ->GameTable
newTable (x:xs) turn element 
    |((x == element) && (turn  == 0)) = (['x'] ++ xs)
    |((x == element) && (turn  == 1)) = (['o'] ++ xs)
    | otherwise =  x:(newTable xs turn element)

runGame ::[Player j] -> GameTable -> Name ->Name -> Turn -> Player j -> Player j -> IO [Player j]
runGame option  gtable player1 player2 turn playerl1 playerl2 = do
    -- myClearScreen
    putStrLn ("\n" ++ "                              " ++
        (show (gtable !! 0)) ++ " | " ++ (show (gtable !! 1)) ++ " | " ++ (show (gtable !! 2)) ++
        "\n                              ---------------\n" ++ "                              " ++
        (show (gtable !! 3)) ++ " | " ++ (show (gtable !! 4)) ++ " | " ++ (show (gtable !! 5)) ++
        "\n                              ---------------\n" ++ "                              " ++
        (show (gtable !! 6)) ++ " | " ++ (show (gtable !! 7)) ++ " | " ++ (show (gtable !! 8)) ++"\n")
    -- System.IO.getChar
    -- menu option
    let wonponits = 2
    if (winPlayer1 gtable) then do 
        putStrLn ("Congratulations " ++ (show player1) ++ "! You win "++show(wonponits)++" points!!\nand your rival loses "++show(wonponits)++" points")
        file <- openFile "option.txt" WriteMode
        hPutStrLn file (show (refreshScore option player1))
        hClose file
        file <- openFile "option.txt" ReadMode
        refreshedData <- hGetLine file
        hClose file
        putStrLn "\nPress enter to return to menu..."
        aux <- readChar
        savePlayerToFile (addScorePlayer wonponits playerl1)
        savePlayerToFile (addScorePlayer (-wonponits) playerl2)
        menu (read refreshedData)
    else do
        if (winPlayer2 gtable) then do
            savePlayerToFile (addScorePlayer (-wonponits) playerl1)
            savePlayerToFile (addScorePlayer wonponits playerl2)
            putStrLn ("Congratulations " ++ (show player2) ++ "! You win "++show(wonponits)++" points!!\nand your rival loses "++show(wonponits)++" points")
            file <- openFile "option.txt" WriteMode
            hPutStrLn file (show (refreshScore option player2))
            hClose file
            file <- openFile "option.txt" ReadMode
            refreshedData <- hGetLine file
            hClose file
            putStrLn "\nPress enter to return to menu..."
            aux <- readChar
            menu (read refreshedData)
        else do 
            if ((length (intersect "123456789" gtable)) == 0 ) then do 
                putStrLn "Draw! Both players lose"
                putStrLn "Press enter to return to menu"
                aux <- readChar
                menu option
            else do 
                if (turn == 0 ) then do 
                    putStrLn ((show player1) ++ ", your turn ")
                    op <- readChar
                    if not (elem op ['1'..'9']) then do 
                        putStrLn "Option invalid! Try again "
                        runGame option (newTable gtable turn op ) player1 player2 0 playerl1 playerl2
                    else 
                        if not (elem op gtable) then do 
                            putStrLn "Option already choosed! "
                            runGame option (newTable gtable turn op ) player1 player2 0 playerl1 playerl2
                        else    
                            runGame option (newTable gtable turn op ) player1 player2 1 playerl1 playerl2
                else do
                    putStrLn ((show player2) ++ ", your turn ")
                    op <- readChar
                    if not (elem op ['1'..'9']) then do 
                        putStrLn "Option invalid! Try again "
                        runGame option (newTable gtable turn op ) player1 player2 1 playerl1 playerl2
                    else 
                        if not (elem op gtable) then do 
                            putStrLn "Option already choosed! "
                            runGame option (newTable gtable turn op ) player1 player2 1 playerl1 playerl2
                        else    
                            runGame option (newTable gtable turn op ) player1 player2 0 playerl1 playerl2

winPlayer1 :: GameTable -> Bool
winPlayer1 table 
    | (((table !! 0) == 'x') && ((table !! 1) == 'x') && ((table !! 2) == 'x')) = True
    | (((table !! 3) == 'x') && ((table !! 4) == 'x') && ((table !! 5) == 'x')) = True
    | (((table !! 6) == 'x') && ((table !! 7) == 'x') && ((table !! 8) == 'x')) = True
    
    | (((table !! 0) == 'x') && ((table !! 3) == 'x') && ((table !! 6) == 'x')) = True
    | (((table !! 1) == 'x') && ((table !! 4) == 'x') && ((table !! 7) == 'x')) = True
    | (((table !! 2) == 'x') && ((table !! 5) == 'x') && ((table !! 8) == 'x')) = True

    | (((table !! 0) == 'x') && ((table !! 4) == 'x') && ((table !! 8) == 'x')) = True
    | (((table !! 2) == 'x') && ((table !! 4) == 'x') && ((table !! 6) == 'x')) = True
    | otherwise = False

winPlayer2 :: GameTable -> Bool
winPlayer2 table   
    | (((table !! 0) == 'o') && ((table !! 1) == 'o') && ((table !! 2) == 'o')) = True
    | (((table !! 3) == 'o') && ((table !! 4) == 'o') && ((table !! 5) == 'o')) = True
    | (((table !! 6) == 'o') && ((table !! 7) == 'o') && ((table !! 8) == 'o')) = True

    | (((table !! 0) == 'o') && ((table !! 3) == 'o') && ((table !! 6) == 'o')) = True
    | (((table !! 1) == 'o') && ((table !! 4) == 'o') && ((table !! 7) == 'o')) = True
    | (((table !! 2) == 'o') && ((table !! 5) == 'o') && ((table !! 8) == 'o')) = True

    | (((table !! 0) == 'o') && ((table !! 4) == 'o') && ((table !! 8) == 'o')) = True
    | (((table !! 2) == 'o') && ((table !! 4) == 'o') && ((table !! 6) == 'o')) = True
    | otherwise = False

refreshScore :: [Player j] -> String -> [Player j]
refreshScore players winner = addScorePlayerOnList 1 (getPlayerByName winner players) players
    -- | (name == winner) = [(Player name (score + 1))]++xs
    -- | otherwise = (Player name score):(refreshScore xs winner)
    -- where
    --     name = getName headPlayer
    --     score = getScore headPlayer


showRanking :: [Player j] -> IO()
showRanking [] = return ()
showRanking (x:xs) = do
    putStrLn((getName x) ++ " win "++(show (getScore x)) ++ " times" )
    showRanking xs
    
-- getName :: Player -> Name
-- getName (Player name _) = name

-- getScore :: Player -> Score
-- getScore (Player _ score ) = score

order :: [Player j] -> [Player j]
order option = sortBy ( compare `on` getScore) option
