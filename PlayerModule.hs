module PlayerModule (
    Player,
    Name,
    Score,
    newPlayerData,
    nullPlayer,
    takePlayer1,
    takePlayer2,
    playersFile,
    showPlayer,
    showPlayers,
    sortPlayersByScore,
    sortPlayersByName,
    getScore,
    getName,
    getPlayerByName,
    bestFromTwo,
    getHigherScorePlayer,
    playerExists,
    insertPlayer,
    deletePlayer,
    addScorePlayer,
    addScorePlayerOnList,
    splitAttributes,
    parsePlayersList,
    savePlayers,
    loadPlayers,
    loadPlayer,
    savePlayerToFile,
    -- validStringName,
    validName,
    alphabet,
    numbers,
    isLetter,
    strIsAlphaNum,
    -- showError,
    -- newPlayer,
    createAccount,
    login
) where 

import Data.List
import Data.List.Split
import Data.Function
import System.Directory
import System.Exit
import System.IO
import System.IO.Error
import System.Console.ANSI
import System.Process
import Control.Monad
import GHC.IO.Exception
import Control.Exception
import Data.Char
import Common

type Name = String
type Score = Int

-- player data structure
data Player j = Null | Player Name Score
    deriving (Eq, Show, Read)

firstCall = 0
nullPlayer = Null
playersFile = "players.txt"

takePlayer1 (player1, _) = player1
takePlayer2 (_, player2) = player2

newPlayerData :: Name -> Score -> Player j
newPlayerData name score = (Player name score)

-- show player info as String
showPlayer :: Player j -> IO ()
showPlayer Null = do putStrLn "Any player to show!"
showPlayer (Player name score) = do putStrLn ("name: " ++ name ++ ", score: " ++ (show score))

-- show all players info
showPlayers :: [Player j] -> IO ()
showPlayers [] = do
    putStrLn "Any player to show!"
showPlayers [player] = do
    showPlayer player
showPlayers players = do
    showPlayer (head players)
    showPlayers (tail players)

-- return players list sorted by higher score
sortPlayersByScore :: [Player j] -> [Player j]
sortPlayersByScore players = sortBy (\(Player _ a) (Player _ b) -> (compare b a)) players

-- return players list sorted by name
sortPlayersByName :: [Player j] -> [Player j]
sortPlayersByName players = sortBy (\(Player a _) (Player b _) -> compare a b) players

-- get score from player
getScore :: Player j -> Int
getScore Null = -1
getScore (Player _ score) = score

-- get name from player
getName :: Player j -> Name
getName Null = "Error! null player doesn't have name"
getName (Player name _) = name

-- get player by name from a list
getPlayerByName :: Name -> [Player j] -> Player j
getPlayerByName _ [] = Null
getPlayerByName name players = if ((getName headPlayer) == name) then headPlayer
                                    else (getPlayerByName name (tail players))
                                    where headPlayer = head players

-- return higher score player out of two players
bestFromTwo :: Player j -> Player j -> Player j
bestFromTwo playerA playerB = if (scoreA >= scoreB) then playerA else playerB
                                  where scoreA = getScore playerA
                                        scoreB = getScore playerB

-- return player with higher score
getHigherScorePlayer :: [Player j] -> Player j
getHigherScorePlayer [] = Null
getHigherScorePlayer [player] = player
getHigherScorePlayer players = bestFromTwo (head players) (getHigherScorePlayer (tail players))

-- checks if player exists in a list based on name
playerExists :: Player j -> [Player j] -> Bool
playerExists Null _ = False
playerExists _ [] = False
playerExists player players = if (getName player == getName headPlayer)
                                  then True else playerExists player (tail players)
                                  where headPlayer = head players

-- insert player in a list of players
insertPlayer :: Player j -> [Player j] -> [Player j]
insertPlayer player players = [player] ++ players

-- delete player of a list based on name
deletePlayer :: Player j -> [Player j] -> [Player j]
deletePlayer player players = 
    deleteBy (\(Player nameA _) (Player nameB _) -> nameA==nameB) player players

-- update a player in a list of players
updatePlayerOnList :: Player j -> [Player j] -> [Player j]
updatePlayerOnList player players = insertPlayer player (deletePlayer player players)

-- add score to a player
-- to subtract score, put number between parentheses: 
--       ex: addScorePlayer (-3) player
addScorePlayer :: Score -> Player j -> Player j
addScorePlayer _ Null = Null
addScorePlayer score (Player name playerScore) = 
    if newScore<0 then (Player name 0)
    else (Player name newScore)
    where newScore = playerScore+score

-- add score to a player from a list of players
-- to subtract score, put number between parentheses: 
--       ex: addScorePlayer (-3) player
addScorePlayerOnList :: Score -> Player j -> [Player j] -> [Player j]
addScorePlayerOnList _ Null players = players
addScorePlayerOnList score player [] = [addScorePlayer score player]
addScorePlayerOnList score player players = 
    updatePlayerOnList playerAddedPoints players
    where
        playerAddedPoints = addScorePlayer score player

-- takes a string from a list, like "jhon,3" and splits and
-- insert in another list as a player
-- ex: (Player "jhon" 3)
splitAttributes :: [String] -> [Player j]
splitAttributes [] = []
splitAttributes players = 
    [(Player name score)] ++ splitAttributes (tail players)
    where 
        player = splitOn "," (head players)
        name = player !! 0
        score = read (player !! 1) :: Int

-- takes a list of players pass to a list of strings
-- ex [(Player "jhon" 3)] ==> ["jhon,3"]
-- parsePlayersList :: (Show t) => [Player j] -> [String]
parsePlayersList :: [Player j] -> [String]
parsePlayersList [] = []
parsePlayersList players = 
    [name ++ "," ++ (show score)] ++ parsePlayersList (tail players)
    where
        player = head players
        name = getName player
        score = getScore player

-- save players to file
savePlayers players = do
    let filePlayersName = playersFile
    handle <- openFile filePlayersName WriteMode
    hPutStr handle $ unlines (parsePlayersList players)
    hClose handle

-- load players from file and return in a list
loadPlayers = do
    {catch (readFile) treat_error;}
    where
        readFile = do
            handle <- openFile playersFile ReadMode
            contents <- hGetContents handle
            print contents
            let jogadores = splitAttributes (lines contents)
            clearScreen
            hClose handle
            return jogadores

        treat_error err = if isDoesNotExistError err then do
            handle <- openFile playersFile WriteMode;
            hClose handle;
            return [];
        else
            ioError err

loadPlayer playerName = do
    let playersIO = loadPlayers
    players <- playersIO
    let player = getPlayerByName playerName players
    -- let player <- getPlayerByName playerName players
    -- player <- playerIO
    return player

savePlayerToFile player = do
    let playersIO = loadPlayers
    players <- playersIO
    let updatedPlayerList = updatePlayerOnList player players
    savePlayers updatedPlayerList

alphabet = "abcdefghijklmnopqrstuvwxyz"
numbers = "1234567890"
-- isLetter x = x `elem` alphabet

strIsAlphaNum :: String -> Bool
strIsAlphaNum "" = True
strIsAlphaNum name = if ((isAlphaNum letter) == False)
                  then False 
                  else strIsAlphaNum rest
                  where letter = head name
                        rest = tail name

validName :: String -> [Player j] -> Bool
validName name players = (
    ((length name)>1) && 
    (isLetter (head name)) && 
    (strIsAlphaNum name) && 
    ((playerExists (getPlayerByName name players) players)==False))

createAccount = do
    let playersIO = loadPlayers
    players <- playersIO
    putStr "Notice that the characters aren't showing but it's been read\n"
    putStr "\nType your new account name: \n"
    nameupper <- getLine
    let name = lowerString nameupper
    myClearScreen
    putStrLn ("Account name typed: " ++ name)
    let nameIsValid = validName name players
    if nameIsValid
    then
        do
            putStrLn "\nAre you sure you want this name? (y/n)"
            option <- readChar
            if (toLower option == 'y')
            then do
                savePlayers (insertPlayer (Player name 0) players)
            else do
                createAccount
    else
        do
            putStrLn "Invalid name or name already registered!!!"
            putStrLn "  name must start with letter and contain only letters and numbers"
            putStrLn "press ENTER to try again or 'e' to go to menu"
            option <- readChar
            if (toLower option == 'e')
            then do
                putStrLn "going to menu..."
            else do
                createAccount

login = do
    let playersIO = loadPlayers
    players <- playersIO
    myClearScreen
    if players == [] then do
        myClearScreen
        putStr "There aren't players registered\nLet's create an account: \n"
        pause
        createAccount
        newLogin <- login
        return newLogin
    else do
        putStrLn "Type your name acount: "
        nameupper <- getLine
        let name = lowerString nameupper
        let player = (Player name 0)
        if (playerExists player players) then do
            return name
        else do
            putStrLn ("This account name [" ++ name ++ "] doesn't exist")
            putStrLn "do you want to register this account? (y/n)"
            option <- readChar
            if (lowerChar option)=='y' then do
                savePlayerToFile player
                return name
            else do
                newLogin <- login
                return newLogin
