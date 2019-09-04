module PlayerModule (
    Name,
    Score,
    Player,
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
    savePlayers
) where 

import Data.List
import Data.List.Split
import System.Directory
import System.Exit
import Control.Monad
import System.IO

type Name = String
type Score = Int

-- player data structure
data Player j = Null | Player Name Score
    deriving (Eq, Show)

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
sortPlayersByScore players = sortBy (\(Player a _) (Player b _) -> compare b a) players

-- return players list sorted by name
sortPlayersByName :: [Player j] -> [Player j]
sortPlayersByName players = sortBy (\(Player _ a) (Player _ b) -> compare a b) players

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
    deleteBy (\(Player nomeA _) (Player nomeB _) -> nomeA==nomeB) player players

-- add score to a player
-- to subtract score, put number between parentheses: 
--       ex: addScorePlayer (-3) player
addScorePlayer :: Score -> Player j -> Player j
addScorePlayer _ Null = Null
addScorePlayer score (Player name playerScore) = 
    (Player name (playerScore+score))

-- add score to a player from a list of players
-- to subtract score, put number between parentheses: 
--       ex: addScorePlayer (-3) player
addScorePlayerOnList :: Score -> Player j -> [Player j] -> [Player j]
addScorePlayerOnList _ Null _ = []
addScorePlayerOnList score player [] = [addScorePlayer score player]
addScorePlayerOnList score player players = 
    insertPlayer addedPoints listWithoutPlayer
    where
        listWithoutPlayer = deletePlayer player players
        addedPoints = addScorePlayer score player

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
savePlayers players handle tempHandle filePlayersName tempName = do
    hPutStr tempHandle $ unlines (parsePlayersList players)
    hClose handle
    hClose tempHandle
    removeFile filePlayersName
    renameFile tempName filePlayersName

