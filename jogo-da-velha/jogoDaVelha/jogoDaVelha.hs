import Control.Exception
import System.Process
import System.IO.Error
import System.IO
import Data.List --intersect
type Players = [Player]
type Turn = Int
type Name = String
type Score = Int
type GameTable = [Char]

data Player = Player Name Score deriving (Show, Eq, Read)




startHashGame :: IO() 
startHashGame = do
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


menu :: Players -> IO Players
menu option = do
                system "clear || cls"
                putStrLn "::::::::::::::::::::::::::::::::::::::: Hash Game :::::::::::::::::::::::::::::::::::::::"
                putStrLn "\n1 - REGISTER A PLAYER"
                putStrLn "2 - PLAY"
                putStrLn "3 - RANKING VIEW"
                putStrLn "0 - EXIT"
                putStr "OPTION: "
                op <- getChar
                getChar
                runOption option op

runOption :: Players -> Char -> IO Players
runOption option '1' = registerPlayer option
runOption option '2' = prepareGame option
runOption option '0' = do
    putStrLn("\nThanks for playing!\n")
    return option
    --return to selection game

runOption option _ = do
    putStrLn("\nInvalid option! Try again!")
    putStr ("\n Press enter to continue....")
    getChar
    menu option


registerPlayer :: Players -> IO Players
registerPlayer option = do
    putStr("\nEnter a user name: ")
    name <- getLine
    if(validPlayer option name) then do
        putStrLn ("This player "++ (show name) ++" already exists, try another...")
        putStr ("\n Press enter to continue....")
        getChar
        menu option
    else do
        file <- openFile "option.txt" WriteMode
        hPutStrLn file (show ((Player name 0): option))
        hClose file
        putStrLn("Player " ++ name ++ " successfully registered!!")
        putStr("\n Press enter to continue...")
        getChar
        menu ((Player name 0):option)

validPlayer :: Players -> Name -> Bool
validPlayer [] _ = False
validPlayer ((Player n p):xs) name --s:xs
                |(n == name) = True
                |otherwise = validPlayer xs name

prepareGame :: Players -> IO Players
prepareGame option = do 
        putStr "What is your name: "
        player1 <- getLine
        if not (validPlayer option player1) then do 
            putStrLn ("\n"++ (show player1) ++ " not exists!")
            putStr "\nPress enter to continue"
            getChar
            menu option
        else do
            putStr "And the name of your rival: "
            player2 <- getLine
            if not (validPlayer option player2) then do
                putStrLn( "\n"++ (show player2) ++ " not exists!")
                putStr "\nPress enter to continue"
                getChar
                menu option
            else do
                newGame option player1 player2

newGame :: Players -> Name -> Name -> IO Players
newGame option player1 player2 = do
    putStrLn ("\nBegin the game: " ++  (show player1) ++ " vs "++ player2)
    putStrLn ("\n" ++ player1 ++ " will be \'X\' and "++ player2 ++ "will be \'O\'")
    runGame option ['1'..'9'] player1 player2 0

newTable ::GameTable -> Turn -> Char ->GameTable
newTable (x:xs) turn element 
    |((x == element) && (turn  == 0)) = (['X'] ++ xs)
    |((x == element) && (turn  == 1)) = (['0'] ++ xs)
    | otherwise =  x:(newTable xs turn element)

runGame ::Players -> GameTable -> Name ->Name -> Turn -> IO Players
runGame option  gtable player1 player2 turn = do
    putStrLn ("\n" ++ "                              " ++
        (show (gtable !! 0)) ++ " | " ++ (show (gtable !! 1)) ++ " | " ++ (show (gtable !! 2)) ++
        "\n                              ---------------\n" ++ "                              " ++
        (show (gtable !! 3)) ++ " | " ++ (show (gtable !! 4)) ++ " | " ++ (show (gtable !! 5)) ++
        "\n                              ---------------\n" ++ "                              " ++
        (show (gtable !! 6)) ++ " | " ++ (show (gtable !! 7)) ++ " | " ++ (show (gtable !! 8)) ++"\n")
    -- getChar
    -- menu option
    if (winPlayer1 gtable) then do 
        putStrLn ("Congratulations " ++ (show player1) ++ "! You win!!")
        arq <- openFile "dados.txt" WriteMode
        hPutStrLn arq (show (refreshScore dados player1))
        hClose arq
        arq <- openFile "dados.txt" ReadMode
        refreshedData <- hGetLine arq
        hClose arq
        putStr "\nPress enter to return to menu..."
        getChar
        menu (read refreshedData)
    else do
        if (winPlayer2 gtable) then do
            putStrLn ("Congratulations " ++ (show player2) ++ "! You win!!")
            arq <- openFile "dados.txt" WriteMode
            hPutStrLn arq (show (refreshScore dados player2))
            hClose arq
            arq <- openFile "dados.txt" ReadMode
            refreshedData <- hGetLine arq
            hClose arq
            putStr "\nPress enter to return to menu..."
            getChar
            menu (read refreshedData)
        else do 
            if ((length (intersect "123456789" gtable)) == 0 ) then do 
                putStrLn "Fail! Both players loses"
                putStrLn "Press enter to return to menu"
                getChar
                menu option
            else do 
                if (turn == 0 ) then do 
                    putStr ((show player1) ++ ", your turn")
                    op <- getChar
                    getChar
                    if not (elem op ['1'..'9']) then do 
                        putStrLn "Option invalid! Try again"
                        runGame option player1 player2 0
                    else 
                        if not (elem op gtable) then do 
                            putStrln "Option already choosed!"
                            runGame option player1 player2 0
                        else    
                            runGame option (newTable gtable turn op ) player1 player2 1 
                else do
                    putStr ((show player2) ++ ", your turn")
                    op <- getChar
                    getChar
                    if not (elem op ['1'..'9']) then do 
                        putStrLn "Option invalid! Try again"
                        runGame option player1 player2 1
                    else 
                        if not (elem op gtable) then do 
                            putStrln "Option already choosed!"
                            runGame option player1 player2 1
                        else    
                            runGame option (newTable gtable turn op ) player1 player2 0


winPlayer1 :: GameTable -> Bool
winPlayer1 table 
  
	| (((table !! 0) == 'X') && ((table !! 1) == 'X') && ((table !! 2) == 'X')) = True
	| (((table !! 3) == 'X') && ((table !! 4) == 'X') && ((table !! 5) == 'X')) = True
	| (((table !! 6) == 'X') && ((table !! 7) == 'X') && ((table !! 8) == 'X')) = True
	
	| (((table !! 0) == 'X') && ((table !! 3) == 'X') && ((table !! 6) == 'X')) = True
	| (((table !! 1) == 'X') && ((table !! 4) == 'X') && ((table !! 7) == 'X')) = True
	| (((table !! 2) == 'X') && ((table !! 5) == 'X') && ((table !! 8) == 'X')) = True

	| (((table !! 0) == 'X') && ((table !! 4) == 'X') && ((table !! 8) == 'X')) = True
	| (((table !! 2) == 'X') && ((table !! 4) == 'X') && ((table !! 6) == 'X')) = True
	| otherwise = False

winPlayer2 :: GameTable -> Bool
winPlayer2 table   
    | (((table !! 0) == 'X') && ((table !! 1) == 'X') && ((table !! 2) == 'X')) = True
    | (((table !! 3) == 'X') && ((table !! 4) == 'X') && ((table !! 5) == 'X')) = True
    | (((table !! 6) == 'X') && ((table !! 7) == 'X') && ((table !! 8) == 'X')) = True

    | (((table !! 0) == 'X') && ((table !! 3) == 'X') && ((table !! 6) == 'X')) = True
    | (((table !! 1) == 'X') && ((table !! 4) == 'X') && ((table !! 7) == 'X')) = True
    | (((table !! 2) == 'X') && ((table !! 5) == 'X') && ((table !! 8) == 'X')) = True

    | (((table !! 0) == 'X') && ((table !! 4) == 'X') && ((table !! 8) == 'X')) = True
    | (((table !! 2) == 'X') && ((table !! 4) == 'X') && ((table !! 6) == 'X')) = True
    | otherwise = False

refreshScore :: Players -> String-> Players
refreshScore ((Player name  score ):xs) winner
    | (name == winner) = [(Player name (score + 1))]++xs
    | otherwise = (Player name score):(refreshScore xs winner)


showRanking :: Players -> IO()
showRanking [] = return []
showRanking (x:xs) = do
    putStrLn((getName x) ++ " win "++(show (getScore x)) ++ " times" )
    showRanking xs
    
getName :: Player -> Name
getName (Player name _) = name

getScore :: Player -> Score
getChar (Player _ score ) = score

order :: Players -> Players
order option = sortBy ( compare `on` getScore) option