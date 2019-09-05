import Control.Exception
import System.Process
import System.IO.Error
import System.IO

type Players = [Player]
type Turn = Int
type Name = String
type Score = Int
type GameTable = [Char]

data Player = Player Name Score
    deriving (Show, Eq, Read)




my_main :: IO() 
my_main = do
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

runGame ::Players -> GameTable -> Name ->Name -> Turn -> IO Players
runGame option  gtable player1 player2 turn = do
    putStrLn ("\n" ++ "                              " ++
        (show (gtable !! 0)) ++ " | " ++ (show (gtable !! 1)) ++ " | " ++ (show (gtable !! 2)) ++
        "\n                              ---------------\n" ++ "                              " ++
        (show (gtable !! 3)) ++ " | " ++ (show (gtable !! 4)) ++ " | " ++ (show (gtable !! 5)) ++
        "\n                              ---------------\n" ++ "                              " ++
        (show (gtable !! 6)) ++ " | " ++ (show (gtable !! 7)) ++ " | " ++ (show (gtable !! 8)) ++
        "\n")
    getChar
    menu option
    -- if (player1 gtable) then do 
    --     putStrLn ("Congratulations " ++ player1 ++ "! You win!!")
    --     arq <- openFile "dados.txt" WriteMode
    --     hPutStrLn arq (show (refreshScore dados player1))
    --     hClose arq
    --     arq <- openFile "dados.txt" ReadMode
    --     dados_atualizados <- hGetLine arq
    --     hClose arq
    --     putStr "\nPress enter to return to the menu..."
    --     getChar
    --     menu (read dados_atualizados)