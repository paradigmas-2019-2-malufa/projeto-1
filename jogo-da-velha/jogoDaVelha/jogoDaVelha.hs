{-
Jogo da velha versão 1.0
-}

import Control.Exception
import System.Process
import System.IO.Error
import System.IO

--definitions
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
                system "cls || clear"
                putStrLn "::::::::::::::::::::::::::::::::::::::: Hash Game :::::::::::::::::::::::::::::::::::::::"
                putStrLn "\n1 - PLAY"
                putStrLn "2 - RANKING"
                putStrLn "0 - EXIT"
                putStr "OPTION: "
                op <- getChar
                getChar
                runOption option op

runOption :: Players -> Char -> IO Players
runOption option '0' = do
    menu option
runOption option _ = do
    putStrLn("Invalid option! Try again...")
    getChar
    menu option