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
                system "clear"
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
runOption option '0' = do
    putStrLn("\nThanks for playing!\n")
    {-go back to manu gameboy-}
    return option

{-
runOption option '2' = do

runOption option '3' = do
-}

runOption option _ = do
    putStrLn("\nInvalid option! Try again!")
    putStr ("\n Press <Enter> to continue....")
    getChar
    menu option {-go back to hash manu-}


registerPlayer :: Players -> IO Players
registerPlayer option = do
    putStrLn("\nEnter a user name: \n")
    name <- getLine
    if(validPlayer option name) then do
        putStrLn "This player already exists, try another..."
        putStr ("\n Press <Enter> to continue....")
        getChar
        menu option
    else do
        file <- openFile "option.txt" WriteMode
        hPutStrLn file (show ((Player name 0): option))
        hClose file
        putStrLn("Player " ++ name ++ "successfully registered!!")
        putStr("\n Press <Enter> to continue...")
        getChar
        menu ((Player name 0):option)

validPlayer :: Players -> Name -> Bool
validPlayer [] _ = False
validPlayer ((Player n p):xs) name --s:xs
                |(n == name) = True
                |otherwise = validPlayer xs name