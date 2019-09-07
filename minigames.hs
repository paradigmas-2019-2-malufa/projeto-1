import System.IO
import System.Exit
import Control.Monad
import Common
import PlayerOptions (playerOptions)
-- import GuessTheNumber (startGuessTheNumber)

printMenu :: IO ()
printMenu = do
    myClearScreen
    putStr "  __  __ _____ _   _ _____  _____          __  __ ______  _____ \n"
    putStr " |  \\/  |_   _| \\ | |_   _|/ ____|   /\\   |  \\/  |  ____|/ ____|\n"
    putStr " | \\  / | | | |  \\| | | | | |  __   /  \\  | \\  / | |__  | (___  \n"
    putStr " | |\\/| | | | | . ` | | | | | |_ | / /\\ \\ | |\\/| |  __|  \\___ \\ \n"
    putStr " | |  | |_| |_| |\\  |_| |_| |__| |/ ____ \\| |  | | |____ ____) |\n"
    putStr " |_|  |_|_____|_| \\_|_____|\\_____/_/    \\_\\_|  |_|______|_____/ \n"
    putStr "                   __________________________\n"
    putStr "                  |OFF   ON                  |\n"
    putStr "                  | .----------------------. |\n"
    putStr "                  | |  << Choose a game >> | |\n"
    putStr "                  | |1: Tic Tac Toe        | |\n"
    putStr "                  | |2: Rock paper scissors| |\n"
    putStr "                  | |3: Hangman game       | |\n"
    putStr "                  | |4: Guess the number   | |\n"
    putStr "                  | |                      | |\n"
    putStr "                  | |p: Players options    | |\n"
    putStr "                  | |______________________/ |\n"
    putStr "                  |          _________       |\n"
    putStr "                  |    .    (Minigames)      |\n"
    putStr "                  |  _| |_   \"\"\"\"\"\"\"\"\"  .-.  |\n"
    putStr "                  |-[_   _]-       .-. ( A ) |\n"
    putStr "                  |   |_|         ( B ) '-'  |\n"
    putStr "                  |    '           '-'       |\n"
    putStr "                  |          ___   ___       |\n"
    putStr "                  |         (___) (___)  ,., .\n"
    putStr "                  |                     ;:;:/\n"
    putStr "                  '-----------------------´\n"
    putStr "    Press game key number ('q' to quit): \n"

menuAction :: Char -> IO ()
menuAction '1' = putStrLn "\ngame 1 not done yet..."
menuAction '2' = putStrLn "\ngame 2 not done yet..."
menuAction '3' = putStrLn "\ngame 3 not done yet..."
menuAction '4' = putStrLn "\ngame 4 not done yet..."-- startGuessTheNumber
menuAction 'p' = playerOptions
menuAction 'q' = do
    putStrLn "\nQuitting game..."
    exitSuccess
menuAction _ = putStrLn "\nInvalid option"

main :: IO ()
main = do
    forever (printMenu >> readChar >>= menuAction)
