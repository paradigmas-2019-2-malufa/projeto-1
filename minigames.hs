import System.IO
import System.Exit
import Control.Monad
import Common
import PlayerOptions (playerOptions)
import GuessTheNumber (startGuessTheNumber)
import RockPaperScissors (startRockPaperScissors)
import TicTacToe (startTicTacToe)
import Sudoku (startSudoku)
import PlayerModule

-- Text ascii art generated with patorjk web site (http://patorjk.com/software/taag-v1/). font: big
-- Videogame ascii art by Joan Stark, adapted from (https://www.asciiart.eu/computers/game-consoles)

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
    putStr "                  | |3: Guess the number   | |\n"
    putStr "                  | |4: Sudoku             | |\n"
    putStr "                  | |                      | |\n"
    putStr "                  | |p: Players options    | |\n"
    putStr "                  | |______________________/ |\n"
    putStr "                  |          _________       |\n"
    putStr "                  |    .    (Minigames)      |\n"
    putStr "                  |  _| |_   \"\"\"\"\"\"\"\"\"  .-.  |\n"
    putStr "                  |-[_   _]-       .-. ( A ) |\n"
    putStr "                  |   |_|         ( B ) '-'  |\n"
    putStr "                  |    '           '-'       | !! when you're typing\n"
    putStr "                  |          ___   ___       |    the characters won't show\n"
    putStr "                  |         (___) (___)  ,., .    on terminal, but they're\n"
    putStr "                  |                     ;:;:/     been recorded !!\n"
    putStr "                  '-----------------------´\n"
    putStr "    Press game key number ('q' to quit): \n"

-- menuAction :: Char -> Player j -> Player j -> (Player j, Player j) IO ()
menuAction '1' twoPlayers = do startTicTacToe
menuAction '2' twoPlayers = do startRockPaperScissors twoPlayers
menuAction '3' twoPlayers = do startGuessTheNumber twoPlayers
menuAction '4' twoPlayers = do startSudoku twoPlayers
menuAction 'p' twoPlayers = do playerOptions twoPlayers
menuAction 'q' twoPlayers = do
    putStrLn "\nQuitting game..."
    exitSuccess
menuAction _ twoPlayers = do
    putStrLn "\nInvalid option"

-- runMinigames :: (String t, String t) -> IO ()
runMinigames twoPlayers = do
    -- userName <- login
    printMenu
    choice <- readChar
    menuAction choice twoPlayers
    runMinigames twoPlayers
    -- let player1 = p1
    -- let player2 = p2

main :: IO ()
main = do
    userName <- login
    runMinigames (userName, "")
