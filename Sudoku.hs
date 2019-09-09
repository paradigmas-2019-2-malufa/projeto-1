module Sudoku
(grid
,invalid
,buildGrid
,checkIfWon
,getValueFrom
,readNumber
,getRow
,getColumn
,getBlock
,printGrid
,gameLoop
,startSudoku
) where

import System.IO
import Data.List
import Common
import PlayerModule

grid :: [Int]
grid = [5, 3, 0,  0, 7, 0,  0, 0, 0,
        6, 0, 0,  1, 9, 5,  0, 0, 0,
        0, 9, 8,  0, 0, 0,  0, 6, 0,

        8, 0, 0,  0, 6, 0,  0, 0, 3,
        4, 0, 0,  8, 0, 3,  0, 0, 1,
        7, 0, 0,  0, 2, 0,  0, 0, 6,

        0, 6, 0,  0, 0, 0,  2, 8, 0,
        0, 0, 0,  4, 1, 9,  0, 0, 5,
        0, 0, 0,  0, 8, 0,  0, 7, 9]

invalid :: Int -> Int -> Int -> [Int] -> Bool
invalid column row num oldGrid
    | num == -1 = False
    | not(num `elem` [0..9]) = True
    | not(column `elem` [0..8])   = True
    | not(row `elem` [0..8])   = True
    | (getValueFrom grid column row) /= 0 = True
    | num == 0 = False
    | num `elem` getBlock column row oldGrid   = True
    | num `elem` getRow row grid 0 = True
    | num `elem` getColumn column grid 0 = True
    | otherwise              = False

buildGrid :: Int -> Int -> Int -> [Int] -> [Int]
buildGrid column row num oldGrid =  if invalid column row num oldGrid then oldGrid
                              else if num == -1 then grid
                                   else take (column + 9*row) oldGrid ++ [num] ++ drop (column + 9*row + 1) oldGrid

-- verifica de ja preencheu todo o grid
checkIfWon :: [Int] -> Bool
checkIfWon [] = True
checkIfWon newGrid = if (take 1 newGrid) == [0] then False else checkIfWon (drop 1 newGrid)

-- Pega um valor de um grid da coord column row
getValueFrom :: [Int] -> Int -> Int -> Int
getValueFrom grid column row = (grid) !! (column + 9*row)

readNumber::IO Int
readNumber = do
    num <- getLine
    return (read num)

getRow :: Int -> [Int] -> Int -> [Int]
getRow _ _ 9 = []
getRow n grid column = [grid !! (column + 9*n)] ++ getRow n grid (column+1)

getColumn :: Int -> [Int] -> Int -> [Int]
getColumn _ _ 9 = []
getColumn n grid column = [grid !! (9*column + n)] ++ getColumn n grid (column+1)

-- pega um bloco em forma de vetor em que o valor da coord column row está localizada
getBlock :: Int -> Int -> [Int] -> [Int]
getBlock column row grid =    (take 3 (getRow l grid c)) ++ 
                        (take 3 (getRow (l+1) grid c)) ++ 
                        (take 3 (getRow (l+2) grid c))
                        where 
                            l = (quot row 3)*3
                            c = (quot column 3)*3

printGrid :: [Int] -> IO ()
printGrid oldGrid = do
              putStrLn "    1 2 3   4 5 6   7 8 9  "
              putStrLn verticalLine
              myPrint oldGrid 0 1
            where myPrint [] _ _  = do
                                   putStrLn verticalLine
                  myPrint oldGrid 3 row = do
                                   putStrLn verticalLine
                                   myPrint oldGrid 0 row
                  myPrint oldGrid n row = do
                                   putStrLn (linha (take 9 oldGrid) (show(row)++" |") 0)
                                   myPrint (drop 9 oldGrid) (n + 1) (row+1)

                  linha [] str _     = str ++ " |"
                  linha a  str 3     = linha a (str ++ " |") 0
                  linha (a:as) str n = linha as (str ++ " " ++ (show a)) (n + 1)

                  verticalLine = "   "++(replicate 23 '-')

gameLoop :: [Int] -> Player j -> IO Char
gameLoop oldGrid player = do
    myClearScreen
    printGrid oldGrid
    putStrLn "type the row or some option: "
    row <- readNumber
    putStrLn "type the column or some option: "
    column <- readNumber
    putStrLn "\n Options: \n   -1 to new game;\n   -2 to main menu;\n"
    putStrLn "type a number [1..9], 0 to erase or some option: "
    num <- readNumber
    if num /= -2 then do
        let newGrid = buildGrid (column-1) (row-1) num oldGrid
        if (oldGrid == newGrid) then do
            putStrLn "Invalid value, out of range or repeated\n"
            pause
            gameLoop newGrid player
        else do 
            if checkIfWon newGrid then do 
                savePlayerToFile (addScorePlayer 3 player)
                putStrLn "\n\nCongratulations! You win! you got 3 points"
                printGrid newGrid
                pause
                return 'a'
            else do
                gameLoop newGrid player
    else do
        return 'a'


startSudoku twoPlayers = do
    myClearScreen
    player <- loadPlayer (takeName1 twoPlayers)
    putStrLn "                   <==--[ SuDoKu ]--==>\n"
    putStrLn "  RULES:\n"
    putStrLn "* You win when you fill all the gaps with numbers from 1 to 9"
    putStrLn "* The number can't be repeated in the row, column or block\n"
    pause
    gameLoop grid player
    return ()
