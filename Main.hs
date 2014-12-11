module Main where

import TicTacLogic
    
------------------------------------------------------------------------
-- Solver
------------------------------------------------------------------------

convertRow :: Row -> [Char]
convertRow row = 
    let helper [] acc = acc
        helper (0:xs) acc = helper xs ('O':acc)
        helper (1:xs) acc = helper xs ('X':acc)
        helper ((-1):xs) acc = helper xs ('.':acc)
    in helper row []

convertBoard :: Board -> [Char]
convertBoard board = 
    let helper [] acc = reverse acc
        helper (r:rs) acc = helper rs ((convertRow r) ++ "\r\n" ++ acc)
    in helper board []

goThroughAllRules :: Board -> [(Board -> Board)] -> Board
goThroughAllRules board [] = board
goThroughAllRules board (rule:rules) =
    let newBoard = runRule board rule
    in 
        if (isComplete newBoard)
        then newBoard
        else goThroughAllRules newBoard rules

solve :: Board -> [(Board -> Board)] -> Board
solve board rules =
    let newBoard = goThroughAllRules board rules
    in 
        if (newBoard == board) -- it's either solved or it isn't solvable
        then newBoard
        else goThroughAllRules newBoard rules

solver :: Board -> IO ()
solver board = do
    let solved = solve board [avoidTripleOne, 
                     avoidTripleTwo, 
                     avoidTripleThree, 
                     completingRowOrColumn, 
                     avoidingRowOrColumnDuplication,
                     advancedTechniqueOne,
                     advancedTechniqueTwo]
    let converted = convertBoard solved
    putStrLn converted

------------------------------------------------------------------------
-- Reading input
------------------------------------------------------------------------

charToIntList :: [Char] -> [Int] -> [Int]
charToIntList [] list = (reverse list)
charToIntList ('X':xs) list = charToIntList xs (1:list)
charToIntList ('O':xs) list = charToIntList xs (0:list)
charToIntList ('.':xs) list = charToIntList xs (-1:list)

printError :: String -> IO Board
printError msg = do
    putStrLn msg
    return [[]]

readLines :: Char -> Int -> Int -> Board -> IO Board
readLines gameChar _ 0 board = return (reverse board)
readLines gameChar gameSize rowsLeft board = do
    row <- getLine  -- X
    let rowList = (charToIntList row [])
    if (length rowList) == gameSize
        then readLines gameChar gameSize (rowsLeft - 1) (rowList:board)
        else printError "Invalid row length"

gameType :: [Char] -> IO Board
gameType ('T':xs) = do
    let pairs = (map (\x -> read x :: Int) (words xs))
    if ((pairs !! 0) == (pairs !! 1)) && ((mod (pairs !! 0) 2) == 0) 
        then (readLines 'T' (pairs !! 0) (pairs !! 0) [])
        else printError "incorrect input values"

gameType ('S':xs) = printError "sudoku"
gameType _ = printError "unknown character"


------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    gameInfo <- getLine
    let board = gameType gameInfo
    board >>= solver -- this operator requires us to return IO

