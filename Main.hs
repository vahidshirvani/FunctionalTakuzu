-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module Main where

import TicTacLogic
import Data.Char (isSpace)
    
------------------------------------------------------------------------
-- Producing Output
------------------------------------------------------------------------

-- Source: http://en.wikipedia.org/wiki/Trim_(programming)#Haskell
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

convertRow :: Row -> String
convertRow row =
    let helper [] acc = acc
        helper (0:xs) acc = helper xs ('O':acc)
        helper (1:xs) acc = helper xs ('X':acc)
        helper ((-1):xs) acc = helper xs ('.':acc)
    in helper row []

convertBoard :: Board -> String
convertBoard board = 
    let helper [] acc = reverse acc
        helper (r:rs) acc = helper rs (convertRow r ++ "\n" ++ acc)
    in helper board []

solver :: Board -> IO ()
solver board = do
    let solved = solve board
    let converted = trim (convertBoard solved)
    putStrLn converted


------------------------------------------------------------------------
-- Reading Input
------------------------------------------------------------------------

charToIntList :: String -> [Int] -> [Int]
charToIntList [] list = reverse list
charToIntList ('X':xs) list = charToIntList xs (1:list)
charToIntList ('1':xs) list = charToIntList xs (1:list)
charToIntList ('O':xs) list = charToIntList xs (0:list)
charToIntList ('0':xs) list = charToIntList xs (0:list)
charToIntList ('.':xs) list = charToIntList xs (-1:list)

printError :: String -> IO Board
printError msg = do
    putStrLn msg
    return [[]]

readLines :: Char -> Int -> Int -> Board -> IO Board
readLines gameChar _ 0 board = return (reverse board)
readLines gameChar gameSize rowsLeft board = do
    row <- getLine  -- X
    let rowList = charToIntList row []
    if length rowList == gameSize
        then readLines gameChar gameSize (rowsLeft - 1) (rowList:board)
        else printError "Invalid row length"

gameType :: String -> IO Board
gameType ('T':xs) = do
    let pairs = map (\x -> read x :: Int) (words xs)
    if (head pairs == (pairs !! 1)) && (mod (head pairs) 2 == 0)
        then readLines 'T' (head pairs) (head pairs) []
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

