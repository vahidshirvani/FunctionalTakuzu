-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module Main where

import AbstractGameLogic (Board,Cell,Row)
import TicTacLogic
import SudokuLogic
import Data.Char (isSpace, digitToInt, intToDigit)
    
------------------------------------------------------------------------
-- Output Calculation for both games
------------------------------------------------------------------------
convertRow :: Char -> Row -> String
convertRow 'T' row =
    let helper [] acc = acc
        helper (0:xs) acc = helper xs ('O':acc)
        helper (1:xs) acc = helper xs ('X':acc)
        helper ((-1):xs) acc = helper xs ('.':acc)
    in helper row []
convertRow 'S' row =
    let helper [] acc = acc
        helper ((-1):xs) acc = helper xs ('.':acc)
        helper (x:xs) acc = helper xs (intToDigit x:acc)
    in helper row []

convertBoard :: Char -> Board -> String
convertBoard gameChar board =
    let helper [] acc = reverse acc
        helper (r:rs) acc = helper rs (convertRow gameChar r ++ "\n" ++ acc)
    in helper board []

-- Source: http://en.wikipedia.org/wiki/Trim_(programming)#Haskell
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

solver :: Char -> Board -> IO ()
solver gameChar board = do
    let solved = if gameChar == 'T'
                 then TicTacLogic.solveTakuzu board
                 else SudokuLogic.solveSudoku board
    let converted = trim (convertBoard gameChar solved)
    putStrLn converted


------------------------------------------------------------------------
-- Input readers for both games
------------------------------------------------------------------------
charToIntList :: Char -> String -> [Int] -> [Int]
charToIntList _ [] list = reverse list
charToIntList 'T' ('X':xs) list = charToIntList 'T' xs (1:list)
charToIntList 'T' ('1':xs) list = charToIntList 'T' xs (1:list)
charToIntList 'T' ('O':xs) list = charToIntList 'T' xs (0:list)
charToIntList 'T' ('0':xs) list = charToIntList 'T' xs (0:list)
charToIntList gameChar ('.':xs) list = charToIntList gameChar xs (-1:list)
charToIntList 'S' (x:xs) list = charToIntList 'S' xs (digitToInt x:list)

printError :: String -> IO Board
printError msg = do
    putStrLn msg
    return [[]]

readLines :: Char -> Int -> Int -> Board -> IO Board
readLines gameChar _ 0 board = return (reverse board)
readLines gameChar gameSize rowsLeft board = do
    row <- getLine  -- X
    let rowList = charToIntList gameChar row []
    if length rowList == gameSize
        then readLines gameChar gameSize (rowsLeft - 1) (rowList:board)
        else printError "Invalid row length"


------------------------------------------------------------------------
-- Game Split
------------------------------------------------------------------------
gameType :: String -> IO Board
gameType ('T':xs) = do
    let pairs = map (\x -> read x :: Int) (words xs)
    if (head pairs == (pairs !! 1)) && (mod (head pairs) 2 == 0)
        then readLines 'T' (head pairs) (head pairs) []
        else printError "incorrect input values"
gameType ('S':xs) = readLines 'S' 9 9 []
gameType _ = printError "Unknown character"


------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------
main :: IO ()
main = do
    gameInfo <- getLine
    let board = gameType gameInfo
    board >>= (solver (head gameInfo))