-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc Abstract Game Logic for TicTac and Sudoku, Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module AbstractGameLogic where

import Data.List (transpose)

------------------------------------------------------------------------
-- Board
--
-- We represent the board with a list of numbers where
-- TicTac has 1 or 0,
-- Sudoku has 1 to 9
-- and both use -1 for empty cells
------------------------------------------------------------------------
type Cell = Int
type Row = [Cell]
type Board = [Row]


------------------------------------------------------------------------
-- Abstract Board Helper Functions
------------------------------------------------------------------------
countCellsOfType :: Cell -> Row -> Int
countCellsOfType t = length . filter (== t)

countEmptyCells :: Row -> Int
countEmptyCells = countCellsOfType (-1)

-- will check if the board contains any emtpy cells
isComplete :: Board -> Bool
isComplete = foldr (\ row -> (&&) (countEmptyCells row == 0)) True

-- will return a list of all the complete rows in the board
getCompleteRows :: Board -> [Row]
getCompleteRows board =
    let helper [] acc = reverse acc
        helper (r:rs) acc =
            if countEmptyCells r == 0
            then helper rs (r:acc)
            else helper rs acc
    in helper board []

indexOfElement :: Cell -> Row -> Int
indexOfElement elem list =
    let helper [] cnt = -1
        helper (x:xs) cnt =
            if x == elem
            then cnt
            else helper xs (cnt+1)
    in helper list 0

-- replace the element in the specified index with the given element
replaceElementInRow :: Int -> Cell -> Row -> Row
replaceElementInRow index xo row =
    let helper i [] acc = reverse acc
        helper 0 (r:rs) acc = helper (-1) rs (xo:acc)
        helper i (r:rs) acc = helper (i-1) rs (r:acc)
    in helper index row []

-- replace the row in the specified index with the given row
replaceRowInBoard :: Int -> Row -> Board -> Board
replaceRowInBoard index row board =
    let helper i [] acc = reverse acc
        helper 0 (x:xs) acc = helper (-1) xs (row:acc)
        helper i (x:xs) acc = helper (i-1) xs (x:acc)
    in helper index board []

-- get the indices were the element occur in the list
getElementIndices :: Cell -> Row -> [Int]
getElementIndices xo row =
    let helper [] i acc = reverse acc
        helper (r:rs) i acc =
            if r == xo
            then helper rs (i+1) (i:acc)
            else helper rs (i+1) acc
    in helper row 0 []

-- put the specified element once in every empty spot and return all combinations
getPossibleRows :: Cell -> Row -> [Row]
getPossibleRows xo row =
    let emptyIndices = getElementIndices (-1) row
    in [replaceElementInRow spot xo row | spot <- emptyIndices]


------------------------------------------------------------------------
-- High-order function for rule-based solving
------------------------------------------------------------------------
applyOnceInBothDirections :: Board -> (Row -> Row) -> Board
applyOnceInBothDirections board rowFn =
    let rowsFixedBoard = map rowFn board
        transposedBoard = transpose rowsFixedBoard
        colsFixedBoard = map rowFn transposedBoard
        newFixedBoard = transpose colsFixedBoard
    in newFixedBoard

applyRowFnUnidirectional :: (Row -> Row -> Row) -> Row -> Row
applyRowFnUnidirectional rowFn = rowFn []

applyRowFnBidirectional :: (Row -> Row -> Row) -> Row -> Row
applyRowFnBidirectional rowFn = rowFn [] . rowFn []

runRule :: Board -> (Board -> Board) -> Board
runRule board ruleFn =
    let newBoard = ruleFn board
    in if board == newBoard
            then board
            else runRule newBoard ruleFn

goThroughAllRules :: Board -> [Board -> Board] -> Board
goThroughAllRules board [] = board
goThroughAllRules board (rule:rules) =
    let newBoard = runRule board rule
    in
        if isComplete newBoard
        then newBoard
        else goThroughAllRules newBoard rules

solve :: Board -> [Board -> Board] -> Board
solve board listOfAllRuleFn =
    let newBoard = goThroughAllRules board listOfAllRuleFn
    in
        if newBoard == board -- it's either solved or it isn't solvable
        then newBoard
        else goThroughAllRules newBoard listOfAllRuleFn