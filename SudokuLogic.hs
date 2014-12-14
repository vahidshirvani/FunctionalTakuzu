-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc Sudoku Logic Solver, Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module SudokuLogic where

import AbstractGameLogic
import Data.List (transpose)

allCellValues :: [Cell]
allCellValues = [1..9]

possibleValues :: Row -> [Cell]
possibleValues row = filter (`notElem` row) allCellValues


--
-- 3x3 SubBoards
--
toSubBoard :: Board -> Board
toSubBoard board =
    let helper :: Board -> Board -> Board
        helper [] acc = reverse acc
        helper ([]:[]:[]:xs) acc = helper xs acc
        helper (row1:row2:row3:xs) acc =
            let (subA, rest1) = splitAt 3 row1
                (subB, rest2) = splitAt 3 row2
                (subC, rest3) = splitAt 3 row3
                newSubBoardRow = subA++subB++subC
                restOfBoard = rest1:rest2:rest3:xs
            in helper restOfBoard (newSubBoardRow:acc)
    in helper board []


applyOnceInAllDirections :: Board -> (Row -> Row) -> Board
applyOnceInAllDirections board rowFn =
    let appliedHorAndVert = applyOnceInBothDirections board rowFn  -- horizontal and vertical, see AbstractGameLogic
        subBoard = toSubBoard appliedHorAndVert
        appliedToSubBoard = map rowFn subBoard
        newFixedBoard = toSubBoard appliedToSubBoard
    in newFixedBoard


--
-- Completing a row, a column or a 3x3 sub board
--

fillRow :: Row -> Row
fillRow row =
    let possibleMoves = possibleValues row
        numberOfPossibleMoves = length possibleMoves
    in if numberOfPossibleMoves == 1
       then replaceEmptyCellInRow row (head possibleMoves)
       else row

completeRowColAndSubBoard :: Board -> Board
completeRowColAndSubBoard board = applyOnceInAllDirections board fillRow


--
-- Rule Aggregation
--

sudokuRuleFnList :: [Board -> Board]
sudokuRuleFnList = [completeRowColAndSubBoard]

solveSudoku :: Board -> Board
solveSudoku board = AbstractGameLogic.solve board sudokuRuleFnList

