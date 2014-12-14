-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc Sudoku Logic Solver, Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module SudokuLogic where

import AbstractGameLogic
import Data.List (transpose)

--
-- Rule Aggregation
--

sudokuRuleFnList :: [Board -> Board]
sudokuRuleFnList = []

solveSudoku :: Board -> Board
solveSudoku board = AbstractGameLogic.solve board sudokuRuleFnList

