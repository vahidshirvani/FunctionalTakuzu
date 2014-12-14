-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc Tests for Sudoku Logic Solver, Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module Main where

import AbstractGameLogic (Board,Row,Cell
                          ,runRule
                          ,applyOnceInBothDirections
                          ,applyRowFnUnidirectional
                          ,applyRowFnBidirectional)
import SudokuLogic
import Test.Tasty
import Test.Tasty.HUnit


--
-- Board
--
testSubBoard :: Board -> Board -> TestTree
testSubBoard input output = testCase
    "Test to convert board to rows of 3x3 sub boards"
    (toSubBoard input @?= output)


testReverseSubBoard :: Board -> TestTree
testReverseSubBoard board = testCase
    "Test if subboard of subboard is again the original board"
    (toSubBoard (toSubBoard board) @?= board)

--
-- Aggregate all tests and run
--

allTests :: TestTree
allTests = testGroup "SudokuLogicTests" [
        testGroup "Board and 3x3 sub boards" [
                -- 3x3 board should return itself
                testSubBoard [[1,2,3],[4,5,6],[7,8,9]] [[1,2,3,4,5,6,7,8,9]],

                -- Test reals 3x3 sub boards of a 9x9 board
                testSubBoard [[9,6,1,5,3,7,2,8,4],
                              [2,8,7,4,1,9,6,3,5],
                              [3,4,5,2,8,6,1,7,9],
                              [5,3,4,6,7,8,9,1,2],
                              [6,7,2,1,9,5,3,4,8],
                              [1,9,8,3,4,2,5,6,7],
                              [8,5,9,7,6,1,4,2,3],
                              [4,2,6,8,5,3,7,9,1],
                              [7,1,3,9,2,4,8,5,6]]

                              [[9,6,1,2,8,7,3,4,5],
                               [5,3,7,4,1,9,2,8,6],
                               [2,8,4,6,3,5,1,7,9],
                               [5,3,4,6,7,2,1,9,8],
                               [6,7,8,1,9,5,3,4,2],
                               [9,1,2,3,4,8,5,6,7],
                               [8,5,9,4,2,6,7,1,3],
                               [7,6,1,8,5,3,9,2,4],
                               [4,2,3,7,9,1,8,5,6]],

                testReverseSubBoard [[9,6,1,5,3,7,2,8,4],
                                     [2,8,7,4,1,9,6,3,5],
                                     [3,4,5,2,8,6,1,7,9],
                                     [5,3,4,6,7,8,9,1,2],
                                     [6,7,2,1,9,5,3,4,8],
                                     [1,9,8,3,4,2,5,6,7],
                                     [8,5,9,7,6,1,4,2,3],
                                     [4,2,6,8,5,3,7,9,1],
                                     [7,1,3,9,2,4,8,5,6]]
            ]
    ]

main :: IO ()
main = defaultMain allTests
