-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc TicTac Logic Solver, Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module TicTacLogic where

import AbstractGameLogic
import Data.List (transpose)

------------------------------------------------------------------------
-- TicTac Logic Solver
--
-- Our tic-tac logic board game is based on a board that is defined with
-- the types below. We basically represent the board with a list of
-- 1, 0 and -1, where -1 is an empty cell.
--
-- To solve a takzu you can call the solve function which is defined
-- in the end of this file.
-- Example: solve [[0,0],[1,0]]
--
-- Restriction: It only works for Takuzus that have 1 single solution
------------------------------------------------------------------------

opposite :: Int -> Int
opposite 1 = 0
opposite 0 = 1
opposite (-1) = -1  -- the opposite of an empty cell is an empty cell

-- fill the empty spots with the specified element
fillRow :: Cell -> Row -> Row
fillRow xo row =
    let helper [] acc = reverse acc
        helper ((-1):xs) acc = helper xs (xo:acc)
        helper (x:xs) acc = helper xs (x:acc)
    in helper row []

------------------------------------------------------------------------
-- Verifier
-- will check if a board is valid
------------------------------------------------------------------------

foundTripleInRow :: Row -> Bool
foundTripleInRow [] = False
foundTripleInRow (x:y:z:xs) = ((x == y) && (x == z) && (x /= (-1))) || foundTripleInRow (y : z : xs)

foundTripleInRow (x:xs) = foundTripleInRow xs

foundTripleInRows :: Board -> Bool
foundTripleInRows = foldr ((||) . foundTripleInRow) False

rowEqualOneRowInRows :: Row -> Board -> Bool
rowEqualOneRowInRows rowA = foldr
       (\ rowB -> (||) (rowA == rowB && countEmptyCells rowA == 0))
        False

twoRowsWereSimilar :: Board -> Bool
twoRowsWereSimilar [] = False
twoRowsWereSimilar (row:rows) = rowEqualOneRowInRows row rows || twoRowsWereSimilar rows


equalOrLessOfType :: Board -> Bool
equalOrLessOfType [] = True
equalOrLessOfType (r:rs) =
    let numOfOccurences0 = countCellsOfType 0 r
        numOfOccurences1 = countCellsOfType 1 r
        half = div (length r) 2
    in (numOfOccurences0 <= half && numOfOccurences1 <= half) && equalOrLessOfType rs


isValid :: Board -> Bool
isValid board =
    not (foundTripleInRows board) &&
    not (foundTripleInRows (transpose board)) &&
    not (twoRowsWereSimilar board) &&
    not (twoRowsWereSimilar (transpose board)) &&
    equalOrLessOfType board &&
    equalOrLessOfType (transpose board)

------------------------------------------------------------------------
-- Algorithms from conceptispuzzles
-- @see: http://www.conceptispuzzles.com/index.aspx?uri=puzzle/tic-tac-logic/techniques
------------------------------------------------------------------------

--
-- Avoiding triples 1
-- = Basic techniques 1
--
-- 1 1 -1 -> 1 1 0
-- 0 0 -1 -> 0 0 1
--

avoidTripleForward :: Row -> Row -> Row
avoidTripleForward acc [] = acc  -- it will be called twice --> no reverse needed here
avoidTripleForward acc (x:y:z:xs) =  -- > 2 cells remaining
    if (x == y) && (x /= -1) && (z == -1)
        then avoidTripleForward (x:acc) (y:opposite x:xs)
        else avoidTripleForward (x:acc) (y:z:xs)
avoidTripleForward acc (z:xs) = avoidTripleForward (z:acc) xs   -- 1 or 2 cells remaining


avoidTripleOne :: Board -> Board
avoidTripleOne board = applyOnceInBothDirections board (applyRowFnBidirectional avoidTripleForward)


--
-- Avoiding triples 2
-- = Basic techniques 2
--
-- 1 -1 1 -> 1 0 1
-- 0 -1 0 -> 0 1 0
--

checkMiddleCell :: Row -> Row -> Row
checkMiddleCell acc [] = reverse acc
checkMiddleCell acc (x:y:z:xs) =
    if (x == z) && (y == -1)
        then checkMiddleCell (opposite x:x:acc) (z:xs)
        else checkMiddleCell (x:acc) (y:z:xs)
checkMiddleCell acc (x:xs) = checkMiddleCell (x:acc) xs


avoidTripleTwo :: Board -> Board
avoidTripleTwo board = applyOnceInBothDirections board (applyRowFnUnidirectional checkMiddleCell)


--
-- Avoiding triples 3
-- = Basic techniques 3
--
-- e.g. 3 fields empty, 3xO, 2x1
-- if we set one 1 cell to 0, the other two cells have to be 1
-- but if they are neighbours with a neighbour 1 this would be invalid
-- if half of the row is of the same type, e.g. 0, we have to fill up cells with 1
--
xStepBeforeFinish :: Cell -> Cell -> Row -> Bool
xStepBeforeFinish empty t row =
    let numOfOccurences = countCellsOfType t row
    in ((numOfOccurences + empty) == div (length row) 2)  -- + 1 because we can make a move

getIndexOfVerifiedBoard :: Cell -> Board -> [Row] -> Int -> Bool -> Int
getIndexOfVerifiedBoard xo board rows index cond =
    let helper [] i = (-1)
        helper (r:rs) i =
            let completeRow = fillRow (opposite xo) r
                newBoard = replaceRowInBoard index completeRow board
                bool = isValid newBoard
            in
                if bool == cond
                then i
                else helper rs (i+1)
    in helper rows 0

-- we found out the right answer by making a wrong move
solveByEliminatingWrongs :: Cell -> Board -> Board
solveByEliminatingWrongs xo board =
    let helper i [] acc = reverse acc
        helper i (r:rs) acc =
            if xStepBeforeFinish 1 xo r
            then
                let rows = getPossibleRows xo r
                    index = getIndexOfVerifiedBoard xo board rows i False
                in
                    if index == (-1)
                    then helper (i+1) rs (r:acc)
                    else helper (i+1) rs ((getPossibleRows (opposite xo) r !! index):acc)
            else helper (i+1) rs (r:acc)
    in helper 0 board []

avoidTripleThree :: Board -> Board
avoidTripleThree board =
    let rX = solveByEliminatingWrongs 1 board
        rO = solveByEliminatingWrongs 0 rX
        cX = solveByEliminatingWrongs 1 (transpose rO)
        cO = solveByEliminatingWrongs 0 cX
        newBoard = transpose cO
    in newBoard

--
-- Completing a row or a column
-- = Basic techniques 4
--

tryToFill :: Cell -> Row -> Row
tryToFill xo row =
    let numOfOccurences = countCellsOfType xo row
    in
        if numOfOccurences == div (length row) 2
        then fillRow (opposite xo) row
        else row

tryToFillBoth :: Row -> Row
tryToFillBoth =
    tryToFill 0 . tryToFill 1

completingRowOrColumn :: Board -> Board
completingRowOrColumn board =
    applyOnceInBothDirections board tryToFillBoth

--
-- Avoiding row or column duplication
-- = Extra Requirements (a) Basic Technique 5. (1 point)
--

-- if the difference between the two rows are limited to two empty spots
-- then return a complete row which is different from the second row
compareTwoRows :: Row -> Row -> Row
compareTwoRows rowA rowB =
    let helper [] [] acc = reverse acc
        helper (x : xs) (y : ys) acc
            | x == y = helper xs ys (x : acc)
            | x == (-1) = helper xs ys (opposite y : acc)
            | otherwise = rowA

    in helper rowA rowB []

-- compare a non complete row with some other completed rows
-- and return a different completed version if possible
-- e.g. input [1,-1,-1,1] [[1,0,1,1]]
-- will output [1,1,0,1]
compareRowAgainstRows :: Row -> [Row] -> Row
compareRowAgainstRows row [] = row
compareRowAgainstRows row (r:rs) =
    let result = compareTwoRows row r
    in
        if result == row
        then compareRowAgainstRows row rs
        else result

-- goes through the board row-wise and tries to fill the rows
-- that only have two empty spots left
avoidRowDuplicates :: Board -> Board
avoidRowDuplicates board =
    let completeRows = getCompleteRows board
        helper [] acc = reverse acc
        helper (r:rs) acc =
            if countEmptyCells r == 2
            then helper rs (compareRowAgainstRows r completeRows:acc)
            else helper rs (r:acc)
    in helper board []

avoidingRowOrColumnDuplication :: Board -> Board
avoidingRowOrColumnDuplication board =
    let rowsFixedBoard = avoidRowDuplicates board
        transposedBoard = transpose rowsFixedBoard
        colsFixedBoard = avoidRowDuplicates transposedBoard
        newFixedBoard = transpose colsFixedBoard
    in newFixedBoard

--
-- Advanced technique 1
-- = Extra Requirements (b) Advanced Technique 1. (2 points)
--

-- we found out the right answer by making a wrong move
solveByEliminatingWrongs2 :: Cell -> Board -> Board
solveByEliminatingWrongs2 xo board =
    let helper i [] acc = reverse acc
        helper i (r1:rs) acc =
            if xStepBeforeFinish 2 xo r1
            then
                let rows1 = getPossibleRows xo r1
                    indices = [
                        let rows2 = getPossibleRows xo r2
                        in getIndexOfVerifiedBoard xo board rows2 i True | r2 <- rows1]
                    index = indexOfElement (-1) indices
                in
                    if index == (-1)
                    then helper (i+1) rs (r1:acc)
                    else helper (i+1) rs ((getPossibleRows (opposite xo) r1 !! index):acc)
            else helper (i+1) rs (r1:acc)
    in helper 0 board []

advancedTechniqueOne :: Board -> Board
advancedTechniqueOne board =
    let rX = solveByEliminatingWrongs2 1 board
        rO = solveByEliminatingWrongs2 0 rX
        cX = solveByEliminatingWrongs2 1 (transpose rO)
        cO = solveByEliminatingWrongs2 0 cX
        newBoard = transpose cO
    in newBoard

--
-- Advanced technique 2
-- = Extra Requirements (c) Advanced Technique 2. (2 points)
--

-- The verification phase for the avoidTripleThree will also check for duplicate rows
advancedTechniqueTwo :: Board -> Board
advancedTechniqueTwo = avoidTripleThree


--
-- Rule Aggregation
--

takuzuRuleFnList :: [Board -> Board]
takuzuRuleFnList = [ avoidTripleOne,
                    avoidTripleTwo,
                    avoidTripleThree,
                    completingRowOrColumn,
                    avoidingRowOrColumnDuplication,
                    advancedTechniqueOne,
                    advancedTechniqueTwo
                    ]

solveTakuzu :: Board -> Board
solveTakuzu board = AbstractGameLogic.solve board takuzuRuleFnList
