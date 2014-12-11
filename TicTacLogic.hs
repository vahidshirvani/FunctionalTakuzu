module TicTacLogic where

import Data.List (transpose)

type Cell = Int
type Row = [Cell]
type Board = [Row]

opposite :: Int -> Int
opposite 1 = 0
opposite 0 = 1
opposite (-1) = -1  -- the opposite of an empty cell is an empty cell

countCellsOfType :: Cell -> (Row -> Int)
countCellsOfType t = length . filter (== t)

countEmptyCells :: Row -> Int
countEmptyCells row = countCellsOfType (-1) row

-- will check if the board contains any emtpy cells
isComplete :: Board -> Bool
isComplete [] = True
isComplete (r:rs) = 
    if (countEmptyCells r == 0)
    then isComplete rs
    else False

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
foundTripleInRow (x:y:z:xs) =
    if (x == y) && (x == z) && (x /= (-1))
        then True
        else foundTripleInRow (y:z:xs)
foundTripleInRow (x:xs) =
    foundTripleInRow xs

foundTripleInRows :: Board -> Bool
foundTripleInRows [] = False
foundTripleInRows (row:rows) =
    if (foundTripleInRow row)
        then True
        else foundTripleInRows rows

rowEqualOneRowInRows :: Row -> Board -> Bool
rowEqualOneRowInRows rowA [] = False
rowEqualOneRowInRows rowA (rowB:rows) =
    if (rowA == rowB && (countEmptyCells rowA) == 0)
        then True
        else rowEqualOneRowInRows rowA rows

twoRowsWereSimilar :: Board -> Bool
twoRowsWereSimilar [] = False
twoRowsWereSimilar (row:rows) =
    if (rowEqualOneRowInRows row rows)
        then True
        else twoRowsWereSimilar rows

equalOrLessOfType :: Board -> Bool
equalOrLessOfType [] = True
equalOrLessOfType (r:rs) =
    let numOfOccurences0 = countCellsOfType 0 r
        numOfOccurences1 = countCellsOfType 1 r
        half = (div (length r) 2)
    in
        if (numOfOccurences0 <= half && numOfOccurences1 <= half)
        then equalOrLessOfType rs
        else False

isValid :: Board -> Bool
isValid board =
    (not (foundTripleInRows board)) &&
    (not (foundTripleInRows (transpose board))) &&
    (not (twoRowsWereSimilar board)) &&
    (not (twoRowsWereSimilar (transpose board))) &&
    (equalOrLessOfType board) &&
    (equalOrLessOfType (transpose board))

------------------------------------------------------------------------
-- Algorithms from conceptispuzzles
-- @see: http://www.conceptispuzzles.com/index.aspx?uri=puzzle/tic-tac-logic/techniques
------------------------------------------------------------------------

--
-- High-order functions
--

applyOnceInBothDirections :: Board -> (Row -> Row) -> Board
applyOnceInBothDirections board rowFn =
    let rowsFixedBoard = map rowFn board
        transposedBoard = transpose rowsFixedBoard
        colsFixedBoard = map rowFn transposedBoard
        newFixedBoard = transpose colsFixedBoard
    in newFixedBoard

applyRowFnUnidirectional :: (Row -> Row -> Row) -> (Row -> Row)
applyRowFnUnidirectional rowFn = rowFn []

applyRowFnBidirectional :: (Row -> Row -> Row) -> (Row -> Row)
applyRowFnBidirectional rowFn = (rowFn []) . (rowFn [])

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
    if ((x == y) && (x /= -1) && (z == -1))
        then avoidTripleForward (x:acc) (y:(opposite x):xs)
        else avoidTripleForward (x:acc) (y:z:xs)
avoidTripleForward acc (z:xs) = avoidTripleForward (z:acc) xs   -- 1 or 2 cells remaining


-- avoidTripleOne [[-1,1,-1,-1,-1,-1],[0,0,-1,-1,1,1],[0,1,0,1,0,1],[-1,1,-1,-1,-1,-1]]
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
        then checkMiddleCell ((opposite x):x:acc) (z:xs)
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
    in  -- + 1 because we can make a move
        if (numOfOccurences + empty) == (div (length row) 2)
        then True
        else False

indexOfElement :: Cell -> Row -> Int 
indexOfElement elem list =
    let helper [] cnt = -1
        helper (x:xs) cnt =
            if (x == elem)
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
            if (r == xo)
            then helper rs (i+1) (i:acc)
            else helper rs (i+1) acc
    in helper row 0 []

-- put the specified element once in every empty spot and return all combinations
getPossibleRows :: Cell -> Row -> [Row]
getPossibleRows xo row =
    let emptyIndices = getElementIndices (-1) row
    in [replaceElementInRow spot xo row | spot <- emptyIndices]

getIndexOfVerifiedBoard :: Cell -> Board -> [Row] -> Int -> Bool -> Int
getIndexOfVerifiedBoard xo board rows index cond =
    let helper [] i = (-1)
        helper (r:rs) i =
            let completeRow = fillRow (opposite xo) r
                newBoard = replaceRowInBoard index completeRow board
                bool = isValid newBoard
            in
                if (bool == cond)
                then i
                else helper rs (i+1)
    in helper rows 0

-- we found out the right answer by making a wrong move
solveByEliminatingWrongs :: Cell -> Board -> Board
solveByEliminatingWrongs xo board =
    let helper i [] acc = reverse acc
        helper i (r:rs) acc =
            if (xStepBeforeFinish 1 xo r)
            then
                let rows = getPossibleRows xo r
                    index = getIndexOfVerifiedBoard xo board rows i False
                in
                    if (index == (-1))
                    then helper (i+1) rs (r:acc)
                    else helper (i+1) rs (((getPossibleRows (opposite xo) r) !! index):acc)
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
        if (numOfOccurences == (div (length row) 2))
        then fillRow (opposite xo) row
        else row

tryToFillBoth :: Row -> Row
tryToFillBoth =
    (tryToFill 0) . (tryToFill 1)

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
        helper (x:xs) (y:ys) acc =
            if (x == y)
            then helper xs ys (x:acc)
            else
                if (x == (-1))
                then helper xs ys ((opposite y):acc)
                else rowA
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
        if (result == row)
        then compareRowAgainstRows row rs
        else result

-- will return a list of all the complete rows in the board
getCompleteRows :: Board -> [Row]
getCompleteRows board =
    let helper [] acc = reverse acc
        helper (r:rs) acc =
            if ((countEmptyCells r) == 0)
            then helper rs (r:acc)
            else helper rs acc
    in helper board []

-- goes through the board row-wise and tries to fill the rows
-- that only have two empty spots left
avoidRowDuplicates :: Board -> Board
avoidRowDuplicates board =
    let completeRows = getCompleteRows board
        helper [] acc = reverse acc
        helper (r:rs) acc =
            if ((countEmptyCells r) == 2)
            then helper rs ((compareRowAgainstRows r completeRows):acc)
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
            if (xStepBeforeFinish 2 xo r1)
            then
                let rows1 = getPossibleRows xo r1
                    indices = [
                        let rows2 = getPossibleRows xo r2
                        in getIndexOfVerifiedBoard xo board rows2 i True | r2 <- rows1]
                    index = indexOfElement (-1) indices
                in
                    if (index == (-1))
                    then helper (i+1) rs (r1:acc)
                    else helper (i+1) rs (((getPossibleRows (opposite xo) r1) !! index):acc)
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
-- Run Logic
--

-- runRule [[-1,1,-1,-1,-1,-1],[0,0,-1,-1,1,1],[0,1,0,1,0,1],[-1,1,-1,-1,-1,-1]] avoidTripleOne
runRule :: Board -> (Board -> Board) -> Board
runRule board ruleFn =
    let newBoard = ruleFn board
    in if board == newBoard
            then board
            else runRule newBoard ruleFn
