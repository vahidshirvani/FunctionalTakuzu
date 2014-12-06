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
    if (x == y) && (y == z)
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

compareTwoRows :: Row -> Board -> Bool        
compareTwoRows rowA [] = False        
compareTwoRows rowA (rowB:rows) =
    if (rowA == rowB)
        then True
        else compareTwoRows rowA rows
       
twoRowsWereSimilar :: Board -> Bool
twoRowsWereSimilar [] = False
twoRowsWereSimilar (row:rows) =
    if (compareTwoRows row rows)
        then True
        else twoRowsWereSimilar rows
   
isValid :: Board -> Bool
isValid board = 
    (not (foundTripleInRows board)) && 
    (not (foundTripleInRows (transpose board))) &&
    (not (twoRowsWereSimilar board)) &&
    (not (twoRowsWereSimilar (transpose board)))

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
-- Avoiding triples 3 (is not working yet)
--
-- e.g. 3 fields empty, 3xO, 2x1
-- if we set one 1 cell to 0, the other two cells have to be 1
-- but if they are neighbours with a neighbour 1 this would be invalid
-- if half of the row is of the same type, e.g. 0, we have to fill up cells with 1
--

isOneStepBeforeFinishType :: Cell -> Row -> Bool
isOneStepBeforeFinishType t row =
    let numOfOccurences = countCellsOfType t row
    in  -- + 1 because we can make a move
        if (numOfOccurences + 1) == (div (length row) 2)
        then True
        else False

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

checkRowForXO :: Cell -> Board -> Board
checkRowForXO xo board =
    let helper i [] acc = reverse acc
        helper i (r:rs) acc = 
            if (isOneStepBeforeFinishType xo r)
            then 
                let rows = getPossibleRows xo r
                    completeRows = [fillRow (opposite xo) row | row <- rows]
                    boards = [replaceRowInBoard i row board | row <- completeRows]
                    bools = [isValid b | b <- boards]
                    index = indexOfElement False bools                   
                in 
                    if (index == (-1))
                    then helper (i+1) rs (r:acc)
                    else helper (i+1) rs (((getPossibleRows (opposite xo) r) !! index):acc)
            else helper (i+1) rs (r:acc)
    in helper 0 board []
        
avoidTripleThree :: Board -> Board
avoidTripleThree board = 
    let rX = checkRowForXO 1 board
        -- rO = checkRowForXO 0 rX
        -- cX = checkRowForXO 1 (transpose rO)
        -- cO = checkRowForXO 0 cX
        -- newBoard = transpose cO
        newBoard = rX
    in newBoard

--
-- Completing a row or a column
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
   
------------------------------------------------------------------------
-- Solver
------------------------------------------------------------------------

-- runRule [[-1,1,-1,-1,-1,-1],[0,0,-1,-1,1,1],[0,1,0,1,0,1],[-1,1,-1,-1,-1,-1]] avoidTripleOne
runRule :: Board -> (Board -> Board) -> Board
runRule board ruleFn =
    let newBoard = ruleFn board
    in if (board == newBoard)
            then board
            else runRule newBoard ruleFn

solver :: Board -> IO ()
solver board = print board


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
     