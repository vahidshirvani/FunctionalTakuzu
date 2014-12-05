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
     