--import Data.List (transpose)

type Row = [Int]
type Board = [Row]

opposite :: Int -> Int
opposite 1 = 0
opposite 0 = 1
opposite (-1) = -1  -- the opposite of an empty cell is an empty cell

------------------------------------------------------------------------
-- Algorithms from conceptispuzzles
-- @see: http://www.conceptispuzzles.com/index.aspx?uri=puzzle/tic-tac-logic/techniques
------------------------------------------------------------------------

avoidTripleForward :: Row -> Row -> Row
avoidTripleForward [] acc = acc  -- it will be called twice --> no reverse needed here
avoidTripleForward (x:y:z:xs) acc =  -- > 2 cells remaining
    if ((x == y) && (x /= -1) && (z == -1))
        then avoidTripleForward (y:(opposite x):xs) (x:acc)
        else avoidTripleForward (y:z:xs) (x:acc)
avoidTripleForward (z:xs) acc = avoidTripleForward xs (z:acc)  -- 1 or 2 cells remaining

avoidTripleBidirectional :: Row -> Row
avoidTripleBidirectional row = avoidTripleForward (avoidTripleForward row []) []


------------------------------------------------------------------------
-- Solver
------------------------------------------------------------------------

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
     