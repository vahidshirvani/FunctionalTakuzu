charToIntList :: [Char] -> [Int] -> [Int]
charToIntList [] list = (reverse list)
charToIntList ('X':xs) list = charToIntList xs (1:list)
charToIntList ('O':xs) list = charToIntList xs (0:list)
charToIntList ('.':xs) list = charToIntList xs (-1:list)

readLines :: Char -> Int -> [[Int]] -> IO ()
readLines t 0 board = 
    print (show (reverse board))
readLines t x board = do
    row <- getLine -- X
    readLines t (x-1) ((charToIntList row []):board)

gameType :: [Char] -> IO ()
gameType ('T':xs) = do
    let pairs = (map (\x -> read x :: Int) (words xs))
    if ((pairs !! 0) == (pairs !! 1)) && ((mod (pairs !! 0) 2) == 0) 
        then (readLines 'T' (pairs !! 0) [])
        else print "incorrect input values"
gameType ('S':xs) = 
    print "sudoku"
gameType _ = 
    print "unknown character" 

main :: IO ()
main = do
    gameInfo <- getLine
    gameType gameInfo
    
     