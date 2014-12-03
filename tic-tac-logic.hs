readLines :: Char -> Int -> [String] -> IO () 
readLines t 0 board = 
    print (show (reverse board))
readLines t x board = do
    row <- getLine
    readLines t (x-1) (row:board)

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
    
     