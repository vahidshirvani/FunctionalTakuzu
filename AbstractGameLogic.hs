-- ----------------------------------------------------------------------------------------------
-- @author Emanuel Stoeckli, Vahid Shirvani
-- @date 2014-12
-- @desc Abstract Game Logic for TicTac and Sudoku, Advanced Functional Programming, HT2014
-- ----------------------------------------------------------------------------------------------

module AbstractGameLogic where

------------------------------------------------------------------------
-- Board
--
-- We represent the board with a list of numbers where
-- TicTac has 1 or 0,
-- Sudoku has 1 to 9
-- and both use -1 for empty cells
------------------------------------------------------------------------
type Cell = Int
type Row = [Cell]
type Board = [Row]


------------------------------------------------------------------------
-- Abstract Helper Functions
------------------------------------------------------------------------
countCellsOfType :: Cell -> Row -> Int
countCellsOfType t = length . filter (== t)

countEmptyCells :: Row -> Int
countEmptyCells = countCellsOfType (-1)

-- will check if the board contains any emtpy cells
isComplete :: Board -> Bool
isComplete = foldr (\ row -> (&&) (countEmptyCells row == 0)) True


------------------------------------------------------------------------
-- Rule-based Solving
------------------------------------------------------------------------
runRule :: Board -> (Board -> Board) -> Board
runRule board ruleFn =
    let newBoard = ruleFn board
    in if board == newBoard
            then board
            else runRule newBoard ruleFn

goThroughAllRules :: Board -> [Board -> Board] -> Board
goThroughAllRules board [] = board
goThroughAllRules board (rule:rules) =
    let newBoard = runRule board rule
    in
        if isComplete newBoard
        then newBoard
        else goThroughAllRules newBoard rules

solve :: Board -> [Board -> Board] -> Board
solve board listOfAllRuleFn =
    let newBoard = goThroughAllRules board listOfAllRuleFn
    in
        if newBoard == board -- it's either solved or it isn't solvable
        then newBoard
        else goThroughAllRules newBoard listOfAllRuleFn