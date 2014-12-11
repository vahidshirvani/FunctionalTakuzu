module Main where

import TicTacLogic
import Test.Tasty
import Test.Tasty.HUnit

--
-- Avoiding triples 1
--
testFillOppositeAfterPairs :: Row -> Row -> TestTree
testFillOppositeAfterPairs input output = testCase
    "Testing 1 Row with two pairs (X X -1) Bidirectional"
    (applyRowFnBidirectional avoidTripleForward input @?= output)

testFillOppositeAfterPairsUnidirectional :: Row -> Row -> TestTree
testFillOppositeAfterPairsUnidirectional input output = testCase
    "Testing 1 Row with two pairs (X X -1) Unidirectional"
    (applyRowFnUnidirectional avoidTripleForward input @?= reverse output)

testAvoidTripleOne :: Board -> Board -> TestTree
testAvoidTripleOne input output = testCase
    "Testing 1 Board with pairs (X X -1) in both directions"
    (avoidTripleOne input @?= output)

testAvoidTripleOneRecursive :: Board -> Board -> TestTree
testAvoidTripleOneRecursive input output = testCase
    "Testing 1 Board with pairs (X X -1) in both directions, with RECURSION"
    (runRule input avoidTripleOne @?= output)


--
-- Avoiding triples 2
--
testAvoidTripleTwoRow :: Row -> Row -> TestTree
testAvoidTripleTwoRow input output = testCase
    "Testing 1 Row with empty middle cell (X -1 X)"
    (applyRowFnUnidirectional checkMiddleCell input @?= output)

testAvoidTripleTwo :: Board -> Board -> TestTree
testAvoidTripleTwo input output = testCase
    "Testing 1 Board with empty middle cell (X -1 X), ONLY 1 iteration"
    (avoidTripleTwo input @?= output)

testAvoidTripleTwoRecursive :: Board -> Board -> TestTree
testAvoidTripleTwoRecursive input output = testCase
    "Testing 1 Board with empty middle cell (X -1 X), with RECURSION"
    (runRule input avoidTripleTwo @?= output)


--
-- Avoiding triples 3
--

testXStepBeforeFinish :: Cell -> Cell -> Row -> Bool -> TestTree
testXStepBeforeFinish input1 input2 input3 output = testCase
    "Testing 1 Row to see if it is in our interest"
    (xStepBeforeFinish input1 input2 input3 @?= output)

testIndexOfElement :: Cell -> Row -> Int -> TestTree
testIndexOfElement input1 input2 output = testCase
    "Testing 1 Row to find the index of element"
    (indexOfElement input1 input2 @?= output)

testReplaceElementInRow :: Int -> Cell -> Row -> Row -> TestTree
testReplaceElementInRow input1 input2 input3 output = testCase
    "Testing 1 Row to replace an element"
    (replaceElementInRow input1 input2 input3 @?= output)

testGetElementIndices :: Cell -> Row -> [Int] -> TestTree
testGetElementIndices input1 input2 output = testCase
    "Testing 1 Row to find indices of an element occurrences"
    (getElementIndices input1 input2 @?= output)

testGetPossibleRows :: Cell -> Row -> [Row] -> TestTree
testGetPossibleRows input1 input2 output = testCase
    "Testing 1 Row to find all possible combinations"
    (getPossibleRows input1 input2 @?= output)

testGetIndexOfVerifiedBoard :: Cell -> Board -> [Row] -> Int -> Bool -> Int -> TestTree
testGetIndexOfVerifiedBoard input1 input2 input3 input4 input5 output = testCase
    "Testing 1 several rows to find index of verified row"
    (getIndexOfVerifiedBoard input1 input2 input3 input4 input5 @?= output)

testSolveByEliminatingWrongs :: Cell -> Board -> Board -> TestTree
testSolveByEliminatingWrongs input1 input2 output = testCase
    "Testing 1 Board that can be filled up, ONLY row-wise"
    (solveByEliminatingWrongs input1 input2 @?= output)

testAvoidTripleThree :: Board -> Board -> TestTree
testAvoidTripleThree input output = testCase
    "Testing 1 Board that can be filled up"
    (avoidTripleThree input @?= output)

--
-- Completing a row or a column
--

testTryToFill :: Cell -> Row -> Row -> TestTree
testTryToFill t input output = testCase
    "Testing 1 Row with various empty cells (1,1,-1,-1)"
    (tryToFill t input @?= output)

testCompletingRowOrColumn :: Board -> Board -> TestTree
testCompletingRowOrColumn input output = testCase
    "Testing 1 Board that can be filled up (1,1,-1,-1), ONLY 1 iteration"
    (completingRowOrColumn input @?= output)

--
-- Avoiding row or column duplication
--

testCompareTwoRows :: Row -> Row -> Row -> TestTree
testCompareTwoRows input1 input2 output = testCase
    "Testing 1 Row against another row (1,-1), (1,0)"
    (compareTwoRows input1 input2 @?= output)

testCompareRowAgainstRows :: Row -> [Row] -> Row -> TestTree
testCompareRowAgainstRows input1 input2 output = testCase
    "Testing 1 Row against another rows (1,-1), ((1,0))"
    (compareRowAgainstRows input1 input2 @?= output)

testGetCompleteRows :: Board -> [Row] -> TestTree
testGetCompleteRows input output = testCase
    "Testing 1 Board to return complete rows ((1,-1), (1,0))"
    (getCompleteRows input @?= output)

testAvoidRowDuplicates :: Board -> Board -> TestTree
testAvoidRowDuplicates input output = testCase
    "Testing 1 Board that can be filled up ((1,-1), (1,0)), ONLY row-wise"
    (avoidRowDuplicates input @?= output)

testAvoidingRowOrColumnDuplication :: Board -> Board -> TestTree
testAvoidingRowOrColumnDuplication input output = testCase
    "Testing 1 Board that can be filled up ((1,-1), (1,0))"
    (avoidingRowOrColumnDuplication input @?= output)

--
-- Advanced technique 1
--

testSolveByEliminatingWrongs2 :: Cell -> Board -> Board -> TestTree
testSolveByEliminatingWrongs2 input1 input2 output = testCase
    "Testing 1 Board that can be filled up, ONLY row-wise"
    (solveByEliminatingWrongs2 input1 input2 @?= output)

testAdvancedTechniqueOne :: Board -> Board -> TestTree
testAdvancedTechniqueOne input1 output = testCase
    "Testing 1 Board that can be filled up"
    (advancedTechniqueOne input1 @?= output)

--
-- Advanced technique 2
--

testAdvancedTechniqueTwo :: Board -> Board -> TestTree
testAdvancedTechniqueTwo input output = testCase
    "Testing 1 Board that can be filled up"
    (advancedTechniqueTwo input @?= output)

--
-- Aggregate all tests and run
--

allTests :: TestTree
allTests = testGroup "TicTacLogicTests" [
        testGroup "Avoiding triples 1" [
                -- Unidirectional -> check the field on the right side
                testFillOppositeAfterPairsUnidirectional [0,0,-1] [0,0,1],
                testFillOppositeAfterPairsUnidirectional [-1,0,0,-1] [-1,0,0,1],

                -- Bidirectional -> check both sides
                testFillOppositeAfterPairs [1,1,-1] [1,1,0],
                testFillOppositeAfterPairs [0,0,-1] [0,0,1],
                testFillOppositeAfterPairs [-1,0,0,-1] [1,0,0,1],
                testFillOppositeAfterPairs [-1,0,0,-1,-1] [1,0,0,1,-1],

                -- Apply rule once
                testAvoidTripleOne
                    [[-1,1,-1,-1,-1,-1],[0,0,-1,-1,1,1],[0,1,0,1,0,1],[-1,1,-1,-1,-1,-1]]
                    [[1,1,-1,-1,-1,0],[0,0,1,0,1,1],[0,1,0,1,0,1],[1,1,-1,-1,-1,0]],

                -- Recursively apply rule until board does not change anymore
                testAvoidTripleOneRecursive
                    [[-1,1,-1,-1,-1,-1],[0,0,-1,-1,1,1],[0,1,0,1,0,1],[-1,1,-1,-1,-1,-1]]
                    [[1,1,0,-1,-1,0],[0,0,1,0,1,1],[0,1,0,1,0,1],[1,1,0,-1,-1,0]]
            ]
        ,
        testGroup "Avoiding triples 2" [
                -- we start by testing a simple row
                testAvoidTripleTwoRow [1,-1,1] [1,0,1],
                testAvoidTripleTwoRow [0,-1,0] [0,1,0],
                testAvoidTripleTwoRow [-1,0,-1,0,-1] [-1,0,1,0,-1],

                -- let's test a board now
                testAvoidTripleTwo [[1,1,1],[-1,-1,-1],[1,1,1]] [[1,1,1],[0,0,0],[1,1,1]],
                testAvoidTripleTwo [[0,0,0],[-1,-1,-1],[0,0,0]] [[0,0,0],[1,1,1],[0,0,0]],

                -- we run it only once
                testAvoidTripleTwo [[0,-1,0],[-1,-1,-1],[0,-1,0]] [[0,1,0],[1,0,1],[0,1,0]],

                -- run as long as the board changes
                testAvoidTripleTwoRecursive [[0,-1,0],[-1,-1,-1],[0,-1,0]] [[0,1,0],[1,0,1],[0,1,0]]
            ]
        ,
        testGroup "Avoiding triples 3" [
                -- first test will identify row as interesting while second row is not
                testXStepBeforeFinish 1 1 [1,-1,-1,-1] True,
                testXStepBeforeFinish 1 1 [1,-1,-1,-1,-1,0] False,

                -- first test will manage to find element while next test will return (-1)
                testIndexOfElement 1 [-1,-1,-1,-1,-1,1] 5,
                testIndexOfElement 1 [-1,-1,-1,-1,-1,-1] (-1),

                -- test will manage to replace element
                testReplaceElementInRow 1 1 [-1,-1,-1,-1,-1,-1] [-1,1,-1,-1,-1,-1],

                -- test will check if returned indices are correct
                testGetElementIndices 1 [-1,1,-1,1,-1,1] [1,3,5],
                testGetElementIndices 1 [-1,-1,-1,-1,-1,-1] [],

                -- test will check if returned rows are correct
                testGetPossibleRows 1 [-1,1,-1,-1] [[1,1,-1,-1],[-1,1,1,-1],[-1,1,-1,1]],

                -- test will check if returned indix for verified board is correct
                testGetIndexOfVerifiedBoard 1 [[0,0,1,1],[-1,1,-1,0],[1,0,0,1],[1,1,0,0]] [[1,1,-1,0]] 1 False (0),
                testGetIndexOfVerifiedBoard 1 [[0,-1,-1,1],[0,1,1,0],[1,0,0,1],[1,1,0,0]] [[0,-1,1,1]] 0 False (-1),

                -- test will check if returned board is correct
                testSolveByEliminatingWrongs 1 [[0,0,1,1],[-1,1,-1,0],[1,0,0,1],[1,1,0,0]] [[0,0,1,1],[0,1,-1,0],[1,0,0,1],[1,1,0,0]],

                -- test will check if returned board is correct
                testAvoidTripleThree [[0,0,1,1],[0,1,1,0],[1,0,-1,-1],[1,1,0,0]] [[0,0,1,1],[0,1,1,0],[1,0,0,-1],[1,1,0,0]]
            ]
        ,
        testGroup "Completing a row or a column" [
                -- row should be filled in first test but not in second test
                testTryToFill 1 [1,1,1,1,-1,-1,-1,-1] [1,1,1,1,0,0,0,0],
                testTryToFill 1 [1,1,1,-1,-1,-1,-1,-1] [1,1,1,-1,-1,-1,-1,-1],
                
                -- both rows and columns should be filled in if possible
                testCompletingRowOrColumn [[-1,0,1,-1],[0,-1,1,0],[1,0,0,-1],[1,1,-1,0]] [[0,0,1,1],[0,1,1,0],[1,0,0,1],[1,1,0,0]]
            ]
        ,
        testGroup "Avoiding row or column duplication" [
                -- row should be filled in first test but not in second test
                testCompareTwoRows [1,-1,-1,1] [1,0,1,1] [1,1,0,1],
                testCompareTwoRows [1,-1,-1,1] [1,0,1,0] [1,-1,-1,1],

                -- row should be filled in first test but not in second test
                testCompareRowAgainstRows [1,-1,-1,1] [[1,0,1,1],[1,0,1,0]] [1,1,0,1],
                testCompareRowAgainstRows [1,-1,-1,1] [[1,0,1,0],[0,0,1,1]] [1,-1,-1,1],

                -- should return one row in the first test but not any in second test
                testGetCompleteRows [[1,-1,-1,1],[1,0,1,0],[0,0,-1,1]] [[1,0,1,0]],
                testGetCompleteRows [[1,-1,-1,1],[1,-1,1,0],[0,0,-1,1]] [],
                
                -- first test should return a complete board but not the second 
                testAvoidRowDuplicates [[0,-1,1,-1],[0,1,1,0],[1,-1,0,-1],[1,1,0,0]] [[0,0,1,1],[0,1,1,0],[1,0,0,1],[1,1,0,0]],
                testAvoidRowDuplicates [[-1,0,1,-1],[0,-1,1,0],[1,-1,-1,1],[-1,1,0,0]] [[-1,0,1,-1],[0,-1,1,0],[1,-1,-1,1],[-1,1,0,0]],

                -- test should return a complete board, it's solved by looking at columns
                testAvoidingRowOrColumnDuplication [[0,0,1,1],[-1,1,-1,0],[-1,0,-1,1],[1,1,0,0]] [[0,0,1,1],[0,1,1,0],[1,0,0,1],[1,1,0,0]]
            ]
        ,
        testGroup "Advanced technique 1" [
                -- test will find a one right value by making a mistake, row-wise
                testSolveByEliminatingWrongs2 1 [[-1,-1,-1,-1],[0,1,1,0],[1,0,0,1],[1,1,0,0]] [[0,-1,-1,-1],[0,1,1,0],[1,0,0,1],[1,1,0,0]],

                -- test solver in both row and columns
                testAdvancedTechniqueOne [[0,-1,1,1],[0,-1,1,0],[1,-1,0,1],[1,-1,0,0]] [[0,0,1,1],[0,-1,1,0],[1,-1,0,1],[1,-1,0,0]]
            ]
        ,
        testGroup "Advanced technique 2" [
                -- test solver in both row and columns
                testAdvancedTechniqueTwo [[0,-1,-1,1],[0,1,1,0],[1,0,0,1],[1,1,0,0]] [[0,0,-1,1],[0,1,1,0],[1,0,0,1],[1,1,0,0]]
            ]
    ]

main :: IO ()
main = defaultMain allTests
