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

testAvoidTripleTree :: Board -> Board -> TestTree
testAvoidTripleTree input output = testCase
    "Testing 1 Board that can be filled up"
    (avoidTripleThree input @?= output)

--
-- Completing a row or a column
--

testTryToFill :: Cell -> Row -> Row -> TestTree
testTryToFill t input output = testCase
    "Testing 1 Row with four empty cells (1,1,1,1,-1,-1,-1,-1)"
    (tryToFill t input @?= output)

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
                --testAvoidTripleTree [[0,1,-1,0,-1,0,1,0]] [[0,1,1,0,1,0,1,0]]
            ]
        ,
        testGroup "Completing a row or a column" [
                testTryToFill 1 [1,1,1,1,-1,-1,-1,-1] [1,1,1,1,0,0,0,0]
            ]
    ]

main :: IO ()
main = defaultMain allTests
