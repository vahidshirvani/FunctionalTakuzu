module Main where

import TicTacLogic
import Test.Tasty
import Test.Tasty.HUnit

--
-- Avoiding triples 1
--

testFillOppositeAfterPairs :: Row -> Row -> TestTree
testFillOppositeAfterPairs input output = testCase "Testing 1 Row with two pairs X X -1" ((applyRowFnBidirectional avoidTripleForward input) @?= output)

testAvoidTripleOne :: Board -> Board -> TestTree
testAvoidTripleOne input output = testCase "Testing 1 Board with pairs X X -1 in both directions" ((avoidTripleOne input) @?= output)

--
-- Aggregate all tests and run
--

allTests :: TestTree
allTests = testGroup "TicTacLogicTests" [
        testGroup "Avoiding triples 1" [
                testFillOppositeAfterPairs [1,1,-1] [1,1,0],
                testFillOppositeAfterPairs [0,0,-1] [0,0,1],
                testAvoidTripleOne
                    [[-1,1,-1,-1,-1,-1],[0,0,-1,-1,1,1],[0,1,0,1,0,1],[-1,1,-1,-1,-1,-1]]
                    [[1,1,-1,-1,-1,0],[0,0,1,0,1,1],[0,1,0,1,0,1],[1,1,-1,-1,-1,0]]
            ]
    ]

main :: IO ()
main = defaultMain allTests