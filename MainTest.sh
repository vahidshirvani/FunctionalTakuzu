#! /usr/bin/env sh

function test_Takuzu2x2 () {
    > ./mainTests/2x2/result.txt  ## clear old test results
    cat ./mainTests/2x2/input.txt | ./dist/build/tic-tac-logic/tic-tac-logic > ./mainTests/2x2/result.txt
    diff ./mainTests/2x2/output.txt ./mainTests/2x2/result.txt
    assertTrue '2x2 example from project description fails' $?
}

function test_Takuzu4x4 () {
    > ./mainTests/4x4/result.txt  ## clear old test results
    cat ./mainTests/4x4/input.txt | ./dist/build/tic-tac-logic/tic-tac-logic > ./mainTests/4x4/result.txt
    diff ./mainTests/4x4/output.txt ./mainTests/4x4/result.txt
    assertTrue '4x4 example from project description fails' $?
}

function test_Takuzu6x6 () {
    > ./mainTests/6x6/result.txt  ## clear old test results
    cat ./mainTests/6x6/input.txt | ./dist/build/tic-tac-logic/tic-tac-logic > ./mainTests/6x6/result.txt
    diff ./mainTests/6x6/output.txt ./mainTests/6x6/result.txt
    assertTrue '6x6 example from project description fails' $?
}

### first test ###
function test_Takuzu8x8 () {
    > ./mainTests/8x8/result.txt  ## clear old test results
    cat ./mainTests/8x8/input.txt | ./dist/build/tic-tac-logic/tic-tac-logic > ./mainTests/8x8/result.txt
    diff mainTests/8x8/output.txt mainTests/8x8/result.txt
    assertTrue '8x8 example from project description fails' $?
}

## Call and Run all Tests
. "shunit2"