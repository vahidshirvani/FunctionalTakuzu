#! /usr/bin/env sh

### first test ###
function test_Takuzu6x6 () {
    cd mainTests/01/
    > result.txt  ## clear old test results
    cat input.txt | ../../dist/build/tic-tac-logic/tic-tac-logic > result.txt
    diff ./output.txt ./result.txt
    assertTrue '6x6 example from project description fails' $?
}

## Call and Run all Tests
. "shunit2"