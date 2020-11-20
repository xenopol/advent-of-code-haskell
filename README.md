# advent-of-code-haskell

Solve Advent of Code puzzles with Haskell

## Building

    stack build

## Running a day

    cd src/2015/01
    stack runghc Day01.hs < input.txt
    echo -n "()" | stack runghc 1.hs

## Running a test

The tests are from the example of the day

    stack runghc Test.hs
