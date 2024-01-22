#!/bin/bash

mkdir -p tested
touch test-results.csv
for ITERATION in 1 2 3 4 5 6 7 8 9 10; do
    for SIZE in 10 20 30; do
        for LOOPFRAC in 0 0.2 0.5 1; do
            # Build a program using those parameters.
            stack run $SIZE $LOOPFRAC
            # Make a backup of the program.
            cp "generated/Test0.hs" "tested/Test-${$ITERATION}-${$SIZE}-${$LOOPFRAC}.hs"
            # Benchmark!
            stack bench
            # Save the CSV for later.
            cat "tests.csv" | tail -n 2 >> test-results.csv
        done
    done
done