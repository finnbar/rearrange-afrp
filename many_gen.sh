#!/bin/bash

mkdir -p corpus
for ITERATION in 1 2 3 4 5 6 7 8 9 10; do
    for SIZE in 10 20 30 40; do
        for LOOPFRAC in 0 0.2 0.5 1; do
            # Build a program using those parameters.
            stack run $SIZE $LOOPFRAC
            # Save that program.
            cp "generated/Test0.hs" "corpus/Test-$ITERATION-$SIZE-$LOOPFRAC.hs"
       done
    done
done
