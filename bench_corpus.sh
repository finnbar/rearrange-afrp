#!/bin/bash

touch test-results.csv
for t in corpus/Test-$i-*
do
    cp $t generated/Test0.hs
    # Benchmark!
    stack bench
    # Save the CSV for later.
    cat "tests.csv" | tail -n 2 >> test-results.csv
done
