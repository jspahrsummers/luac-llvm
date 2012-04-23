#!/bin/bash

echo "*** Compiling compiler"
ghc -e 'main' main.hs

echo "*** Running compiler on sample input file"
llc -O0 -o output.S output.ll && clang -fcatch-undefined-behavior -ftrapv -O0 -llua -o output output.S

echo "*** Running sample program"
./output
