#!/bin/bash

echo "*** Compiling compiler and running on sample input file"
ghc -e 'main' main.hs

if [ $? -ne 0 ]
then
    exit 1
fi

echo "*** Compiling generated LLVM assembly"
llc -O0 -o output.S output.ll && clang -fcatch-undefined-behavior -ftrapv -O0 -llua -o output output.S

if [ $? -ne 0 ]
then
    exit 1
fi

echo "*** Running sample program"
./output
