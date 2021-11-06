#!/bin/bash

mkdir -p output
mkdir -p log
rm -rf output/$2
dotnet ../build/Smartian.dll fuzz \
  -t $1 -p bc/$2.bin -a abi/$2.abi -v 1 -o output/$2 --useothersoracle \
  > log/$2.txt 2>&1
