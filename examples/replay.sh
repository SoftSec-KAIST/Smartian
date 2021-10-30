#!/bin/bash

echo "Replaying test cases that increased coverage"
dotnet ../build/Smartian.dll replay -p bc/$1.bin -i ./output/$1/testcase/ -t 1
echo "Replaying test cases that triggered bugs"
dotnet ../build/Smartian.dll replay -p bc/$1.bin -i ./output/$1/bug/ \
  --useothersoracle
