#!/bin/sh

make profile

./main 10 0.8 10000

gprof ./main | ../gprof2dot.py | dot -Tpng -o output.png
