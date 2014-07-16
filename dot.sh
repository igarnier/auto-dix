#!/bin/sh

for I in *.dot; do
    dot -Tps $I > $I.ps
done