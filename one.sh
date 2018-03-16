#!/bin/bash

validator=ai_validator/lista2/validator.py
zad=zad1
input=l2z1.py
case=10

for j in $(seq 1 3); do
    for i in 0 1 2; do
        python $validator --stdio --cases $case $zad python3.6 ai/$input | grep -E "Running case" &
    done

    wait
    # for i in 0 1 2; do wait ${pid[$i]}; done
done
