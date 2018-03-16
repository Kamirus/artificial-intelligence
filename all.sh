#!/bin/bash

MAX=12
validator=ai_validator/lista2/validator.py
zad=zad1
input=l2z1.py

for i in $(seq 1 $MAX); do
    python $validator --stdio --cases $i $zad python3.6 ai/$input | grep -E "Running case" &
done;
