#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys


def V(i,j):
    return 'V%d_%d' % (i,j)
    
def domains(Vs):
    return [ q + ' in 1..9' for q in Vs ]
    
def all_different(Qs):
    return 'all_distinct([' + ', '.join(Qs) + '])'
    
def get_column(j):
    return [V(i,j) for i in range(9)] 
            
def get_row(i):
    return [V(i,j) for j in range(9)] 

def get_block(k):
    a, b = k // 3 * 3, k % 3 * 3
    return [V(a + oi, b + oj) for oi in range(3) for oj in range(3)]

def horizontal():   
    return [all_different(get_row(i)) for i in range(9)]

def vertical():
    return [all_different(get_column(j)) for j in range(9)]

def blocks():
    return [all_different(get_block(k)) for k in range(9)]


def print_constraints(Cs, indent, d):
    position = indent
    print (indent - 1) * ' ',
    for c in Cs:
        print c + ',',
        position += len(c)
        if position > d:
            position = indent
            print
            print (indent - 1) * ' ',

      
def sudoku(assigments):
    variables = [ V(i,j) for i in range(9) for j in range(9)]
    
    print ':- use_module(library(clpfd)).'
    print 'solve([' + ', '.join(variables) + ']) :- '
    
    
    cs = domains(variables) + vertical() + horizontal() + blocks() #TODO: too weak contraints, add something!
    for i,j,val in assigments:
        cs.append( '%s #= %d' % (V(i,j), val) )
    
    print_constraints(cs, 4, 70),
    print
    print '    labeling([ff], [' +  ', '.join(variables) + ']).' 
    print 
    print ':- solve(X), write(X), nl.'       

if __name__ == "__main__":
    raw = 0
    triples = []
    
    for x in sys.stdin:
        x = x.strip()
        if len(x) == 9:
            for i in range(9):
                if x[i] != '.':
                    triples.append( (raw,i,int(x[i])) ) 
            raw += 1          
    sudoku(triples)
    
"""
89.356.1.
3...1.49.
....2985.
9.7.6432.
.........
.6389.1.4
.3298....
.78.4....
.5.637.48

53..7....
6..195...
.98....6.
8...6...3
4..8.3..1
7...2...6
.6....28.
...419..5
....8..79

3.......1
4..386...
.....1.4.
6.924..3.
..3......
......719
........6
2.7...3..
"""    
