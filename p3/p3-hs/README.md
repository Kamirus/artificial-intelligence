# Nonogram solver

[What nonograms are?](https://en.wikipedia.org/wiki/Nonogram)

[Try to solve it yourself](https://www.puzzle-nonograms.com/)


## Build and run

`stack build`

`cat <input_file> | stack exec main`


## Input description

First line contains: `<number_of_rows> <number_of_columns>`

Next `<number_of_rows>` lines describe each row - block lengths separated by spaces

Same for columns 

**Example**:
```
20 15
3
1 2
1 4
1 1 2
1 1 1 1
1 3 2
2 3 1
1 1 1 2
2 2 2
1 1 2 2
1 1 2 2
1 1 1 1
4 1 1
2 2 2 1
2 3 3
2 2 3
1 3 1 1
2 1 1 1 2
1 2 3
1 6
4 3
6 1 2 3
2 3
6
1 2 2
1 1 2
2 4 1 1
1 1 2 2 2 1
1 1 1 2 1 1
1 3 2 3
3 2 2
4 3 4 2
1 3 4 5
2 2
3
```
