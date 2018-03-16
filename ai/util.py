import random

# 2D matrix
def init_random_matrix(rows, cols):
    return [[random.randint(0, 1) for _ in range(len(cols))] for _ in range(len(rows))]


def get_column(i, matrix):
    return [matrix[j][i] for j in range(len(matrix))]


def get_columns(matrix):
    return [get_column(i, matrix) for i in range(len(matrix[0]))]


# sequence
def neg_kth(seq, index):
    s = seq[:]
    s[index] = int(not s[index])
    return s


# funcs
def memo_one_arg(f):
    m = {}

    def aux(x):
        if x not in m:
            m[x] = f(x)
        return m[x]

    return aux
