import random

from z4 import opt_dist


def init_matrix(l1, l2):
    return tuple(tuple(random.randint(0, 1) for _ in len(l1)) for _ in len(l2))


def get_column(i, matrix):
    return [matrix[j][i] for j in range(len(matrix))]


def iter_columns(matrix):
    for row in matrix[:1]:
        return (get_column(i, matrix) for i in range(len(row)))


class LogPics:
    def __init__(self, row_c, col_c):
        self.row_c = row_c
        self.col_c = col_c
        self.matrix = init_matrix(row_c, col_c)

    def get_wrong_rows(self):
        return [(i, row) for i, row in enumerate(self.matrix)
                if opt_dist(row, self.row_c[i])]

    def get_wrong_cols(self):
        return [(i, col) for i, col in enumerate(iter_columns(self.matrix))
                if opt_dist(col, self.col_c[i])]

    def solve(self):
        while True:
            rows = self.get_wrong_rows()
            cols = self.get_wrong_cols()
            if rows:
                ...
            else 



if __name__ == '__main__':

    # r_c = [1, 1]
    # c_c = [1, 1]
    # matrix = [[1, 0], [0, 1]]
    # print(is_done(matrix, r_c, c_c))

    # matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    # print(*iter_columns(matrix))
