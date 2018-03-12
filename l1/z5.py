import random

from z4 import opt_dist


def init_matrix(rows, cols):
    return [[random.randint(0, 1) for _ in range(len(cols))] for _ in range(len(rows))]


def get_column(i, matrix):
    return [matrix[j][i] for j in range(len(matrix))]


def get_columns(matrix):
    return [get_column(i, matrix) for i in range(len(matrix[0]))]


def neg_kth(seq, index):
    s = seq[:]
    s[index] = int(not s[index])
    return s


class LogPics:
    def __init__(self, row_c, col_c):
        # rows + columns
        self.row_c = row_c
        self.col_c = col_c
        self._reinitialize()

    def solve(self, max_tries=100, print_all=False):
        for _ in range(max_tries):
            print_all and self.print_matrix()
            try:
                wrongs = self._get_wrongs()  # indexes of wrong rows and columns
                i = random.choice(wrongs)
            except IndexError:
                return self  # done
            else:
                if random.random() < 0.01 and len(wrongs) < len(self.matrix):
                    # break ok ones
                    oks = [k for k, _ in enumerate(self.matrix)
                           if k not in wrongs]
                    i = random.choice(oks)
                    # _, j = random.choice(list(self._iter_cost_j(i)))
                    _, j = max(self._iter_cost_j(i))
                else:
                    _, j = min(self._iter_cost_j(i))
                self._neg(i, j)
        self._reinitialize()
        return self.solve(max_tries=len(self.matrix) ** 2 + max_tries, print_all=print_all)

    def print_matrix(self):
        print(*(''.join('#' if x else '.' for x in row)
                for row in self.matrix[:self.n]), sep='\n', end='\n\n')

    def _get_wrongs(self):
        return [i for i, s in enumerate(self.matrix) if opt_dist(s, self.reqs[i])]

    def _iter_cost_j(self, i):
        if i < self.n:  # i = row index
            j0, n = self.n, self.m  # then j0 = first column index
        else:  # i = column index
            j0, n = 0, self.n

        for off in range(n):
            j = j0 + off
            neg_seq_i = neg_kth(self.matrix[i], off)
            neg_seq_j = neg_kth(self.matrix[j], i % n)
            sum = opt_dist(neg_seq_i, self.reqs[i]) \
                + opt_dist(neg_seq_j, self.reqs[j])
            yield sum, j

    def _neg(self, i, j):
        if not (0 <= i < self.n):  # i != row index
            i, j = j, i
        assert 0 <= i < self.n and self.n <= j < len(
            self.matrix), "one should be row index, 2nd col index"
        self.matrix[i] = neg_kth(self.matrix[i], j - self.n)
        self.matrix[j] = neg_kth(self.matrix[j], i)

    def _reinitialize(self):
        self.matrix = init_matrix(self.row_c, self.col_c)
        self.n = len(self.matrix)  # number of rows
        self.matrix.extend(get_columns(self.matrix))
        self.m = len(self.matrix) - self.n  # number of columns
        self.reqs = self.row_c + self.col_c


if __name__ == '__main__':
    for r, c in [
        ([7, 7, 7, 7, 7, 7, 7], [7, 7, 7, 7, 7, 7, 7]),
        ([2, 2, 7, 7, 2, 2, 2], [2, 2, 7, 7, 2, 2, 2]),
        ([2, 2, 7, 7, 2, 2, 2], [4, 4, 2, 2, 2, 5, 5]),
        ([7, 6, 5, 4, 3, 2, 1], [1, 2, 3, 4, 5, 6, 7]),
        ([7, 5, 3, 1, 1, 1, 1], [1, 2, 3, 7, 3, 2, 1]),
        # ([1, 1, 1, 1], [2, 2]),
        # ([2, 3], [1, 1, 1, 1, 1]),
        # ([3, 2, 3, 4, 2], [1, 2, 3, 1, 1, 2, 2, 1, 1]),
        # ([3, 3, 3, 4, 2], [2, 2, 3, 1, 1, 2, 2, 1, 1]),
    ]:
        LogPics(r, c).solve(print_all=0).print_matrix()
