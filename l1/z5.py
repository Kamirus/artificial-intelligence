import random

from z4 import opt_dist


def init_matrix(l1, l2):
    return [[random.randint(0, 1) for _ in range(len(l1))] for _ in range(len(l2))]


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
        self.reinitialize()

    def reinitialize(self):
        self.matrix = init_matrix(self.row_c, self.col_c)
        self.n = len(self.matrix)
        self.matrix.extend(get_columns(self.matrix))
        self.reqs = self.row_c + self.col_c

    def get_wrongs(self):
        return [i for i, s in enumerate(self.matrix) if opt_dist(s, self.reqs[i])]

    def iter_cost_j(self, i):
        n = self.n
        j0 = n if i < n else 0  # if row then 1st col else 1st row
        for off in range(n):
            j = j0 + off
            neg_seq_i = neg_kth(self.matrix[i], off)
            neg_seq_j = neg_kth(self.matrix[j], i % n)
            sum = opt_dist(neg_seq_i, self.reqs[i]) \
                + opt_dist(neg_seq_j, self.reqs[j])
            yield sum, j

    def neg(self, i, j):
        n = self.n
        self.matrix[i][j % n] = int(not self.matrix[i][j % n])
        self.matrix[j][i % n] = int(not self.matrix[j][i % n])

    def solve(self, max_tries=100, print_all=False):
        for _ in range(max_tries):
            try:
                wrongs = self.get_wrongs()
                i = random.choice(wrongs)
            except IndexError:
                return self  # done
            else:
                if len(wrongs) < self.n and random.random() < 0.0001:
                    # break ok ones
                    ok_rows = [x for x in range(self.n) if x not in wrongs]
                    self.neg(random.choice(ok_rows), random.choice(range(self.n)))
                else:
                    _, j = min(self.iter_cost_j(i))
                    self.neg(i, j)
                print_all and self.print_matrix()
        self.reinitialize()
        return self.solve(max_tries=10 * max_tries, print_all=print_all)

    def print_matrix(self):
        print(*(''.join('#' if x else '.' for x in s)
                for s in self.matrix[:self.n]), sep='\n', end='\n\n')


if __name__ == '__main__':
    for r, c in [
        # ([7, 7, 7, 7, 7, 7, 7], [7, 7, 7, 7, 7, 7, 7]),
        # ([2, 2, 7, 7, 2, 2, 2], [2, 2, 7, 7, 2, 2, 2]),
        ([2, 2, 7, 7, 2, 2, 2], [4, 4, 2, 2, 2, 5, 5]),
        # ([7, 6, 5, 4, 3, 2, 1], [1, 2, 3, 4, 5, 6, 7]),
        # ([7, 5, 3, 1, 1, 1, 1], [1, 2, 3, 7, 3, 2, 1])
    ]:
        LogPics(r, c).solve(print_all=True).print_matrix()

    # matrix = init_matrix(row_c, col_c)
    # print(matrix)
    # matrix.extend(get_columns(matrix))
    # print(matrix)

    # matrix = [[1, 0], [0, 1]]
    # print(is_done(matrix, r_c, c_c))

    # matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    # print(*get_columns(matrix))
