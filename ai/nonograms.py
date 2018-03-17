import random

from functools import lru_cache
from util import get_columns, init_random_matrix, neg_kth, memo
from typing import Callable, Tuple


class SimpleNonogram:
    def __init__(self, row_c, col_c):
        # rows + columns
        self.row_c = row_c
        self.col_c = col_c
        self.cost_func = opt_dist
        self._reinitialize()

    def solve(self, max_tries=None, print_all=False):
        first_max_tries = (self.n * self.m) ** 2
        max_tries = max_tries or first_max_tries

        for _ in range(max_tries):
            print_all and self.print_matrix()
            try:
                wrongs = self._get_wrongs()  # indexes of wrong rows and columns
                i = random.choice(wrongs)
            except IndexError:
                return self  # done
            else:
                if random.random() < 0.21 and len(wrongs) < len(self.matrix):
                    # break ok ones
                    oks = [k for k, _ in enumerate(self.matrix)
                           if k not in wrongs]
                    i = random.choice(oks)
                    j = random.randrange(len(self.matrix[i])) \
                        + (self.n if i < self.n else 0)
                    # _, j = random.choice(list(self._iter_cost_j(i)))
                    # _, j = max(self._iter_cost_j(i))
                else:
                    _, j = min(self._iter_cost_j(i))
                self._neg(i, j)
        self._reinitialize()
        # next_max_tries = len(self.matrix) ** 2 + max_tries
        # next_max_tries = max_tries + first_max_tries
        print_all and print('\n', '-' * 10, 'reinitialize', '-' * 10)
        next_max_tries = max_tries
        return self.solve(max_tries=next_max_tries, print_all=print_all)

    def print_matrix(self):
        print(*(''.join('#' if x else '.' for x in row)
                for row in self.matrix[:self.n]), sep='\n', end='\n\n')

    def _get_wrongs(self):
        return [i for i, s in enumerate(self.matrix) if self.cost_func(tuple(s), self.reqs[i])]

    def _iter_cost_j(self, i):
        if i < self.n:  # i = row index
            j0, n = self.n, self.m  # then j0 = first column index
        else:  # i = column index
            j0, n = 0, self.n

        for off in range(n):
            j = j0 + off
            neg_seq_i = tuple(neg_kth(self.matrix[i], off))
            neg_seq_j = tuple(neg_kth(self.matrix[j], i % n))
            sum = self.cost_func(neg_seq_i, self.reqs[i]) \
                + self.cost_func(neg_seq_j, self.reqs[j])
            yield sum, j

    def _neg(self, i, j):
        if not (0 <= i < self.n):  # i != row index
            i, j = j, i
        assert 0 <= i < self.n and self.n <= j < len(
            self.matrix), "one should be row index, 2nd col index"
        self.matrix[i] = list(neg_kth(self.matrix[i], j - self.n))
        self.matrix[j] = list(neg_kth(self.matrix[j], i))

    def _reinitialize(self):
        self.matrix = init_random_matrix(self.row_c, self.col_c)
        self.n = len(self.matrix)  # number of rows
        self.matrix.extend(get_columns(self.matrix))
        self.m = len(self.matrix) - self.n  # number of columns
        self.reqs = self.row_c + self.col_c


def opt_dist(s, k):  # ones_zeros_str, ones_len
    # 10110
    # k=3
    #   233
    prefix_sums = [0]  # prefix_sums[i], sum of first i elements
    for i, x in enumerate(s):
        prefix_sums.append(x + prefix_sums[i])

    best_k_elem_sum = max(prefix_sums[i + k] - prefix_sums[i]
                          for i in range(len(prefix_sums) - k))
    return prefix_sums[-1] + k - best_k_elem_sum * 2
    # best_sum = max(sum(int(x) for x in s[i:i + k])
    #                for i in range(len(s) - k + 1))  # all substr beginnings
    # return sum(map(int, s)) - best_sum + k - best_sum


@memo
def _dp_opt_dist_2d(seq, ks, start, i_ki, sep0s):
    assert i_ki < len(ks)
    assert sep0s >= 0

    if start >= len(seq):
        return None

    def _dp():
        ki = ks[i_ki]
        for off in range(sep0s + 1):
            if i_ki == len(ks) - 1:  # last one, take all
                yield opt_dist(seq[start:], ki)
            else:
                end = start + ki + off
                rec = _dp_opt_dist_2d(seq, ks, end + 1, i_ki + 1, sep0s - off)
                if rec is not None:
                    s = seq[start:end]
                    sep_cost = int(seq[end])
                    yield opt_dist(s, ki) + sep_cost + rec
    return min(_dp())


def opt_dist_2d(seq, ks):
    """
    calculate cost of changing [seq] ({0,1}*)
    into 0* 1^k1 0* ... 1^kn 0*, where ks={k1..kn}
    """
    # all chars - required 1s - minimum 0 separators
    additional_0s = len(seq) - sum(ks) - (len(ks) - 1)
    return _dp_opt_dist_2d(seq, ks, 0, 0, additional_0s)


class Nonogram(SimpleNonogram):
    def __init__(self, row_c, col_c):
        super().__init__(row_c, col_c)
        self.cost_func = opt_dist_2d


# if __name__ == '__main__':
#     assert opt_dist_2d('0000', [1, 2]) == 3
#     assert opt_dist_2d('0100', [1, 2]) == 4
#     assert opt_dist_2d('00000000', [1, 2, 3]) == 6
#     assert opt_dist_2d('10110111', [1, 2, 3]) == 0
#     assert opt_dist_2d('000000000', [1, 2, 3]) == 6
#     x = opt_dist_2d('011100000', [1, 2, 3])
#     print(x)
#     print('ok')
