import functools
import queue
import random

from typing import Iterable, Tuple

# 2D matrix


def init_random_matrix(rows, cols):
    return [[random.randint(0, 1) for _ in range(len(cols))] for _ in range(len(rows))]


def get_column(i, matrix):
    return [matrix[j][i] for j in range(len(matrix))]


def get_columns(matrix):
    return [get_column(i, matrix) for i in range(len(matrix[0]))]


# sequence
def neg_kth(seq: Iterable[int], index: int) -> Iterable[int]:
    return (int(not x) if index == i else x
            for i, x in enumerate(seq))


# funcs
def memo(f):
    m = {}

    @functools.wraps(f)
    def aux(*args):
        if args not in m:
            m[args] = f(*args)
        return m[args]

    return aux


# data structures
class PQueue():
    """
    Hides priority of queue.PriorityQueue as optional parameter
    Elements of equal priority in FIFO order
    """

    def __init__(self) -> None:
        self.cnt = 0
        self.q = queue.PriorityQueue()

    def push(self, item, priority=0):
        self.q.put((priority, self.cnt, item))
        self.cnt += 1

    def pop(self):
        _, _, item = self.q.get()
        return item
