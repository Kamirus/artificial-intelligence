import functools
import queue
import random

from typing import Iterable, Tuple, Generic, TypeVar, Any

T = TypeVar('T')

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
class PQueue(Generic[T]):
    """
    Hides priority of queue.PriorityQueue as optional parameter
    Elements of equal priority in FIFO order
    """

    def __init__(self) -> None:
        self.cnt = 0
        self.q = queue.PriorityQueue()

    def push(self, item: T, priority: Any=1) -> None:
        self.q.put((priority, self.cnt, item))
        self.cnt += 1

    def pop(self) -> T:
        _, _, item = self.q.get()
        return item

# heuristics


class Heuristics:
    @staticmethod
    def manhattan(pos1: Tuple[float, float], pos2: Tuple[float, float]) -> float:
        return abs(pos1[0] - pos2[0]) + abs(pos1[1] - pos2[1])

    @staticmethod
    def euclidean(pos1: Tuple[float, float], pos2: Tuple[float, float]) -> float:
        x = abs(pos1[0] - pos2[0])
        y = abs(pos1[1] - pos2[1])
        return (x * x + y * y) ** 0.5
