from collections import deque
from sys import argv


def load_position(pos):
    x, y = pos
    return ord(x) - ord('a') + 1, int(y)


def dump_position(pos):
    x, y = pos
    return chr(ord('a') + x - 1) + str(y)


def is_mat(state):



def min_mat(line):
    color, *positions = line.split()
    state = tuple(load_position(pos) for pos in positions)
    states = deque([state])
    while states:
        state = states.popleft()
        if is_mat(state):
            return state
        make_possible_moves


# def main():
#     filename = argv[1]
#     with open(filename) as f:
#         for line in f:
#
