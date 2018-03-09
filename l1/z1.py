from collections import deque
from sys import argv


def valid_pos(pos):
    x, y = pos
    return 1 <= x <= 8 and 1 <= y <= 8


def set_ith(seq, i, val):
    return seq.__class__(val if i == j else x
                         for j, x in enumerate(seq))


def king_moves(state, i):
    assert i in (0, 2), f'not valid king index: {i}'
    return _moves(state, i, _king_all_moves)


def tower_moves(state, i):
    assert i == 1, f'not valid tower index: {i}'
    return _moves(state, i, _tower_all_moves)


def _moves(state, i, all_moves_func):
    x, y = state[i]
    alls = all_moves_func(x, y)
    moves = (pos for pos in alls
             if pos not in state  # move somewhere and dont intersect
             and valid_pos(pos))
    return (set_ith(state, i, pos) for pos in moves)


def _king_all_moves(x, y):
    return ((x + i, y + j) for i in range(-1, 2) for j in range(-1, 2))


def _tower_all_moves(x, y):
    yield from ((x + i, y) for i in range(-x + 1, 9 - x))
    yield from ((x, y + j) for j in range(-y + 1, 9 - y))


def all_moves_from(state, white_turn=True):
    if white_turn:
        yield from king_moves(state, 0)
        yield from tower_moves(state, 1)
    else:
        yield from king_moves(state, 2)


def load_position(pos):
    x, y = pos
    return ord(x) - ord('a') + 1, int(y)


def dump_position(pos):
    x, y = pos
    return chr(ord('a') + x - 1) + str(y)


def draw(state):
    state = tuple(state)
    marks = ['W', 'T', 'B']

    def mark(pos):
        return marks[state.index(pos)] if pos in state else '.'

    table = ((mark((x, y)) for x in range(1, 9)) for y in range(8, 0, -1))
    for row in table:
        print(*row, sep=' ')
    print()


s = [(1, 1), (2, 2), (3, 3)]
draw(s)
for state in all_moves_from(s):
    draw(state)


def is_mat(state):
    pass


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
