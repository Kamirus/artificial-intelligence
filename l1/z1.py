from itertools import chain
from collections import deque
from sys import argv, setrecursionlimit

setrecursionlimit(5000)


def valid_pos(pos):
    x, y = pos
    return 1 <= x <= 8 and 1 <= y <= 8


def set_ith(seq, i, val):
    return seq.__class__(val if i == j else x
                         for j, x in enumerate(seq))


def _king_moves(state, i): return _moves(state, i, _king_all_moves)


def white_king_moves(state): return _king_moves(state, 0)


def tower_moves(state): return _moves(state, 1, _tower_all_moves(state))


def black_king_moves(state): return _king_moves(state, 2)


def white_attacked_positions(state):
    state_without_black = set_ith(state, 2, (0, 0))
    return chain((s[0] for s in white_king_moves(state_without_black)),
                 (s[1] for s in tower_moves(state_without_black)))


def black_attacked_positions(state):
    state_without_white = ((0, 0), (0, 0), state[2])
    return (s[2] for s in black_king_moves(state_without_white))


def _moves(state, i, all_moves_func):
    x, y = state[i]
    alls = all_moves_func(x, y)
    moves = (pos for pos in alls
             if pos not in state  # move somewhere and dont intersect
             and valid_pos(pos))
    return (set_ith(state, i, pos) for pos in moves)


def _king_all_moves(x, y):
    return ((x + i, y + j) for i in range(-1, 2) for j in range(-1, 2))


def _tower_all_moves(state):
    x, y = state[1]

    def one_dir(mx, my):
        for pos in ((x + mx * k, y + my * k)
                    for k in range(1, 8)):
            if pos in state or not valid_pos(pos):
                break
            yield pos

    def aux(*_):
        yield from one_dir(-1, 0)  # left moves
        yield from one_dir(1, 0)  # right moves
        yield from one_dir(0, 1)  # up moves
        yield from one_dir(0, -1)  # down moves
    return aux


def white_possible_moves(state):
    black_attacked = set(black_attacked_positions(state))
    return chain((s for s in white_king_moves(state) if s[0] not in black_attacked),
                 (s for s in tower_moves(state) if s[1] not in black_attacked))


def black_possible_moves(state):
    black_attacked = set(black_attacked_positions(state))
    if state[0] in black_attacked or state[1] in black_attacked:
        raise RuntimeError('black attacks sth')

    white_attacked = set(white_attacked_positions(state))
    return (s for s in black_king_moves(state) if s[2] not in white_attacked)


def all_moves_from(state, is_white_turn=True):
    if is_white_turn:
        return white_possible_moves(state)
    else:
        return black_possible_moves(state)


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


def is_mat(state):
    state_without_black = set_ith(state, 2, (0, 0))
    white_attacked_positions = set(chain(
        (s[0] for s in white_king_moves(state_without_black)),
        (s[1] for s in tower_moves(state_without_black)),
    ))
    return all(black_state[2] in white_attacked_positions
               for black_state in chain(black_king_moves(state), [state]))


def path(state, predecessor):
    before = predecessor.get(state)
    if before is not None:
        yield from path(before, predecessor)
    yield state


def min_mat(state, is_white_turn=True, debug=False):
    def aux(state, is_white_turn):
        predecessor = {}
        q = deque([(state, is_white_turn)])
        predecessor[state] = None
        while q:
            state, is_white_turn = q.pop()
            try:
                moves = list(all_moves_from(
                    state, is_white_turn=is_white_turn))
            except RuntimeError:  # black attacks, drop state
                continue
            if not moves:
                return state, predecessor
            for next_state in moves:
                if next_state not in predecessor:
                    predecessor[next_state] = state
                    q.appendleft((next_state, not is_white_turn))
        raise ValueError('ended without mat')

    win, pred = aux(state, is_white_turn)
    min_path = list(path(win, pred))
    if debug:
        for s in min_path:
            draw(s)
    return len(min_path) - 1


def parse_line(line):
    turn, *str_positions = line.split()
    return turn == 'white', tuple(load_position(pos) for pos in str_positions)


def main(debug=False):
    with open('test_z1.in') as f:
        for line in f:
            line = line.strip()
            is_white_turn, state = parse_line(line)
            print(min_mat(state, is_white_turn, debug))


if __name__ == '__main__':
    main(debug=True)
