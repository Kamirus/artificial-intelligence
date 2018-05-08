import random
import sys
from math import inf
from collections import defaultdict as dd
from turtle import *
from typing import List, Tuple, Optional, NamedTuple, Iterable
from collections import namedtuple


#####################################################
# turtle graphic
#####################################################
# tracer(0, 1)  # type: ignore

BOK = 30
SX = -100
SY = 0
M = 8


def kwadrat(x, y, kolor):
    fillcolor(kolor)
    pu()
    goto(SX + x * BOK, SY + y * BOK)
    pd()
    begin_fill()
    for i in range(4):
        fd(BOK)
        rt(90)
    end_fill()


def kolko(x, y, kolor):
    fillcolor(kolor)

    pu()
    goto(SX + x * BOK + BOK/2, SY + y * BOK - BOK)
    pd()
    begin_fill()
    circle(BOK/2)
    end_fill()

#####################################################


def initial_board():
    B = [[None] * M for i in range(M)]
    B[3][3] = 1
    B[4][4] = 1
    B[3][4] = 0
    B[4][3] = 0
    return B


dirs = [(0, 1), (1, 0), (-1, 0), (0, -1),
        (1, 1), (-1, -1), (1, -1), (-1, 1)]

p0 = 150  # 8
p1 = 50  # 5
p2 = 20  # 2
p3 = 50  # 1
p4 = 15  # 0.5
p5 = -5  # 0.2

weights = ((p0, p2, p1, p1, p1, p1, p2, p0),
           (p2, p2, p4, p4, p4, p4, p2, p2),
           (p1, p4, p3, p5, p5, p3, p4, p1),
           (p1, p4, p5, p3, p3, p5, p4, p1),
           (p1, p4, p5, p3, p3, p5, p4, p1),
           (p1, p4, p3, p5, p5, p3, p4, p1),
           (p2, p2, p4, p4, p4, p4, p2, p2),
           (p0, p2, p1, p1, p1, p1, p2, p0),)


def draw(board):
    for i in range(M):
        res = []
        for j in range(M):
            b = board[i][j]
            if b == None:
                res.append('.')
            elif b == 1:
                res.append('#')
            else:
                res.append('o')
        print(''.join(res))
    print()


def show(board):
    for i in range(M):
        for j in range(M):
            kwadrat(j, i, 'green')

    for i in range(M):
        for j in range(M):
            if board[i][j] == 1:
                kolko(j, M - 1 - i, 'black')
            if board[i][j] == 0:
                kolko(j, M - 1 - i, 'white')


State = namedtuple('State', ['board', 'fields', 'h', 'next', 'player'])


def get(board, x, y):
    if 0 <= x < M and 0 <= y < M:
        return board[x][y]
    return None


def next_states(state: State) -> Iterable[State]:
    next_board = [row[:] for row in state.board]
    next_fields = set(state.fields)
    next_h = state.h
    h_mul = 1 if state.player == 0 else -1
    for move in state.fields:
        x0, y0 = move
        to_fields = []
        for dx, dy in dirs:
            to_beat = []
            x, y = x0 + dx, y0 + dy
            if x < 0 or x >= M or y < 0 or y >= M:
                continue
            if next_board[x][y] is None:
                to_fields.append((x, y))
                continue
            while get(next_board, x, y) == 1 - state.player:
                to_beat.append((x, y))
                x += dx
                y += dy
            if to_beat and get(next_board, x, y) == state.player:
                for (nx, ny) in to_beat:
                    next_h += h_mul * 2 * weights[nx][ny]  # update h
                    next_board[nx][ny] = state.player
                next_fields.discard(move)  # valid move
        if move not in next_fields:
            # valid move
            next_board[x0][y0] = state.player
            next_fields.update(to_fields)
            new_state = State(next_board, next_fields, next_h +
                              h_mul * weights[x0][y0], [], 1 - state.player)
            state.next.append(new_state)
            # draw(next_board)
            yield new_state

            # clean for next move
            next_board = [row[:] for row in state.board]
            next_fields = set(state.fields)
            next_h = state.h


def result(board):
    res = 0
    for x in range(M):
        for y in range(M):
            b = board[x][y]
            if b == 0:
                res += 1
            elif b == 1:
                res -= 1
    return res


def init() -> State:
    board = initial_board()
    fields = {(2, 3), (2, 4),
              (3, 2), (3, 5),
              (4, 2), (4, 5),
              (5, 3), (5, 4), }
    return State(board, fields, 0, [], 0)


def update_next_states(state: State) -> None:
    if not state.next:
        # state.next.extend(next_states(state))
        state.next.extend(sorted_next_states(state))


def terminal(state):
    update_next_states(state)
    return not state.next


def max_value(state: State, alpha: float, beta: float, depth: int) -> Tuple[float, Optional[State]]:
    vstate = None
    if depth >= max_depth:
        return state.h, vstate

    update_next_states(state)

    if not state.next:
        return result(state.board), vstate

    value = -inf
    for s in state.next:
        v, _ = min_value(s, alpha, beta, depth + 1)
        if v > value:
            value, vstate = v, s  # value = max(value, )
        if value >= beta:
            return value, vstate
        alpha = max(alpha, value)
    return value, vstate


def min_value(state: State, alpha: float, beta: float, depth: int) -> Tuple[float, Optional[State]]:
    vstate = None
    if depth >= max_depth:
        return state.h, vstate

    update_next_states(state)

    if not state.next:
        return result(state.board), vstate

    value = inf
    for s in state.next:
        v, _ = max_value(s, alpha, beta, depth + 1)
        if v < value:
            value, vstate = v, s  # value = min(value, )
        if value <= alpha:
            return value, vstate
        beta = min(beta, value)
    return value, vstate


def random_move(state: State) -> State:
    return random.choice(state.next)


def agent_move(state: State):
    _, next_state = max_value(state, -inf, inf, 0)
    return next_state
    # return max(state.next, key=lambda s: min_value(s, -inf, inf, 0))


def agent_vs_random() -> int:
    state = init()
    while not terminal(state):
        # draw(state.board)
        if state.player:
            state = random_move(state)
        else:
            # state = random_move(state)
            state = agent_move(state)
    # draw(state.board)
    return result(state.board)


random.seed(42)

max_depth = 2


def sorted_next_states(state: State) -> Iterable[State]:
    # return next_states(state)
    return sorted(next_states(state), key=lambda s: s.h, reverse=state.player)
    # return sorted(next_states(state), key=lambda s: s.h, reverse=1-state.player)
    # return sorted(next_states(state), key=lambda s: s.h, reverse=True)
    # return sorted(next_states(state), key=lambda s: s.h)


def main():
    """
    depth2 935/1000 65loses 47s
    depth3 938/1000 62loses 2m25s
    depth4 98 /100   2loses 2m25s
    """
    N = 1000
    x = sum(agent_vs_random() > 0 for _ in range(N))
    print(f'{100 * x // N}%   {x}/{N}   {N - x} loses')


def pruning():
    """
    5depth; 100N
        no sort       -> 14m1s
        player sort   -> 11m52s
        1-player sort -> 12m10s

    seed 42
    5depth; 10N
        no       sort -> 1m40s; 1m44s
          player sort -> 1m6s ; 1m8s
        1-player sort -> 1m16s; 1m15s
        desc     sort -> 1m9s
        asc      sort -> 1m14s
    """
    global max_depth
    max_depth = 5
    N = 10
    print(f'pruning; {N} times; depth={max_depth}')
    x = sum(agent_vs_random() > 0 for _ in range(N))
    print(f'{100 * x // N}%   {x}/{N}   {N - x} loses')


if __name__ == '__main__':
    main()
    # pruning()
