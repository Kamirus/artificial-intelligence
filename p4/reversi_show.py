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

p0 = 100  # 8
p1 = 70  # 5
p2 = 20  # 2
p3 = 40  # 1
p4 = 5  # 0.5
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


# def moves(state: State) -> List[Tuple[int, int, int]]:
#     return sorted((beats(state.board, x, y, state.player), x, y) for (x, y) in state.fields)


# def beats(board, x0, y0, player) -> int:
#     res = 0
#     for dir in dirs:
#         dx, dy = dir
#         x = x0 + dx
#         y = y0 + dy
#         cnt = 0
#         while get(board, x, y) == 1 - player:
#             x += dx
#             y += dy
#             cnt += 1
#         if get(board, x, y) == player:
#             res += cnt
#     return res


def get(board, x, y):
    if 0 <= x < M and 0 <= y < M:
        return board[x][y]
    return None


def sorted_next_states(state: State) -> List[State]:
    return sorted(next_states(state), key=lambda s: s.h, reverse=1-state.player)


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


# def h(board, player):
#     return sum(weights[i][j] * (1 if player == piece else -1)
#                for i, row in enumerate(board)
#                for j, piece in enumerate(row))


# class State(NamedTuple):
#     board: List[List[int]]
#     moves: List[Tuple[int, int]]
#     h: float
#     next: List[State]
#     prev: State
#     player: int


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


# def _calc_next(state: State):
#     for move in state.moves:
#         board = [row[:] for row in state.board]
#         do_move(board, move, state.player)
#         next_player = 1 - state.player
#         state.next.append(State(board=board, moves=moves(board, next_player),
#                                 h=h(board, next_player), next=[], prev=state, player=next_player))


max_depth = 6


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


def random_move(state):
    return random.choice(state.next)


def agent_move(state):
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


# def turtle_random():
#     player = 0
#     B = Board()

#     while True:
#         B.draw()
#         B.show()
#         m = B.random_move(player) if player else B.agent_move(player)
#         B.do_move(m, player)
#         player = 1-player
#         input()  # raw_input()
#         if B.terminal():
#             break

#     B.draw()
#     B.show()
#     print('Result', B.result())
#     input('Game over!')  # raw_input('Game over!')

#     sys.exit(0)


def main():
    N = 1
    x = sum(agent_vs_random() > 0 for _ in range(N))
    print(f'{100 * x // N}%   {x}')


if __name__ == '__main__':
    main()
