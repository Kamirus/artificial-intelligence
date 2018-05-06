import random
import sys
from math import inf
from collections import defaultdict as dd
from turtle import *
from typing import List, Tuple, Optional, NamedTuple
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

p0 = 8.0
p1 = 5.0
p2 = 2.0
p3 = 1.0
p4 = 0.5
p5 = 0.2

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


def moves(board, player: int) -> List[Tuple[int, int]]:
    return [(x, y) for x in range(M) for y in range(M)
            if board[x][y] == None
            and any(can_beat(board, x, y, dir, player) for dir in dirs)]


def can_beat(board, x, y, direction, player):
    dx, dy = direction
    x += dx
    y += dy
    cnt = 0
    while get(board, x, y) == 1 - player:
        x += dx
        y += dy
        cnt += 1
    return cnt > 0 and get(board, x, y) == player


def get(board, x, y):
    if 0 <= x < M and 0 <= y < M:
        return board[x][y]
    return None


def do_move(board, move, player):
    x, y = move
    x0, y0 = move
    board[x][y] = player
    for dx, dy in dirs:
        x, y = x0, y0
        to_beat = []
        x += dx
        y += dy
        while get(board, x, y) == 1 - player:
            to_beat.append((x, y))
            x += dx
            y += dy
        if get(board, x, y) == player:
            for (nx, ny) in to_beat:
                board[nx][ny] = player


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


def h(board, player):
    return sum(weights[i][j] * (1 if player == piece else -1)
               for i, row in enumerate(board)
               for j, piece in enumerate(row))


State = namedtuple('State', ['board', 'moves', 'h', 'next', 'prev', 'player'])

# class State(NamedTuple):
#     board: List[List[int]]
#     moves: List[Tuple[int, int]]
#     h: float
#     next: List[State]
#     prev: State
#     player: int


def init():
    board = initial_board()
    return State(board=board, moves=moves(board, 0),
                 h=h(board, 0), next=[], prev=None, player=0)


def terminal(state):
    return len(state.moves) + len(state.next) == 0


def _calc_next(state: State):
    for move in state.moves:
        board = [row[:] for row in state.board]
        do_move(board, move, state.player)
        next_player = 1 - state.player
        state.next.append(State(board=board, moves=moves(board, next_player),
                                h=h(board, next_player), next=[], prev=state, player=next_player))

max_depth = 2

def max_value(state: State, alpha: float, beta: float, depth: int) -> float:
    if terminal(state):
        return result(state.board)
    if depth >= max_depth:
        return state.h

    value = -inf
    if not state.next:
        _calc_next(state)
    for s in state.next:
        value = max(value, min_value(s, alpha, beta, depth + 1))
        if value >= beta:
            return value
        alpha = max(alpha, value)
    return value

def min_value(state: State, alpha: float, beta: float, depth: int) -> float:
    if terminal(state):
        return result(state.board)
    if depth >= max_depth:
        return state.h

    value = inf
    if not state.next:
        _calc_next(state)
    for s in state.next:
        value = min(value, max_value(s, alpha, beta, depth + 1))
        if value <= alpha:
            return value
        beta = min(beta, value)
    return value

def random_move(state):
    if not state.next:
        _calc_next(state)
    return random.choice(state.next)

def agent_move(state):
    if not state.next:
        _calc_next(state)
    return max(state.next, key=lambda s: min_value(s, -inf, inf, 0))


def agent_vs_random() -> int:
    state = init()
    while not terminal(state):
        if state.player:
            state = random_move(state)
        else:
            state = agent_move(state)

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
    x = sum(agent_vs_random() > 0 for _ in range(1))
    print(x)


if __name__ == '__main__':
    main()
