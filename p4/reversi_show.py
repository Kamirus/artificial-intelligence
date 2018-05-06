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


class Board:
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

    def __init__(self):
        self.board = initial_board()
        # self.fields = {(i, j) for i in range(M) for j in range(M)
        #                if self.board[i][j] == None}
        # self.move_list = []
        # self.history = []

    def draw(self):
        for i in range(M):
            res = []
            for j in range(M):
                b = self.board[i][j]
                if b == None:
                    res.append('.')
                elif b == 1:
                    res.append('#')
                else:
                    res.append('o')
            print(''.join(res))
        print()

    def show(self):
        for i in range(M):
            for j in range(M):
                kwadrat(j, i, 'green')

        for i in range(M):
            for j in range(M):
                if self.board[i][j] == 1:
                    kolko(j, M - 1 - i, 'black')
                if self.board[i][j] == 0:
                    kolko(j, M - 1 - i, 'white')

    def moves(self, player: int) -> List[Tuple[int, int]]:
        return [(x, y) for x in range(M) for y in range(M)
                if self.board[x][y] == None  # for (x, y) in self.fields
                and any(self.can_beat(x, y, dir, player) for dir in Board.dirs)]

    def can_beat(self, x: int, y: int, direction: Tuple[int, int], player: int) -> bool:
        dx, dy = direction
        x += dx
        y += dy
        cnt = 0
        while self.get(x, y) == 1 - player:
            x += dx
            y += dy
            cnt += 1
        return cnt > 0 and self.get(x, y) == player

    def get(self, x: int, y: int) -> Optional[int]:
        if 0 <= x < M and 0 <= y < M:
            return self.board[x][y]
        return None

    def do_move(self, move: Optional[Tuple[int, int]], player: int) -> None:
        # self.history.append([row[:] for row in self.board])
        # self.move_list.append(move)

        # if move is None:
        #     return
        x, y = move
        x0, y0 = move
        self.board[x][y] = player
        # self.fields.remove(move)  # -= set([move])
        for dx, dy in self.dirs:
            x, y = x0, y0
            to_beat = []
            x += dx
            y += dy
            while self.get(x, y) == 1 - player:
                to_beat.append((x, y))
                x += dx
                y += dy
            if self.get(x, y) == player:
                for (nx, ny) in to_beat:
                    self.board[nx][ny] = player

    # def undo_last_move(self) -> None:
    #     assert len(self.history) and len(self.move_list)
    #     self.board = self.history.pop()
    #     self.fields.add(self.move_list.pop())

    def result(self) -> int:
        res = 0
        for x in range(M):
            for y in range(M):
                b = self.board[x][y]
                if b == 0:
                    res += 1
                elif b == 1:
                    res -= 1
        return res

    # def terminal(self) -> bool:
    #     if not self.fields:
    #         return True
    #     if len(self.move_list) < 2:
    #         return False
    #     return self.move_list[-1] == self.move_list[-2] == None

    # def random_move(self, player: int) -> Optional[Tuple[int, int]]:
    #     ms = self.moves(player)
    #     return random.choice(ms) if ms else None

    def h(self, player: int) -> float:
        return sum(self.weights[i][j] * (1 if player == piece else -1)
                   for i, row in enumerate(self.board)
                   for j, piece in enumerate(row))


State = namedtuple('State', ['board', 'moves', 'h', 'next', 'prev', 'player'])

# class State(NamedTuple):
#     board: List[List[int]]
#     moves: List[Tuple[int, int]]
#     h: float
#     next: List[State]
#     prev: State
#     player: int


class Agent:
    """
    - lazy game tree
    - calculate h for each
    - calculate moves for each
    """

    def __init__(self) -> None:
        b = self.b = Board()
        self.state = State(board=b.board, moves=b.moves(0),  # type: ignore
                           h=b.h(0), next=[], prev=None, player=0)  # type: ignore

    def terminal(self, state: State) -> bool:
        return len(state.moves) + len(state.next) == 0
        # if not self.fields:
        #     return True
        # if len(self.move_list) < 2:
        #     return False
        # return self.move_list[-1] == self.move_list[-2] == None

    def _calc_next(self, state: State) -> None:
        b = self.b
        for move in state.moves:
            b.board = [row[:] for row in state.board]
            b.do_move(move, state.player)
            next_player = 1 - state.player
            state.next.append(State(board=b.board, moves=b.moves(next_player),  # type: ignore
                                    h=b.h(next_player), next=[], prev=state, player=next_player))  # type: ignore

    max_depth = 3
    cuts = 0

    def max_value(self, state: State, alpha: float, beta: float, depth: int) -> float:
        b = self.b
        b.board = state.board

        if self.terminal(state):
            return b.result()
        if depth >= self.max_depth:
            return state.h

        value = -inf
        if not state.next:
            self._calc_next(state)
        for s in state.next:
            value = max(value, self.min_value(s, alpha, beta, depth + 1))
            if value >= beta:
                self.cuts += 1
                return value
            alpha = max(alpha, value)
        return value

    def min_value(self, state: State, alpha: float, beta: float, depth: int) -> float:
        b = self.b
        b.board = state.board

        if self.terminal(state):
            return b.result()
        if depth >= self.max_depth:
            return state.h

        value = inf
        if not state.next:
            self._calc_next(state)
        for s in state.next:
            value = min(value, self.max_value(s, alpha, beta, depth + 1))
            if value <= alpha:
                self.cuts += 1
                return value
            beta = min(beta, value)
        return value

    def random_move(self):
        if not self.state.next:
            self._calc_next(self.state)
        self.state = random.choice(self.state.next)

    def agent_move(self):
        if not self.state.next:
            self._calc_next(self.state)
        self.state = max(
            self.state.next, key=lambda s: self.min_value(s, -inf, inf, 0))

    #     def key(move):
    #         self.do_move(move, player)
    #         v = self.min_value(1 - player, -inf, inf, 0)
    #         self.undo_last_move()
    #         return v
    #     moves = self.moves(player)
    # return max(moves, key=key) if moves else None

def agent_vs_random() -> int:
    B = Agent()

    while not B.terminal(B.state):
        if B.state.player:
            B.random_move()
        else:
            # B.random_move()
            B.agent_move()

    # print(B.cuts)
    B.b.board = B.state.board
    return B.b.result()


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
