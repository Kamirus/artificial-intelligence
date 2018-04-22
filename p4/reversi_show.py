import random
import sys
from collections import defaultdict as dd
from turtle import *
from typing import List, Tuple, Optional

#####################################################
# turtle graphic
#####################################################
tracer(0, 1)  # type: ignore

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

    def __init__(self):
        self.board = initial_board()
        self.fields = {(i, j) for i in range(M) for j in range(M)
                       if self.board[i][j] == None}
        self.move_list = []
        self.history = []

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
        return [(x, y) for (x, y) in self.fields
                if any(self.can_beat(x, y, dir, player) for dir in Board.dirs)]

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
        self.history.append([row[:] for row in self.board])
        self.move_list.append(move)

        if move is None:
            return
        x, y = move
        x0, y0 = move
        self.board[x][y] = player
        self.fields.remove(move)  # -= set([move])
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

    def terminal(self) -> bool:
        if not self.fields:
            return True
        if len(self.move_list) < 2:
            return False
        return self.move_list[-1] == self.move_list[-2] == None

    def random_move(self, player: int) -> Optional[Tuple[int, int]]:
        ms = self.moves(player)
        return random.choice(ms) if ms else None


player = 0
B = Board()

while True:
    B.draw()
    B.show()
    m = B.random_move(player)
    B.do_move(m, player)
    player = 1-player
    input()  # raw_input()
    if B.terminal():
        break

B.draw()
B.show()
print('Result', B.result())
input('Game over!')  # raw_input('Game over!')


sys.exit(0)
