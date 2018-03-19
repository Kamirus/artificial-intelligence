import util

from typing import Tuple, FrozenSet, Iterable, Optional, List, Set

Pos = Tuple[int, int]
Boxes = FrozenSet[Pos]
Map = List[str]


def load_state() -> Tuple[Map, Boxes, Boxes, Pos]:
    replace = '.WKB*+G'
    ___with = '.W..GGG'
    trans = str.maketrans(replace, ___with, '\n')

    board: Map = []  # just walls and blanks
    boxes: Set[Pos] = set()
    targets: Set[Pos] = set()  # where boxes should be placed
    guy: Optional[Pos] = None

    with open('zad_input.txt') as f:
        for i, line in enumerate(f):
            board.append(line.translate(trans))
            for j, c in enumerate(line):
                if c in {'G', '*', '+'}:
                    targets.add((i, j))
                if c in {'B', '*'}:
                    boxes.add((i, j))
                if c in {'K', '+'}:
                    guy = (i, j)

    assert guy is not None, "player not found on map"

    return board, frozenset(boxes), frozenset(targets), guy





class Sokoban:
    def __init__(self) -> None:
        board, boxes, targets, guy = load_state()
        self.map = board
        self.targets = targets
        self.fst = (guy, boxes)
        self.moves = {'U': (-1, 0),
                      'D': (1, 0),
                      'L': (0, -1),
                      'R': (0, 1), }

    def _is_place_free(self, i: int, j: int, boxes: Boxes) -> bool:
        return self.map[i][j] in {'.', 'G'} and (i, j) not in boxes

    def next_states(self, guy: Pos, boxes: Boxes) -> Iterable[Tuple[str, Pos, Boxes]]:
        guy_i, guy_j = guy
        for move in self.moves:
            i, j = self.moves[move]
            x, y = pos = (guy_i + i, guy_j + j)
            is_box = pos in boxes
            if self._is_place_free(*pos, boxes):
                yield move, pos, boxes
            elif is_box and self._is_place_free(x + i, y + j, boxes):
                yield move, pos, boxes - {pos} | {(x + i, y + j)}

    def is_end(self, boxes: Boxes) -> bool:
        return self.targets == boxes

    def search(self) -> str:
        """finds shortest sequence of moves to finish state"""
        q = util.PQueue()
        memo = {}
        q.push(self.fst)
        memo[self.fst] = ''
        while q:
            _, boxes = state = q.pop()
            if self.is_end(boxes):
                return memo[state]
            for move, nguy, nboxes in self.next_states(*state):
                next_state = (nguy, nboxes)
                if next_state not in memo:
                    memo[next_state] = memo[state] + move
                    q.push(next_state)
        raise RuntimeError('Found nothing!')


def main() -> None:
    with open('zad_output.txt', 'w') as f:
        board, boxes, targets, guy = load_state()
        # print(*board, sep='\n', file=f)
        # print(boxes, sep='\n', file=f)
        # print(targets, sep='\n', file=f)
        # print(guy, sep='\n', file=f)

        moves = Sokoban().search()
        print(moves, file=f)

        # sokoban.next_states([1, 2], {1})
        # sokoban.next_states((1, 2), {(1,2)})

        # for move, g, b in Sokoban().next_states(*sokoban.fst):
        #     print(move, g, b, file=f)


if __name__ == '__main__':
    main()
