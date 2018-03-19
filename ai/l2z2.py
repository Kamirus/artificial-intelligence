from util import PQueue, Heuristics, SeekerGrid
from typing import Tuple, FrozenSet, Iterable, Optional, List, Set, Callable, Dict, Generic, TypeVar, Sequence

Pos = Tuple[int, int]
Boxes = FrozenSet[Pos]
Map = List[str]
State = Tuple[Pos, Boxes]


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


class Sokoban(SeekerGrid):
    moves = {'U': (-1, 0),
             'D': (1, 0),
             'L': (0, -1),
             'R': (0, 1), }

    def __init__(self) -> None:
        board, boxes, targets, guy = load_state()
        self.map = board
        self.targets = targets
        self.fst = (guy, boxes)
        self.memo: Dict[State, str] = {}
        self.default = ''

    def f(self, state: State) -> float:
        # cost to current state + h(state)
        return len(self.memo[state]) + self.h(state)

    def h(self, state: State) -> float:
        # d = Heuristics.euclidean
        d = Heuristics.manhattan  # kinda better

        guy, boxes = state
        free_targets = self.targets - boxes
        bad_boxes = boxes - self.targets
        # sum distance to bad boxes
        s = sum((d(guy, box) for box in bad_boxes), .0)
        # sum distances from every bad boxe to closest free target
        s += sum((min(d(box, target) for target in free_targets)
                  for box in bad_boxes), .0)
        return s

    def next_states(self, state: State) -> Iterable[Tuple[str, State]]:
        guy, boxes = state
        guy_i, guy_j = guy
        prev_moves = self.memo[state]
        for move in self.moves:
            i, j = self.moves[move]
            x, y = pos = (guy_i + i, guy_j + j)
            is_box = pos in boxes
            if self._is_place_free(*pos, boxes):
                yield prev_moves + move, (pos, boxes)
            elif is_box and self._is_place_free(x + i, y + j, boxes):
                yield prev_moves + move, (pos, boxes - {pos} | {(x + i, y + j)})

    def is_end(self, state: State) -> bool:
        return self.targets == state[1]

    def _is_place_free(self, i: int, j: int, boxes: Boxes) -> bool:
        return self.map[i][j] in {'.', 'G'} and (i, j) not in boxes


def main() -> None:
    with open('zad_output.txt', 'w') as f:
        # print(Sokoban().search_bfs(), file=f)
        print(Sokoban().search_astar(), file=f)


if __name__ == '__main__':
    main()
