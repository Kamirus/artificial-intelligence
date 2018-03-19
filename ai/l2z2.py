from typing import Tuple, FrozenSet, Iterable, Optional, List, Set

Pos = Tuple[int, int]
Boxes = FrozenSet[Pos]
Map = List[str]


def load_state() -> Tuple[Map, Boxes, Pos]:
    replace = 'KB*+'
    ___with = '..GG'
    trans = str.maketrans(replace, ___with, '\n')

    board: Map = []  # just walls and blanks
    boxes: Set[Pos] = set()  # {(a, b)}
    guy: Optional[Pos] = None  # (a, b)

    with open('zad_input.txt') as f:
        for i, line in enumerate(f):
            boxes.update((i, j) for j, c in enumerate(line) if c in {'B', '*'})
            board.append(line.translate(trans))
            j_guy = max([line.find('+'), line.find('K')])
            if j_guy != -1:
                guy = (i, j_guy)

    assert guy is not None, "player not found on map"

    return board, frozenset(boxes), guy


class Sokoban:
    def __init__(self) -> None:
        board, boxes, guy = load_state()
        self.map = board
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
        pass

def main() -> None:
    f = open('zad_output.txt', 'w')
    board, boxes, guy = load_state()
    print(*board, sep='\n', file=f)
    print(boxes, sep='\n', file=f)
    print(guy, sep='\n', file=f)

    sokoban = Sokoban()

    # sokoban.next_states([1, 2], {1})
    # sokoban.next_states((1, 2), {(1,2)})

    for move, g, b in Sokoban().next_states(*sokoban.fst):
        print(move, g, b, file=f)


if __name__ == '__main__':
    main()
