def load_state():
    replace = 'KB*+'
    ___with = '..GG'
    trans = str.maketrans(replace, ___with, '\n')

    board = []  # just walls and blanks
    boxes = set()  # {(a, b)}
    guy = None  # (a, b)

    with open('zad_input.txt') as f:
        for i, line in enumerate(f):
            boxes.update((i, j) for j, c in enumerate(line) if c in {'B', '*'})
            board.append(line.translate(trans))
            j_guy = max([line.find('+'), line.find('K')])
            if j_guy != -1:
                guy = (i, j_guy)

    return board, frozenset(boxes), guy


class Sokoban:
    def __init__(self):
        board, boxes, guy = load_state()
        self.map = board
        self.fst = (guy, boxes)
        self.moves = {'U': (-1, 0),
                      'D': (1, 0),
                      'L': (0, -1),
                      'R': (0, 1), }

    def _is_place_free(self, i, j, boxes):
        return self.map[i][j] in {'.', 'G'} and (i, j) not in boxes

    def next_states(self, guy, boxes):
        guy_i, guy_j = guy
        for move in self.moves:
            i, j = self.moves[move]
            x, y = pos = (guy_i + i, guy_j + j)
            is_box = pos in boxes
            if self._is_place_free(*pos, boxes):
                yield move, pos, boxes
            elif is_box and self._is_place_free(x + i, y + j, boxes):
                yield move, pos, boxes - {pos} | {(x + i, y + j)}


def main():
    f = open('zad_output.txt', 'w')
    board, boxes, guy = load_state()
    print(*board, sep='\n', file=f)
    print(boxes, sep='\n', file=f)
    print(guy, sep='\n', file=f)

    sokoban = Sokoban()

    for move, g, b in Sokoban().next_states(*sokoban.fst):
        print(move, g, b, file=f)


if __name__ == '__main__':
    main()
