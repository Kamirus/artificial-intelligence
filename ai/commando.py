import random
from util import SeekerGrid
from typing import Tuple, FrozenSet, Iterable, Optional, List, Set, Callable, Dict

Pos = Tuple[int, int]
Poses = FrozenSet[Pos]
Map = List[List[str]]


def raw_load() -> Tuple[Map, Poses]:
    """Reads input map, extracts just target positions from the map"""
    replace = '#G SB'
    ___with = '#  SS'
    trans = str.maketrans(replace, ___with, '\n')

    board: Map = []  # just walls, blanks, possible commandos
    targets: Set[Pos] = set()

    with open('zad_input.txt') as f:
        for i, line in enumerate(f):
            board.append(list(line.translate(trans)))
            for j, c in enumerate(line):
                if c in {'G', 'B'}:
                    targets.add((i, j))

    return board, frozenset(targets)


def split_possible_commandos(board: Map) -> Tuple[Map, Poses]:
    """Return new map with just walls and blanks and possible locations with commando"""
    replace = '# S'
    ___with = '#  '
    # trans = str.maketrans(replace, ___with, '\n')

    new_board: Map = []  # just walls and blanks
    commandos: Set[Pos] = set()

    for i, row in enumerate(board):
        new_board.append([' ' if c == 'S' else c for c in row])
        for j, c in enumerate(row):
            if c == 'S':
                commandos.add((i, j))

    return new_board, frozenset(commandos)


class CommandoReducer:
    moves = {'U': (-1, 0),
             'D': (1, 0),
             'L': (0, -1),
             'R': (0, 1), }
    _moves_keys = tuple(moves.keys())

    def __init__(self, *, debug: bool=False) -> None:
        board, targets = raw_load()
        self.full_map = board
        self.targets = targets
        self.n, self.m = len(board), len(board[0])
        self.counter = 0
        self.debug = debug

    def move(self, move: str) -> str:
        """Move every S on map according to [move] from moves"""
        i, j = self.moves[move]
        rows_idxes = range(self.n) if i <= 0 else range(self.n - 1, -1, -1)
        cols_idxes = range(self.m) if j <= 0 else range(self.m - 1, -1, -1)
        changed = 0
        for x in rows_idxes:
            for y in cols_idxes:
                if self.full_map[x][y] == 'S' and self.full_map[x+i][y+j] != '#':
                    changed = 1
                    self.full_map[x][y] = ' '
                    self.full_map[x+i][y+j] = 'S'
        self.counter += changed
        if self.debug and changed:
            self._print()
            print(self.counter)
        return move if changed else ''

    def move_max(self, move: str) -> str:
        for i in range(self.n if move in {'D', 'U'} else self.m):
            if not self.move(move):
                break
        return move * i

    def move_random(self) -> str:
        move = random.choice(self._moves_keys)
        changed = self.move(move)
        return move if changed else ''

    def _print(self) -> None:
        for row in self.full_map:
            print(''.join(row), sep='\n')

    def reduce(self) -> str:
        return ''.join([
            self.move_max('R'),
            self.move_max('D'),
            self.move_max('L'),
            # self.move_max('U'),
            # *(self.move_random() for _ in range(10)),
            # self.move_max('U'),
        ])


class CommandoSeeker(SeekerGrid):
    moves = {'U': (-1, 0),
             'D': (1, 0),
             'L': (0, -1),
             'R': (0, 1), }

    def __init__(self, board: Map, targets: Poses, state: Poses, moves: str) -> None:
        self.targets = targets
        self.map, self.fst = board, state
        self.memo: Dict[Poses, str] = {}
        self.default = moves

    def search(self, p: Callable[[Poses], float], reduce=False, **kwargs) -> str:
        if not reduce or len(self.fst) < 4:
            return super().search(p)

        cs = CommandoSeeker(self.map, self.targets, self.fst, self.default)
        cs.is_end = self.is_reduced  # type: ignore
        reduce_moves = super(CommandoSeeker, cs).search(p)
        self.fst = self._spy_state
        self.default = reduce_moves

        return self.search(p, reduce=reduce)

    def f(self, state) -> float:
        raise NotImplemented

    def h(self, state) -> float:
        raise NotImplemented

    def next_states(self, state: Poses) -> Iterable[Tuple[str, Poses]]:
        prev_moves = self.memo[state]
        for move in self.moves:
            i, j = self.moves[move]
            new_state = frozenset({(x + i, y + j)
                                   if self.map[x + i][y + j] != '#'
                                   else (x, y)
                                   for x, y in state})
            yield prev_moves + move, new_state

    def is_end(self, state: Poses) -> bool:
        return not bool(state - self.targets)  # is_empty

    def is_reduced(self, state: Poses) -> bool:
        self._spy_state = state
        return len(state) < len(self.fst)
        # if len(state) < len(self.fst):
        #     # print(self.fst)
        #     # print(state)
        #     return True
        # return False
