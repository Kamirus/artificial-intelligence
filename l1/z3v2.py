import random
import itertools

from math import factorial


def binom(n, k):
    return factorial(n) // (factorial(k) * factorial(n - k))


colors = ('c', 'd', 'h', 's')  # clubs diamonds hearts spades
symbols = ('ace', 'king', 'queen', 'jack')

cards_with_numbers = [(color, val) for color in colors for val in range(2, 11)]
cards_with_symbols = [(color, sym) for color in colors for sym in symbols]

values_min_to_max = ['1', '2', '2-2', '3',
                     'straight', 'color', '3-2', '4', 'poker']

len_numbers = len(cards_with_numbers)  # 36
len_symbols = len(cards_with_symbols)  # 16


class NumHands:
    poker = 4 * 5  # colors * (2,3,4,5,6)7,8,9,10
    _4 = 9 * (36 - 4)
    _3_2 = 9 * binom(4, 3) * 8 * binom(4, 2)
    color = 4 * binom(9, 5) - poker
    straight = 5 * 4 ** 5 - poker
    _3 = 9 * binom(4, 3) * binom(8, 2) * 4 ** 2
    _2_2 = binom(9, 2) * (binom(4, 2) ** 2) * (7 * 4)
    _2 = 9 * binom(4, 2) * binom(8, 3) * 4 ** 3
    _1 = binom(36, 5) - poker - _4 - \
        _3_2 - color - straight - _3 - _2_2 - _2
    all = [_1, _2, _2_2, _3, straight, color, _3_2, _4, poker]


l = [(key, val) for key, val in vars(NumHands).items()
     if not key.startswith('__') and key != 'all']
print(*[f'{key}: {val}' for key, val in l], sep='\n')
print(sum(val for _, val in l))
print(binom(36, 5))


class SymHands:
    poker = 0
    _4 = 4 * (16 - 4)
    _3_2 = 4 * binom(4, 3) * 3 * binom(4, 2)
    color = 0
    straight = 0
    _3 = 4 * binom(4, 3) * binom(3, 2) * 4 ** 2
    _2_2 = binom(4, 2) * (binom(4, 2) ** 2) * (3 * 4)
    _2 = 4 * binom(4, 2) * binom(3, 3) * 4 ** 3
    _1 = 0
    all = [_1, _2, _2_2, _3, straight, color, _3_2, _4, poker]


l = [(key, val) for key, val in vars(SymHands).items()
     if not key.startswith('__') and key != 'all']
print(*[f'{key}: {val}' for key, val in l], sep='\n')

def f():
    sum_sym = binom(16, 5)
    sum_num = binom(36, 5)

    def step(i):
        """
        num got i-th set
        what's the probability that he wins?
        """
        return sum(SymHands.all[j] / sum_sym for j in range(i))

    # probability that num wins
    return sum(nr / sum_num * step(i)
               for i, nr in enumerate(NumHands.all))

print(f())
