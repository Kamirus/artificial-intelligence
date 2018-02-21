import random

colors = ('c', 'd', 'h', 's')  # clubs diamonds hearts spades
symbols = ('ace', 'king', 'queen', 'jack')

cards_with_numbers = [(color, val) for color in colors for val in range(2, 11)]
cards_with_symbols = [(color, sym) for color in colors for sym in symbols]

values_min_to_max = ['1', '2', '2-2', '3', 'straight', 'color', '3-2', '4', 'poker']


def random_hand(deck):
    return random.sample(deck, 5)


def is_straight(symbols):
    ss = sorted(symbols, reverse=True)
    try:
        return all(ss[0] == int(s) + i for i, s in enumerate(ss))
    except ValueError:
        return False


def key_hand(hand):
    colors = sorted(c for c, _ in hand)
    symbols = sorted(s for _, s in hand)

    is_all_colors = all(colors[0] == c for c in colors)
    counter = [sum(1 for s in symbols if sym == s) for sym in set(symbols)]

    if is_straight(symbols) and is_all_colors:
        key = 'poker'
    elif 4 in counter:
        key = '4'
    elif 3 in counter and 2 in counter:
        key = '3-2'
    elif is_all_colors:
        key = 'color'
    elif is_straight(symbols):
        key = 'straight'
    elif 3 in counter:
        key = '3'
    elif sum(1 for x in counter if x == 2) == 2:
        key = '2-2'
    elif 2 in counter:
        key = '2'
    else:
        key = '1'

    return key


def cmp(h1, h2):
    return values_min_to_max.index(key_hand(h1)) - values_min_to_max.index(key_hand(h2))


if __name__ == '__main__':
    n = 10000
    numbers_won = 0
    for _ in range(n):
        n_hand = random_hand(cards_with_numbers)
        s_hand = random_hand(cards_with_symbols)
        if cmp(n_hand, s_hand) > 0:
            numbers_won += 1
    print(100 * numbers_won / n, "%", sep='')