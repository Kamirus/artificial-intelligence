import random
import itertools

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
        key = 'poker'  # only numbers
    elif 4 in counter:
        key = '4'
    elif 3 in counter and 2 in counter:
        key = '3-2'
    elif is_all_colors:
        key = 'color'  # only numbers
    elif is_straight(symbols):
        key = 'straight'  # only numbers
    elif 3 in counter:
        key = '3'
    elif sum(1 for x in counter if x == 2) == 2:
        key = '2-2'
    elif 2 in counter:
        key = '2'
    else:
        key = '1'

    return key


def hand_value(hand):
    return 2 * values_min_to_max.index(key_hand(hand)) + (hand[0][1] in symbols)


def test_decks(deck1, deck2, *, n_times=10000):
    deck1_wins = 0
    for _ in range(n_times):
        h1, h2 = (random_hand(d) for d in (deck1, deck2))
        if hand_value(h1) > hand_value(h2):
            deck1_wins += 1
    return 100.0 * deck1_wins / n_times


def number_deck_variations_and_results():
    for i in range(2, 10):
        for c in range(4):
            if (11 - i) * (4 - c) < 5:
                continue
            number_deck = [(color, val) for color in colors[c:] for val in range(i, 11)]
            win_rate = test_decks(number_deck, cards_with_symbols)
            yield win_rate, f'{4-c}c {i}-10 {len(number_deck)}'


if __name__ == '__main__':
    print(f'Numbers win rate: ${test_decks(cards_with_numbers, cards_with_symbols)}%')
    print(*sorted(number_deck_variations_and_results(), reverse=True), sep='\n')
