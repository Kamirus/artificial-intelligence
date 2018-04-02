import os
import random
import itertools
import sys

sys.setrecursionlimit(5000)


def all_possible_spacings(text, words):
    @memo
    def dp(text):
        if not text:
            return [[]]

        return [[word] + ending
                for word in (p for p in prefixes(text) if p in words)
                for ending in dp(text[len(word):])]

    return [' '.join(l) for l in dp(text)]


def max_square_spacing(text, words):
    def f(words_str): return sum(len(word) ** 2 for word in words_str.split())
    return max(all_possible_spacings(text, words), key=f)


def random_spacing(text, words):
    return random.sample(all_possible_spacings(text, words), 1)[0]


def compare(filename, words, *funcs, max_line_len=80):
    sums = [0 for _ in funcs]
    with open_relative(filename) as f:
        for line in f:
            if len(line) > max_line_len:
                continue
            line = line.strip()
            no_spaces_line = ''.join(line.split())
            ls = [f(no_spaces_line, words) for f in funcs]
            for i, l in enumerate(ls):
                sums[i] += l == line
                # if l != line:
                #     print(line, l, '', sep='\n')
    return sums


def main():
    filename = 'pan-tadeusz.txt'
    words = load_words()
    print('Words loaded. Start processing...')
    res = compare(filename, words, max_square_spacing)

    N = 9937
    print(res[0], res[0] / N)  # 8148

    n = 1
    rands = sum(compare(filename, words, random_spacing)[0]
                for _ in range(n)) / n
    print(rands, rands / N)  # n=100 => 923.54


def old_max_square_spacing(text, words):
    @memo
    def dp(text):
        if not text:
            return []

        def step():
            for i, _ in enumerate(text):
                word = text[0:i + 1]
                if word in words:
                    sub = dp(text[i + 1:])
                    if sub is not None:
                        yield [word] + sub

        return max(step(), key=lambda word_list: sum(len(word) ** 2 for word in word_list), default=None)

    res = dp(text)
    assert res is not None, f'not enough words in: {text}'
    return ' '.join(res)


def prefixes(s):
    return (s[:i] for i in range(1, len(s) + 1))


def memo(f):
    m = {}

    def aux(x):
        if x not in m:
            m[x] = f(x)
        return m[x]

    return aux


def open_relative(filename, *args, **kwargs):
    return open(os.path.abspath(os.path.join(os.path.curdir, filename)), *args, **kwargs)


def load_words(filename='polish_words.txt'):
    with open_relative(filename) as f:
        return set(line.strip() for line in f)


if __name__ == '__main__':
    main()
