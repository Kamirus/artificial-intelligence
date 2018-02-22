import os

def value(word_list):
    return sum(len(word) ** 2 for word in word_list)


def main(text, words):
    def dp(text):
        if not text:
            return []
        def step():
            for i, _ in enumerate(text):
                word = text[0:i+1]
                if word in words:
                    try:
                        yield [word] + dp(text[i+1:])
                    except ValueError:
                        pass

        return max(step(), key=value)

    try:
        return dp(text)
    except ValueError as e:
        raise Exception('not enough words') from e


def load_words(filename='polish_words.txt'):
    with open(os.path.abspath(os.path.join(os.path.curdir, filename))) as f:
        return set(line.strip() for line in f)


if __name__ == '__main__':
    words = load_words()
    # text = 'tamatematykapustkinieznosi'
    text = 'cześćczłowiektuinnyktośjaktamtenprzedmiot'
    print(*main(text, words), sep=' ')
