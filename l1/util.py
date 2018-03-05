
def easy_line(line):
    l1 = ''.join(c for c in line.lower() if c.isalpha() or c.isspace())
    return ' '.join(l1.split())


def to_easy_txt(filename):
    with open(filename) as f:
        with open(filename + '.out', 'w') as fout:

            for line in f:
                l = easy_line(line)
                if l:
                    fout.write(l)
                    fout.write('\n')


# if __name__ == '__main__':
#     to_easy_txt('pan-tadeusz.txt')
