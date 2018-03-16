from nonograms import Nonogram


def parse_line_ints(line): return (int(s) for s in line.strip().split())


def read_line_ints(f): return parse_line_ints(f.readline())


def load_input_file(file):
    n, m = read_line_ints(f)
    row_c = tuple(tuple(read_line_ints(f)) for _ in range(n))
    col_c = tuple(tuple(read_line_ints(f)) for _ in range(m))
    return row_c, col_c


def load_input(*, spy=False):
    _input = input
    if spy:
        import sys
        def _input():
            l = input()
            print(l, file=sys.stderr)
            return l
    n, m = parse_line_ints(_input())
    row_c = tuple(tuple(parse_line_ints(_input())) for _ in range(n))
    col_c = tuple(tuple(parse_line_ints(_input())) for _ in range(m))
    return row_c, col_c


if __name__ == '__main__':
    Nonogram(*load_input(spy=1)).solve(print_all=0).print_matrix()

    # with open('zad_input.txt') as f:
    #     row_c, col_c = load_input_file(f)
    #     # import sys
    #     # sys.stdout = open('zad_output.txt', 'w')
    #     Nonogram(row_c, col_c).solve(print_all=0).print_matrix()
