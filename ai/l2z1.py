from nonograms import Nonogram


def read_line_ints(f): return (int(s) for s in f.readline().strip().split())


def load_input_file(file):
    n, m = read_line_ints(f)
    row_c = tuple(tuple(read_line_ints(f)) for _ in range(n))
    col_c = tuple(tuple(read_line_ints(f)) for _ in range(m))
    return row_c, col_c


if __name__ == '__main__':
    with open('zad_input.txt') as f:
        row_c, col_c = load_input_file(f)
        import sys
        sys.stdout = open('zad_output.txt', 'w')
        Nonogram(row_c, col_c).solve(print_all=0).print_matrix()
