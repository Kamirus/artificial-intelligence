from nonograms import Nonogram


def parse_line_ints(line): return (int(s) for s in line.strip().split())


def load_input(spy=1):
    n, m = parse_line_ints(input())
    row_c = tuple(tuple(parse_line_ints(input())) for _ in range(n))
    col_c = tuple(tuple(parse_line_ints(input())) for _ in range(m))

    if spy:
        with open('zad_input.txt', 'w') as f:
            print(n,m, file=f)
            for ll in [row_c, col_c]:
                for l in ll:
                    print(*l, file=f)

    return row_c, col_c


if __name__ == '__main__':
    Nonogram(*load_input(spy=0)).solve(print_all=0).print_matrix()
