
def B(i, j):
    return f'B_{i}_{j}'

def get_neighbors(i, j, rows_len, cols_len):
    return [B(a,b) for a,b in [(i + oi, j + oj) for oi in [-1,0,1] for oj in [-1,0,1] if oi != 0 and oj != 0]
            if 0 <= a < cols_len and 0 <= b < rows_len]

def domains(Vs):
    return [f'{q} in 0..1' for q in Vs]

def constraints(rows, cols):
    R = len(rows)
    C = len(cols)

    def get_row(i):
        return [B(i, j) for j in range(C)]

    def get_col(j):
        return [B(i, j) for i in range(R)]

    def get_squares():
        return [[B(i  ,j), B(i  ,j+1), 
                 B(i+1,j), B(i+1,j+1)] for i in range(R-1) for j in range(C-1)]

    def get_lines():
        yield from ([B(i, j), B(i, j+1), B(i, j+2)] for i in range(R) for j in range(C-2))
        yield from ([B(i, j), B(i+1, j), B(i+2, j)] for i in range(R-2) for j in range(C))

    def radars():
        yield from (' + '.join(get_row(i)) + ' #= ' + rows[i] for i in range(R))
        yield from (' + '.join(get_col(i)) + ' #= ' + cols[i] for i in range(C))

    def forbidden():
        yield from (b + ' #==> ' + a + ' #\/ ' + c for a, b, c in get_lines())
        yield from (' + '.join(sq) + ' #\= 3' for sq in get_squares())
        yield from (f'{a} #/\ {d} #<==> {b} #/\ {c}' for a, b, c, d in get_squares())

    yield from forbidden()
    yield from radars()

def print_constraints(Cs, indent, d):
    position = indent
    writeln( (indent - 1) * ' ')
    for c in Cs:
        writeln( c + ',')
        position += len(c)
        if position > d:
            position = indent
            writeln('')
            writeln( (indent - 1) * ' ')

def storms(rows, cols, triples):
    writeln(':- use_module(library(clpfd)).')

    R = len(rows)
    C = len(cols)

    bs = [B(i, j) for i in range(R) for j in range(C)]

    writeln('solve([' + ', '.join(bs) + ']) :- ')

    cs = domains(bs) + list(constraints(rows, cols))
    for i, j, val in triples:
        cs.append(f'{B(i, j)} #= {val}')

    print_constraints(cs, 4, 70)

    writeln('    labeling([ff], [' + ', '.join(bs) + ']).')
    writeln('')
    writeln(":- tell('prolog_result.txt'), solve(X), write(X), nl, told.")


def writeln(s):
    output.write(s + '\n')


txt = open('zad_input.txt').readlines()
output = open('zad_output.txt', 'w')

rows = txt[0].split()
cols = txt[1].split()
triples = []

for i in range(2, len(txt)):
    if txt[i].strip():
        triples.append(txt[i].split())

storms(rows, cols, triples)
