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
    # import cProfile
    # cProfile.run('Nonogram(*load_input(spy=0)).solve(print_all=0).print_matrix()')


"""
         63086183 function calls (57185236 primitive calls) in 21.014 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000   21.013   21.013 <string>:1(<module>)
        1    0.000    0.000    0.000    0.000 codecs.py:318(decode)
        1    0.000    0.000    0.000    0.000 codecs.py:330(getstate)
       16    0.000    0.000    0.000    0.000 l2z1.py:10(<genexpr>)
      114    0.000    0.000    0.000    0.000 l2z1.py:4(<genexpr>)
       31    0.000    0.000    0.000    0.000 l2z1.py:4(parse_line_ints)
        1    0.000    0.000    0.000    0.000 l2z1.py:7(load_input)
       16    0.000    0.000    0.000    0.000 l2z1.py:9(<genexpr>)
        1    0.000    0.000    0.001    0.001 nonograms.py:10(__init__)
3611803/380902    3.388    0.000   18.413    0.000 nonograms.py:105(_dp)
   816840    0.661    0.000   19.987    0.000 nonograms.py:121(opt_dist_2d)
        1    0.000    0.000    0.001    0.001 nonograms.py:132(__init__)
        1    0.042    0.042   21.013   21.013 nonograms.py:17(solve)
     3111    0.006    0.000    0.006    0.000 nonograms.py:32(<listcomp>)
        1    0.000    0.000    0.000    0.000 nonograms.py:49(print_matrix)
       16    0.000    0.000    0.000    0.000 nonograms.py:50(<genexpr>)
    15170    0.013    0.000    1.251    0.000 nonograms.py:53(_get_wrongs)
    15170    0.195    0.000    1.238    0.000 nonograms.py:54(<listcomp>)
   192928    0.226    0.000   19.609    0.000 nonograms.py:56(_iter_cost_j)
   376909    0.433    0.000    0.457    0.000 nonograms.py:71(_neg)
        1    0.000    0.000    0.001    0.001 nonograms.py:79(_reinitialize)
  3005062    6.380    0.000   12.677    0.000 nonograms.py:87(opt_dist)
 18643775    2.447    0.000    2.447    0.000 nonograms.py:92(<genexpr>)
606741/40561    0.469    0.000   18.514    0.000 nonograms.py:97(_dp_opt_dist_2d)
     3336    0.003    0.000    0.005    0.000 random.py:172(randrange)
      225    0.000    0.000    0.000    0.000 random.py:216(randint)
    21617    0.018    0.000    0.027    0.000 random.py:222(_randbelow)
    18281    0.012    0.000    0.037    0.000 random.py:252(choice)
       15    0.000    0.000    0.000    0.000 util.py:10(<listcomp>)
       15    0.000    0.000    0.000    0.000 util.py:13(get_column)
       15    0.000    0.000    0.000    0.000 util.py:14(<listcomp>)
        1    0.000    0.000    0.000    0.000 util.py:17(get_columns)
        1    0.000    0.000    0.000    0.000 util.py:18(<listcomp>)
2314518/816840    1.567    0.000   19.082    0.000 util.py:31(aux)
        1    0.000    0.000    0.000    0.000 util.py:9(init_random_matrix)
        1    0.000    0.000    0.000    0.000 {built-in method _codecs.utf_8_decode}
        1    0.000    0.000   21.014   21.014 {built-in method builtins.exec}
       31    0.000    0.000    0.000    0.000 {built-in method builtins.input}
 10769305    0.777    0.000    0.777    0.000 {built-in method builtins.len}
  3005062    2.365    0.000    4.812    0.000 {built-in method builtins.max}
618799/12611    0.620    0.000   19.948    0.002 {built-in method builtins.min}
        1    0.000    0.000    0.000    0.000 {built-in method builtins.print}
   816840    0.141    0.000    0.141    0.000 {built-in method builtins.sum}
 18161405    1.238    0.000    1.238    0.000 {method 'append' of 'list' objects}
    21617    0.002    0.000    0.002    0.000 {method 'bit_length' of 'int' objects}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
        1    0.000    0.000    0.000    0.000 {method 'extend' of 'list' objects}
    32137    0.006    0.000    0.006    0.000 {method 'getrandbits' of '_random.Random' objects}
       15    0.000    0.000    0.000    0.000 {method 'join' of 'str' objects}
    15169    0.002    0.000    0.002    0.000 {method 'random' of '_random.Random' objects}
       31    0.000    0.000    0.000    0.000 {method 'split' of 'str' objects}
       31    0.000    0.000    0.000    0.000 {method 'strip' of 'str' objects}
"""
