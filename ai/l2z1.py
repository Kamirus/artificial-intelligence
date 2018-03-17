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
         52544115 function calls (48497243 primitive calls) in 16.775 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000   16.775   16.775 <string>:1(<module>)
        1    0.000    0.000    0.000    0.000 codecs.py:318(decode)
        1    0.000    0.000    0.000    0.000 codecs.py:330(getstate)
       16    0.000    0.000    0.000    0.000 l2z1.py:10(<genexpr>)
      114    0.000    0.000    0.000    0.000 l2z1.py:4(<genexpr>)
       31    0.000    0.000    0.000    0.000 l2z1.py:4(parse_line_ints)
        1    0.000    0.000    0.000    0.000 l2z1.py:7(load_input)
       16    0.000    0.000    0.000    0.000 l2z1.py:9(<genexpr>)
        1    0.000    0.000    0.001    0.001 nonograms.py:10(__init__)
2490223/269608    2.458    0.000   12.843    0.000 nonograms.py:103(_dp)
   912690    0.601    0.000   14.390    0.000 nonograms.py:119(opt_dist_2d)
        1    0.000    0.000    0.001    0.001 nonograms.py:130(__init__)
        1    0.045    0.045   16.774   16.774 nonograms.py:17(solve)
     3600    0.017    0.000    0.017    0.000 nonograms.py:31(<listcomp>)
        1    0.000    0.000    0.000    0.000 nonograms.py:48(print_matrix)
       16    0.000    0.000    0.000    0.000 nonograms.py:49(<genexpr>)
    17012    0.013    0.000    1.254    0.000 nonograms.py:52(_get_wrongs)
    17012    0.222    0.000    1.242    0.000 nonograms.py:53(<listcomp>)
   214576    0.901    0.000   15.197    0.000 nonograms.py:55(_iter_cost_j)
    17011    0.084    0.000    0.166    0.000 nonograms.py:69(_neg)
        1    0.000    0.000    0.001    0.001 nonograms.py:77(_reinitialize)
  2072041    4.437    0.000    8.766    0.000 nonograms.py:85(opt_dist)
 12954662    1.702    0.000    1.702    0.000 nonograms.py:90(<genexpr>)
418182/28589    0.326    0.000   12.915    0.000 nonograms.py:95(_dp_opt_dist_2d)
     3825    0.003    0.000    0.006    0.000 random.py:172(randrange)
      225    0.000    0.000    0.001    0.000 random.py:216(randint)
    24437    0.020    0.000    0.029    0.000 random.py:222(_randbelow)
    20612    0.013    0.000    0.041    0.000 random.py:252(choice)
       15    0.000    0.000    0.001    0.000 util.py:10(<listcomp>)
       15    0.000    0.000    0.000    0.000 util.py:13(get_column)
       15    0.000    0.000    0.000    0.000 util.py:14(<listcomp>)
        1    0.000    0.000    0.000    0.000 util.py:17(get_columns)
        1    0.000    0.000    0.000    0.000 util.py:18(<listcomp>)
   436352    0.188    0.000    0.188    0.000 util.py:22(neg_kth)
  6981632    0.819    0.000    0.819    0.000 util.py:23(<genexpr>)
1931631/912690    1.286    0.000   13.514    0.000 util.py:31(aux)
        1    0.000    0.000    0.001    0.001 util.py:9(init_random_matrix)
        1    0.000    0.000    0.000    0.000 {built-in method _codecs.utf_8_decode}
        1    0.000    0.000   16.775   16.775 {built-in method builtins.exec}
       31    0.000    0.000    0.000    0.000 {built-in method builtins.input}
  7907399    0.566    0.000    0.566    0.000 {built-in method builtins.len}
  2072041    1.611    0.000    3.312    0.000 {built-in method builtins.max}
431593/13870    0.448    0.000   15.487    0.001 {built-in method builtins.min}
        1    0.000    0.000    0.000    0.000 {built-in method builtins.print}
   912690    0.161    0.000    0.161    0.000 {built-in method builtins.sum}
 12625749    0.845    0.000    0.845    0.000 {method 'append' of 'list' objects}
    24437    0.002    0.000    0.002    0.000 {method 'bit_length' of 'int' objects}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
        1    0.000    0.000    0.000    0.000 {method 'extend' of 'list' objects}
    37109    0.007    0.000    0.007    0.000 {method 'getrandbits' of '_random.Random' objects}
       15    0.000    0.000    0.000    0.000 {method 'join' of 'str' objects}
    17011    0.002    0.000    0.002    0.000 {method 'random' of '_random.Random' objects}
       31    0.000    0.000    0.000    0.000 {method 'split' of 'str' objects}
       31    0.000    0.000    0.000    0.000 {method 'strip' of 'str' objects}
"""
