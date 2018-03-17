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
         232066064 function calls (218883729 primitive calls) in 90.451 seconds

   Ordered by: standard name

   ncalls  tottime  percall  cumtime  percall filename:lineno(function)
        1    0.000    0.000  105.706  105.706 <string>:1(<module>)
        1    0.000    0.000    0.000    0.000 codecs.py:318(decode)
        1    0.000    0.000    0.000    0.000 codecs.py:330(getstate)
       16    0.000    0.000    0.000    0.000 l2z1.py:10(<genexpr>)
      114    0.000    0.000    0.000    0.000 l2z1.py:4(<genexpr>)
       31    0.000    0.000    0.000    0.000 l2z1.py:4(parse_line_ints)
        1    0.000    0.000    0.000    0.000 l2z1.py:7(load_input)
       16    0.000    0.000    0.000    0.000 l2z1.py:9(<genexpr>)
  7905090    5.196    0.000   85.280    0.000 nonograms.py:112(opt_dist_2d)
        1    0.000    0.000    0.000    0.000 nonograms.py:123(__init__)
      3/1    0.357    0.119  105.705  105.705 nonograms.py:16(solve)
    30766    0.145    0.000    0.145    0.000 nonograms.py:30(<listcomp>)
        1    0.000    0.000    0.000    0.000 nonograms.py:46(print_matrix)
       16    0.000    0.000    0.000    0.000 nonograms.py:47(<genexpr>)
   147135    0.103    0.000    9.593    0.000 nonograms.py:50(_get_wrongs)
   147135    1.826    0.000    9.490    0.000 nonograms.py:51(<listcomp>)
  1861888    7.756    0.000   93.380    0.000 nonograms.py:53(_iter_cost_j)
   147134    0.732    0.000    1.439    0.000 nonograms.py:67(_neg)
        3    0.000    0.000    0.001    0.000 nonograms.py:75(_reinitialize)
  6643381    7.627    0.000   60.118    0.000 nonograms.py:83(opt_dist)
 40405554   14.139    0.000   41.992    0.000 nonograms.py:84(<genexpr>)
1368094/91263    1.097    0.000   72.584    0.001 nonograms.py:89(_dp_opt_dist_2d)
        1    0.000    0.000    0.000    0.000 nonograms.py:9(__init__)
8011475/839972    6.796    0.000   72.343    0.000 nonograms.py:97(_dp)
    31441    0.023    0.000    0.048    0.000 random.py:172(randrange)
      675    0.000    0.000    0.001    0.000 random.py:216(randint)
   209342    0.165    0.000    0.237    0.000 random.py:222(_randbelow)
   177901    0.108    0.000    0.335    0.000 random.py:252(choice)
       45    0.000    0.000    0.001    0.000 util.py:10(<listcomp>)
       45    0.000    0.000    0.000    0.000 util.py:13(get_column)
       45    0.000    0.000    0.000    0.000 util.py:14(<listcomp>)
        3    0.000    0.000    0.000    0.000 util.py:17(get_columns)
        3    0.000    0.000    0.000    0.000 util.py:18(<listcomp>)
  3785308    1.581    0.000    1.581    0.000 util.py:22(neg_kth)
 60564928    7.121    0.000    7.121    0.000 util.py:23(<genexpr>)
11272125/7905090    7.663    0.000   77.806    0.000 util.py:31(aux)
        3    0.000    0.000    0.001    0.000 util.py:9(init_random_matrix)
        1    0.000    0.000    0.000    0.000 {built-in method _codecs.utf_8_decode}
        1    0.000    0.000  105.706  105.706 {built-in method builtins.exec}
       31    0.000    0.000    0.000    0.000 {built-in method builtins.input}
 32250669    2.159    0.000    2.159    0.000 {built-in method builtins.len}
  6643381    5.606    0.000   47.598    0.000 {built-in method builtins.max}
1484462/117498    1.837    0.000   94.760    0.001 {built-in method builtins.min}
        1    0.000    0.000    0.000    0.000 {built-in method builtins.print}
 48310644   18.322    0.000   33.576    0.000 {built-in method builtins.sum}
   209342    0.018    0.000    0.018    0.000 {method 'bit_length' of 'int' objects}
        1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
        3    0.000    0.000    0.000    0.000 {method 'extend' of 'list' objects}
   310595    0.055    0.000    0.055    0.000 {method 'getrandbits' of '_random.Random' objects}
       15    0.000    0.000    0.000    0.000 {method 'join' of 'str' objects}
   147134    0.017    0.000    0.017    0.000 {method 'random' of '_random.Random' objects}
       31    0.000    0.000    0.000    0.000 {method 'split' of 'str' objects}
       31    0.000    0.000    0.000    0.000 {method 'strip' of 'str' objects}
"""
