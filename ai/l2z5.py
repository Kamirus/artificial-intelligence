from commando import CommandoReducer, CommandoSeeker, split_possible_commandos


def main() -> None:
    with open('zad_output.txt', 'w') as f:
        c = CommandoReducer(debug=False)
        # moves = c.reduce()

        board, state = split_possible_commandos(c.full_map)
        res = CommandoSeeker(board, c.targets, state, '').search_astar()

        print(res, file=f)


if __name__ == '__main__':
    main()
    # import cProfile
    # cProfile.run('main()')

# 157601507 function calls(143744963 primitive calls) in 104.229 seconds

#    Ordered by: standard name

#    ncalls  tottime  percall  cumtime  percall filename: lineno(function)
#         1    0.000    0.000  104.229  104.229 < string > : 1( < module > )
#         2    0.000    0.000    0.000    0.000 _bootlocale.py: 23(getpreferredencoding)
#         1    0.000    0.000    0.000    0.000 codecs.py: 185(__init__)
#         1    0.000    0.000    0.000    0.000 codecs.py: 259(__init__)
#         1    0.000    0.000    0.000    0.000 codecs.py: 308(__init__)
#         2    0.000    0.000    0.000    0.000 codecs.py: 318(decode)
#         1    0.000    0.000    0.292    0.292 commando.py: 116(__init__)
#         1    0.142    0.142  103.933  103.933 commando.py: 123(search)
#   3525248    2.290    0.000   20.976    0.000 commando.py:135(f)
#   3525248    4.125    0.000   15.525    0.000 commando.py:138(h)
#         1    0.000    0.000    0.000    0.000 commando.py:14(raw_load)
#  17381792    4.986    0.000    8.705    0.000 commando.py:140(<genexpr>)
#         1    0.048    0.048    0.292    0.292 commando.py:142(calc_min_distances_to_any_target)
#       124    0.000    0.000    0.000    0.000 commando.py:164(min_distance_to_any_target)
#  14507510   11.675    0.000   33.968    0.000 commando.py:169(next_states)
#  11606008   21.418    0.000   21.418    0.000 commando.py:173(<setcomp>)
#     67280    0.028    0.000    0.092    0.000 commando.py:179(get_neighbors)
#   2888047    1.798    0.000    1.798    0.000 commando.py:184(is_end)
#         1    0.000    0.000    0.000    0.000 commando.py:33(split_possible_commandos)
#         9    0.000    0.000    0.000    0.000 commando.py:43(<listcomp>)
#         1    0.000    0.000    0.000    0.000 commando.py:58(__init__)
#         1    0.003    0.003  104.229  104.229 l2z5.py:4(main)
#   3538703    5.237    0.000   13.546    0.000 queue.py:115(put)
#   2901502    4.755    0.000   16.549    0.000 queue.py:147(get)
#         2    0.000    0.000    0.000    0.000 queue.py:220(_init)
#   2914958    0.764    0.000    1.135    0.000 queue.py:223(_qsize)
#   3538703    0.887    0.000    1.629    0.000 queue.py:226(_put)
#   2901502    0.639    0.000    5.620    0.000 queue.py:229(_get)
#         2    0.000    0.000    0.000    0.000 queue.py:27(__init__)
#     13456    0.008    0.000    0.013    0.000 queue.py:90(empty)
#         6    0.000    0.000    0.000    0.000 threading.py:215(__init__)
#   6440205    1.933    0.000    2.912    0.000 threading.py:239(__enter__)
#   6440205    1.449    0.000    1.897    0.000 threading.py:242(__exit__)
#   6440205    1.533    0.000    2.662    0.000 threading.py:254(_is_owned)
#   6440205    4.253    0.000    6.914    0.000 threading.py:334(notify)
#         3    0.000    0.000    0.000    0.000 typing.py:1184(_generic_new)
#         3    0.000    0.000    0.000    0.000 typing.py:1223(__new__)
#         1    0.000    0.000  103.933  103.933 util.py:107(search_astar)
# 17381792/3525248    6.505    0.000   18.312    0.000 util.py:34(aux)
#         2    0.000    0.000    0.000    0.000 util.py:52(__init__)
#   3538703    2.200    0.000   15.746    0.000 util.py:57(push)
#   2901502    1.330    0.000   17.879    0.000 util.py:61(pop)
#         1   13.613   13.613  103.791  103.791 util.py:80(search)
#         3    0.000    0.000    0.000    0.000 {built-in method __new__ of type object at 0x9d7d40}
#         2    0.000    0.000    0.000    0.000 {built-in method _codecs.utf_8_decode}
#   2901502    4.981    0.000    4.981    0.000 {built-in method _heapq.heappop}
#   3538703    0.742    0.000    0.742    0.000 {built-in method _heapq.heappush}
#         2    0.000    0.000    0.000    0.000 {built-in method _locale.nl_langinfo}
#         2    0.000    0.000    0.000    0.000 {built-in method _thread.allocate_lock}
#         1    0.000    0.000  104.229  104.229 {built-in method builtins.exec}
#   6440208    0.744    0.000    0.744    0.000 {built-in method builtins.len}
#   3525248    2.696    0.000   11.400    0.000 {built-in method builtins.max}
#     40368    0.009    0.000    0.009    0.000 {built-in method builtins.min}
#         1    0.000    0.000    0.000    0.000 {built-in method builtins.print}
#         2    0.000    0.000    0.000    0.000 {built-in method io.open}
#         1    0.000    0.000    0.000    0.000 {built-in method maketrans}
#   6440205    0.980    0.000    0.980    0.000 {method '__enter__' of '_thread.lock' objects}
#   6440205    0.448    0.000    0.448    0.000 {method '__exit__' of '_thread.lock' objects}
#   6440205    1.129    0.000    1.129    0.000 {method 'acquire' of '_thread.lock' objects}
#         9    0.000    0.000    0.000    0.000 {method 'add' of 'set' objects}
#        18    0.000    0.000    0.000    0.000 {method 'append' of 'list' objects}
#         1    0.000    0.000    0.000    0.000 {method 'disable' of '_lsprof.Profiler' objects}
#   2941870    0.881    0.000    0.881    0.000 {method 'get' of 'dict' objects}
#         9    0.000    0.000    0.000    0.000 {method 'translate' of 'str' objects}
