def opt_dist(s, k):  # ones_zeros_str, ones_len
    best_sum = max(sum(int(x) for x in s[i:i + k])
                   for i in range(len(s) - k + 1))  # all substr beginnings
    return sum(map(int, s)) - best_sum + k - best_sum
