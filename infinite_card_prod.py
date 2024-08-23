import itertools
from itertools import count

def infinite_cart_prod(i, j):
    i_reversed = []
    j_forward = []
    n = 0
    while True:
        n += 1
        i_reversed.append(next(i))
        j_forward.append(next(j))
        for k in range(n):
            yield (i_reversed[n-1-k], j_forward[k])

result = list(itertools.islice(infinite_cart_prod(count(0), count(0)), 1000000))
print('done')

