import math
import sys
from collections import defaultdict

with open(sys.argv[1]) as f:
    lines = f.read().strip().splitlines()

min_row = defaultdict(lambda: math.inf)
max_row = defaultdict(lambda: -1)

for line in lines:
    c, r = map(int, line.split(","))
    min_row[c] = min(min_row[c], r)
    max_row[c] = max(max_row[c], r)

def calculate_edge(l, r):
    return abs(l - r) + 1

def calculate_rec(i1, j1, i2, j2):
    if 0 <= i1 < math.inf and 0 <= i2 < math.inf:
        h = calculate_edge(i1, i2)
        w = calculate_edge(j1, j2)
        return h * w
    return 0

res = 0

for c1 in min_row.keys():
    for c2 in min_row.keys():
        res1 = calculate_rec(min_row[c1], c1, max_row[c2], c2)
        res2 = calculate_rec(max_row[c1], c1, min_row[c2], c2)
        res = max(res, res1, res2)

print(res)
