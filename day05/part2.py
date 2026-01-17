import sys
from collections import defaultdict

with open(sys.argv[1]) as f:
    lines = f.readlines()

db = defaultdict(int)

for line in lines:
    if "-" in line:
        start, end = map(int, line.strip().split("-"))
        db[start] += 1
        db[end + 1] -= 1
    else:
        break

r2 = 0
cur = 0
prev = 0

for idx, val in sorted(db.items()):
    cur += val
    if cur and not prev:
        prev = idx
    if prev and not cur:
        r2 += idx - prev
        prev = 0

print(r2)

