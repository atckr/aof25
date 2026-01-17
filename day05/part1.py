import sys
from collections import defaultdict

with open(sys.argv[1]) as f:
    dat = f.read().strip()


rgs, avail = dat.split("\n\n")

db = defaultdict(int)

for rg in rgs.split("\n"):
    r1, r2 = map(int, rg.split("-"))
    db[r1] += 1
    db[r2 + 1] -= 1

avail = sorted(list(map(int, avail.split("\n"))))

r1 = 0
cur = 0
prev = -1
ai = 0
an = len(avail)

for idx, val in sorted(db.items()):
    cur += val
    if cur and prev == -1:
        prev = idx
    if prev != -1:
        while avail[ai] < prev and ai < an:
            ai += 1
    if not cur:
        while avail[ai] >= prev and avail[ai] < idx and ai < an:
            r1 += 1
            ai += 1
        prev = -1

print(r1)
