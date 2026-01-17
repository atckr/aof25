import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()

start = 50
r1 = 0
r2 = 0

for line in lines:
    line = line.strip()
    dist = int(line[1:])
    dir = 1 if line[0] == 'R' else -1
    needed = 100 if start == 0 else 100 - start if dir == 1 else start
    if dist >= needed:
        r2 += 1 + (dist - needed) // 100
    start = (start + dir * dist) % 100

    if start == 0:
        r1 += 1

print(r1)
print(r2)

