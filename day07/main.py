import sys

with open(sys.argv[1]) as f:
    lines = f.read().strip().splitlines()

m = len(lines[0])
state = [0] * m
res = 0

def update_beam(line, j, x):
    if 0 <= j < m and line[j] == ".":
        state[j] += x

for line in lines:
    for j in range(m):
        if line[j] == "^":
            if state[j]:
                res += 1
                update_beam(line, j - 1, state[j])
                update_beam(line, j + 1, state[j])
                state[j] = 0
        elif line[j] == "S":
            state[j] = 1

print(res)
print(sum(state))

