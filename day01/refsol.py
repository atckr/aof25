import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()

position = 50
r1 = 0
r2 = 0

for line in lines:
    line = line.strip()
    current_count = int(line[1:])
    current_dir = 1 if line[0] == 'R' else -1
    needed = 100 if position == 0 else 100 - position if current_dir == 1 else position

    if current_count >= needed:
        r2 += 1
        current_count -= needed

        while current_count >= 100:
            current_count -= 100
            r2 += 1

        if current_dir == -1:
            current_count = 100 - current_count
    else:
        current_count = 100 + position + current_count * current_dir

    position = current_count - 100 if current_count >= 100 else current_count

    if position == 0:
        r1 += 1

print(r1)
print(r2)

