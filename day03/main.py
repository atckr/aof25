import sys

with open(sys.argv[1]) as f:
    lines = f.readlines()

lines = [[int(x) for x in line.strip()] for line in lines]

def largest(lines, jlen):
    res = 0

    for line in lines:
        n = len(line)
        cur = [0] * jlen
        not_set = 0

        for i in range(n):
            for j in range(jlen):
                if (j == not_set or line[i] > cur[j]) and n - i >= jlen - j:
                    cur[j] = line[i]
                    not_set = j + 1
                    break
        res += int("".join([str(x) for x in cur]))
    return res

print(largest(lines, 2))
print(largest(lines, 12))
