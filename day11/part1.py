import sys

with open(sys.argv[1]) as f:
    lines = f.read().strip().splitlines()

adj = defaultdict(list)

for line in lines:
    node, neighbours = line.split(':')
    for neighbour in neighbours.split(' '):
        adj[node.strip()].append(neighbour.strip())

dp = defaultdict(int)
dp["you"] = 1

def dfs(node):
    for nb in adj[node]:
        if nb not in visited:
            visited.add(nb)
            dp[nb] += dp[node]
            dfs(nb)

