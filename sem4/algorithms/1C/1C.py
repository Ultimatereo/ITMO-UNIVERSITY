def dfs_m(v: int) -> bool:
    if mark[v]:
        return False
    mark[v] = True
    for u in g[v]:
        if p[u] == -1 or dfs_m(p[u]):
            p[u] = v
            return True
    return False


def dfs(v: int, code: int):
    mark[v] = True
    if code == 1 and p[v] != -1:
        if not mark[p[v]]:
            dfs(p[v], 0)
            return
    if code == 0:
        for u in g[v]:
            if not mark[u] and p[v] != u:
                dfs(u, 1)


BIG = 1_000_000_000_000
f = open("hobbits.in", "r")
n = int(f.readline())
trans_mas = [list(map(int, f.readline().split())) for _ in range(n)]
f.close()
# Transitive shit

for i in range(n):
    for j in range(n):
        if trans_mas[i][j] == 0:
            trans_mas[i][j] = BIG

for k in range(n):
    for i in range(n):
        for j in range(n):
            trans_mas[i][j] = min(trans_mas[i][j], trans_mas[i][k] + trans_mas[k][j])

# Construct bipartite graph

g = [set() for _ in range(2 * n)]
for i in range(n):
    for j in range(n):
        if trans_mas[i][j] <= n:
            g[i].add(n + j)
            g[n + j].add(i)

# Find max independent set

p = [-1 for _ in range(2 * n)]
for v in range(2 * n):
    mark = [False for _ in range(2 * n)]
    dfs_m(v)

for i in range(n):
    if p[i] != -1:
        p[p[i]] = i

mark = [False for _ in range(2 * n)]
for v in range(n):
    if p[v] == -1 and not mark[v]:
        dfs(v, 0)

c = set()
for i in range(n):
    if not mark[i]:
        c.add(str(i + 1))
for i in range(n, 2 * n):
    if mark[i]:
        c.add(str(i + 1 - n))

# Get complement to MIS and be happy
a = set(str(i) for i in range(1, n + 1)).difference(c)

f = open("hobbits.out", "w")
f.write(str(len(a)) + "\n")
f.write(" ".join(sorted(a)))
f.close()
