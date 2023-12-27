def dfs(v, dmin) -> int:
    if v == n - 1 or mark[v]:
        return dmin
    mark[v] = True
    for i in range(len(g[v])):
        u = g[v][i][0]
        cost = g[v][i][1]
        f = g[v][i][2]
        j = g[v][i][3]
        if not mark[u] and cost - f > 0:
            d = dfs(u, min(dmin, cost - f))
            if d > 0:
                g[v][i][2] += d
                g[u][j][2] -= d
                return d
    return 0


BIG = 1_000_000_000_000
n, m = map(int, input().split())
edges = [[-1 for _ in range(2)] for _ in range(m)]
g = [[] for _ in range(n)]
for i in range(m):
    aa, bb, cc = map(int, input().split())
    g[aa - 1].append([bb - 1, cc, 0, len(g[bb - 1])])
    edges[i][0] = aa - 1
    edges[i][1] = len(g[aa - 1]) - 1
    g[bb - 1].append([aa - 1, cc, 0, len(g[aa - 1]) - 1])

    # g[bb - 1].append([aa - 1, cc, 0, len(g[aa - 1])])
    # edges[i][2] = bb - 1
    # edges[i][3] = len(g[bb - 1]) - 1
    # g[aa - 1].append([bb - 1, 0, 0, len(g[bb - 1]) - 1])

answer = 0
while True:
    mark = [False for _ in range(n)]
    delta = dfs(0, BIG)
    if delta == 0:
        break
    answer += delta


mark = [False for _ in range(n)]
dfs(0, BIG)
answer_list = []
for i in range(m):
    edge = edges[i]
    if (mark[edge[0]] and not mark[g[edge[0]][edge[1]][0]]) or (not mark[edge[0]] and mark[g[edge[0]][edge[1]][0]]):
        answer_list.append(i + 1)

print(len(answer_list), answer)
print(*answer_list)
