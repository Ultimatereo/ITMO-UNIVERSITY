BIG = 10000000000000

def get_max_flow(n, m, edges):
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

    g = [set() for _ in range(n + m)]
    for i in range(n):
        for ver in edges[i]:
            new_no = ver + n
            g[i].add(new_no)
            g[new_no].add(i)

    p = [-1 for _ in range(n + m)]
    for v in range(n + m):
        mark = [False for _ in range(n + m)]
        dfs_m(v)

    count = 0
    for i in range(n):
        if p[i] != -1:
            count += 1
            p[p[i]] = i

    max_flow = []
    for i in range(n):
        if p[i] != -1:
            max_flow.append((i + 1, p[i] - n + 1))

    mark = [False for _ in range(n + m)]
    for v in range(n):
        if p[v] == -1 and not mark[v]:
            dfs(v, 0)

    l_plus = []
    for i in range(n):
        if mark[i]:
            l_plus.append(i + 1)
    r_plus = []
    for i in range(n, n + m):
        if mark[i]:
            r_plus.append(i - n + 1)
    return max_flow, l_plus, r_plus


f = open("assignment.in", "r")
n = int(f.readline())
c = [list(map(int, f.readline().split())) for _ in range(n)]
f.close()
row_w = [-min(c[i]) for i in range(n)]
# col_w = [-min([c[i][j] for i in range(n)]) for j in range(n)]
col_w = [0 for _ in range(n)]


while True:
    edges = [set() for _ in range(n)]
    for i in range(n):
        for j in range(n):
            if c[i][j] + row_w[i] + col_w[j] == 0:
                edges[i].add(j)
    max_flow, l_plus, r_plus = get_max_flow(n, n, edges)

    if len(max_flow) == n:
        s = 0
        for edge in max_flow:
            s += c[edge[0] - 1][edge[1] - 1]
        f = open("assignment.out", "w")
        f.write(str(s) + "\n")
        for edge in max_flow:
            f.write(str(edge[0]) + " " + str(edge[1]))
            f.write("\n")
        f.close()
        break

    min_value = BIG
    for i in l_plus:
        i -= 1
        for j in range(n):
            if c[i][j] + row_w[i] + col_w[j] > 0:
                min_value = min(min_value, c[i][j] + row_w[i] + col_w[j])

    for i in l_plus:
        row_w[i - 1] -= min_value
    for j in r_plus:
        col_w[j - 1] += min_value

