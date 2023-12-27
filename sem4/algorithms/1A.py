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

    for line in edges:
        line.remove(0)
    g = [set() for _ in range(n + m)]
    for i in range(n):
        for ver in edges[i]:
            new_no = ver + n - 1
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


n, m = map(int, input().split())
edges = [set(map(int, input().split())) for _ in range(n)]
max_flow, _, _ = get_max_flow(n, m, edges)
print(len(max_flow))
for edge in max_flow:
    print(*edge)
