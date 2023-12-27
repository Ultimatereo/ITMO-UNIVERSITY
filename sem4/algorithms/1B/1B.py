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


# f = open("test.txt", "r")
# ff = open("smart.txt", "w")
k = int(input())
# k = int(f.readline())
for _ in range(k):
    n, m = map(int, input().split())
    # n, m = map(int, f.readline().split())
    edges = [set(map(int, input().split())) for _ in range(n)]
    for line in edges:
        line.remove(0)
    g = [set() for _ in range(n + m)]
    for j in range(n):
        g[j] = set(i for i in range(n, n + m))
    for j in range(n, n + m):
        g[j] = set(i for i in range(n))
    for i in range(n):
        for ver in edges[i]:
            new_no = ver + n - 1
            g[i].remove(new_no)
            g[new_no].remove(i)

    p = [-1 for _ in range(n + m)]
    for v in range(n + m):
        mark = [False for _ in range(n + m)]
        dfs_m(v)

    # count = 0
    # for i in range(n):
    #     if p[i] != -1:
    #         count += 1
    #
    # print(count)
    # for i in range(n):
    #     if p[i] != -1:
    #         print(i + 1, p[i] - n + 1)
    # print()
    for i in range(n):
        if p[i] != -1:
            p[p[i]] = i

    mark = [False for _ in range(n + m)]
    for v in range(n):
        if p[v] == -1 and not mark[v]:
            dfs(v, 0)


    men = []
    for i in range(n):
        if mark[i]:
            men.append(str(i + 1))
    women = []
    for i in range(n, n + m):
        if not mark[i]:
            women.append(str(i - n + 1))
    print(len(men) + len(women))
    print(len(men), len(women))
    print(*men)
    print(*women)
    print()
#     ff.write(str(len(men) + len(women)))
#     ff.write("\n")
#     ff.write(str(len(men)) + " " + str(len(women)))
#     ff.write("\n")
#     ff.write(" ".join(men))
#     ff.write("\n")
#     ff.write(" ".join(women))
#     ff.write("\n")
#     ff.write("\n")
# f.close()
# ff.close()