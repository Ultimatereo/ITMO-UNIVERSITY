from random import randint
fflag = False
while True:
    f = open("test.txt", "w")
    k = 1
    f.write(str(k) + "\n")
    m = randint(1, 10)
    n = randint(1, 10)
    f.write(str(m) + " " + str(n) + "\n")

    for _ in range(m):
        mas = []
        for i in range(1, n + 1):
            if randint(0, 100) % 2 == 0:
                mas.append(str(i))
        mas.append('0')
        f.write(' '.join(mas))
        f.write("\n")
    f.close()

    # --------

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
        for u in g[v]:
            if not mark[u]:
                if code == 0 and p[u] != v:
                    dfs(u, 1)
                elif code == 1 and p[u] == v:
                    dfs(u, 0)


    f = open("test.txt", "r")
    ff = open("smart.txt", "w")
    # k = int(input())
    k = int(f.readline())
    for _ in range(k):
        # n, m = map(int, input().split())
        n, m = map(int, f.readline().split())
        edges = [set(map(int, f.readline().split())) for _ in range(n)]
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

        mark = [False for _ in range(n + m)]
        for v in range(n):
            if p[v] == -1 and not mark[v]:
                dfs(v, 0)

        men = []
        for i in range(n):
            if mark[i]:
                men.append(i)
        women = []
        for i in range(n, n + m):
            if not mark[i]:
                women.append(i)
        for man in men:
            for woman in women:
                if woman in g[man]:
                    print("THE FUCK")
                    fflag = True
                    break
            if fflag:
                break
        if fflag:
            break
    if fflag:
        break


