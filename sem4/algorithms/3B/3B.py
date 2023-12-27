BIG = 1_000_000_000_000
f = open("assignment.in")
n = int(f.readline())
c = [[0 for _ in range(n + 1)]]

for _ in range(n):
    mas = list(map(int, f.readline().split()))
    mas.insert(0, 0)
    c.append(mas)

row_w = [0 for _ in range(n + 1)]
col_w = [0 for _ in range(n + 1)]
p = [0 for _ in range(n + 1)]
way = [0 for _ in range(n + 1)]

for i in range(1, n + 1):
    p[0] = i
    j0 = 0
    minByCol = [BIG for _ in range(n + 1)]
    mark = [False for _ in range(n + 1)]
    while True:
        mark[j0] = True
        i0 = p[j0]
        delta = BIG
        j1 = 0
        for j in range(1, n + 1):
            if not mark[j]:
                cur = c[i0][j] - row_w[i0] - col_w[j]
                if cur < minByCol[j]:
                    minByCol[j] = cur
                    way[j] = j0
                if minByCol[j] < delta:
                    delta = minByCol[j]
                    j1 = j
        for j in range(n + 1):
            if mark[j]:
                row_w[p[j]] += delta
                col_w[j] -= delta
            else:
                minByCol[j] -= delta
        j0 = j1
        if p[j0] == 0:
            break
    while True:
        j1 = way[j0]
        p[j0] = p[j1]
        j0 = j1
        if j0 == 0:
            break

f = open("assignment.out", "w")
f.write(str(-col_w[0]) + "\n")
for j in range(1, n + 1):
    f.write(str(p[j]) + " " + str(j))
    f.write("\n")
f.close()