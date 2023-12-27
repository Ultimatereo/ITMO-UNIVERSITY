import copy
import itertools

f = open("smart.txt", "r")
ff = int(f.readline())
mas = [list(line.rstrip()) for line in f]
f.close()
if ff == -1:
    f = open("check.txt", "w")
    f.write(str(True))
    f.close()
else:
    n = len(mas)
    m = len(mas[0])
    start = ()
    end = ()


    def check(new_mas, start, end):
        mark = [[False for _ in range(len(new_mas[0]))] for _ in range(len(new_mas))]

        def dfs(start):
            i = start[0]
            j = start[1]
            if mark[i][j]:
                return
            mark[i][j] = True
            if i > 0 and new_mas[i - 1][j] in ['.', '-', 'A', 'B']:
                dfs((i - 1, j))
            if j > 0 and new_mas[i][j - 1] in ['.', '-', 'A', 'B']:
                dfs((i, j - 1))
            if i + 1 < n and new_mas[i + 1][j] in ['.', '-', 'A', 'B']:
                dfs((i + 1, j))
            if j + 1 < m and new_mas[i][j + 1] in ['.', '-', 'A', 'B']:
                dfs((i, j + 1))

        dfs(start)
        return not mark[end[0]][end[1]]


    for i in range(n):
        for j in range(m):
            if mas[i][j] == 'A':
                start = (i, j)
            if mas[i][j] == 'B':
                end = (i, j)

    f = open("check.txt", "w")
    f.write(str(check(mas, start, end)))
    f.close()