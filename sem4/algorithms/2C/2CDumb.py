import copy
import itertools

f = open("test.txt", "r")
n, m = map(int, f.readline().split())
mas = [list(f.readline().strip()) for _ in range(n)]
f.close()

dots = []
start = ()
end = ()
for i in range(n):
    for j in range(m):
        if mas[i][j] == '.':
            dots.append((i, j))
        elif mas[i][j] == 'A':
            start = (i, j)
        elif mas[i][j] == 'B':
            end = (i, j)
answer = -1
mas_ans = copy.deepcopy(mas)


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

flag = False
for number in range(len(dots) + 1):
    for combination in itertools.combinations(range(len(dots)), number):
        new_mas = copy.deepcopy(mas)
        for dot in combination:
            new_mas[dots[dot][0]][dots[dot][1]] = '+'
        if check(new_mas, start, end):
            answer = number
            mas_ans = new_mas
            flag = True
            break
    if flag:
        break


f = open("dumb.txt", "w")
f.write(str(answer) + "\n")
if answer != -1:
    for line in mas_ans:
        f.write(''.join(line) + "\n")
f.close()
