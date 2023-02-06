import math
n, m = map(int, input().split())
cost = [[]]*n
for i in range(n):
    cost[i] = list(map(int, input().split()))
d = [[0 for j in range(m)] for i in range(n)]
p = [["R" for j in range(m)] for i in range(n)]
for i in range(n):
    for j in range(m):
        if i == j == 0:
            d[i][j] = cost[i][j]
        else:
            d[i][j] = -math.inf
            if j > 0:
                d[i][j] = max(d[i][j], d[i][j - 1] + cost[i][j])
            if i > 0:
                d[i][j] = max(d[i][j], d[i - 1][j] + cost[i][j])
            if j == 0 or (j>0 and i > 0 and d[i][j - 1] <= d[i - 1][j]):
                p[i][j] = "D"       
path = ""
i = n - 1
j = m - 1
while i > 0 or j > 0:
    if p[i][j] == "D":
        path = "D" + path
        i -= 1
    else:
        path = "R" + path
        j -= 1
print(d[n-1][m-1])
print(path)
