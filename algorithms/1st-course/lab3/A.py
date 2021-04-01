n, k = map(int, input().split())
cost = [0, 0] + list(map(int, input().split())) + [0]
d = [0]*(n + 1)
p = [0]*(n + 1)
for i in range(2, n + 1):
    numMax = i - 1
    for j in range(max(1, i - k), i):
        if d[j] > d[numMax]:
            numMax = j
    d[i] = d[numMax] + cost[i]
    p[i] = numMax
print(d[n])
path = []
i = n
while i > 1:
    path = [i] + path
    i = p[i]
path = [i] + path
print(len(path) - 1)
print(' '.join(str(v) for v in path))
