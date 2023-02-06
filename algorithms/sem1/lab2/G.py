import fileinput
 
def get(x):
    global p, Min, Max, kolvo
    root = x
    while p[root] != root:
        root = p[root]
    i = x
    while p[i] != i:
        j = p[i]
        p[i] = root
        i = j
    return root
def union(x, y):
    global p, r, Min, Max, kolvo
    x = get(x)
    y = get(y)
    if x == y:
        return p, r
    if r[x] == r[y]:
        r[x] += 1
    if r[x] < r[y]:
        p[x] = y
        Min[y] = min(Min[y], Min[x])
        Max[y] = max(Max[y], Max[x])
        kolvo[y] += kolvo[x]
    else:
        p[y] = x
        Min[x] = min(Min[y], Min[x])
        Max[x] = max(Max[y], Max[x])
        kolvo[x] += kolvo[y] 
    return p, r
    
n = int(input())
r = [0] * (n + 1)
p = [0] * (n + 1)
Min = [0] * (n + 1)
Max = [0] * (n + 1)
kolvo = [1] * (n + 1)
for i in range(n + 1):
    p[i] = i
    Min[i] = i
    Max[i] = i
for line in fileinput.input():
    lst = line.split()
    if lst[0] == "union":
        union(int(lst[1]), int(lst[2]))
    else:
        tmp = get(int(lst[1]))
        print(Min[tmp], Max[tmp], kolvo[tmp])
 
