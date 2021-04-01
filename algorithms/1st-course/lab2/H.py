import fileinput
 
def find(x):
    global p
    if x == p[x]:
        return x
    else:
        return find(p[x])
 
def union(x, y):
    global p, r, exp
    x = find(x)
    y = find(y)
    if x == y:
        return p, r, exp
 
    if r[x] < r[y]:
        x, y = y, x
    p[y] = x
    exp[y] -= exp[x]
    
    if (r[x] == r[y]):
        r[x] += 1
    return p, r, exp
 
def countExp(x):
    global exp, p
    if x == p[x]:
        return exp[x]
    else:
        return countExp(p[x]) + exp[x]
 
n, m = map(int, input().split())
r = [0] * (n + 1)
p = [0] * (n + 1)
exp = [0] * (n + 1)
for i in range(n + 1):
    p[i] = i
    
for i in range(m):
    lst = list(map(str, input().split()))
    if lst[0] == "add":
        exp[find(int(lst[1]))] += int(lst[2])
    elif lst[0] == "join":
        union(int(lst[1]), int(lst[2]))
    else:
        print(countExp(int(lst[1])))
