def OneForAll(n, k, x):
    one = [0]*k
    for i in range(k):
        for j in range(n):
            if x[i][j] != -1:
                one[i] += 1
    bruh = [-1]*(n+1)
    key = 0
    for i in range(k):
        if one[i] == 1:
            for j in range(n):
                if x[i][j] != -1:
                    if ((bruh[j] != -1) and (bruh[j] != x[i][j])):
                        key = 1
                    else:
                        bruh[j] = x[i][j]
    #print(key)
    count = 0
    for i in range(k):
        if one[i] > 1:
            count = 0
            for j in range(n):
                if (((x[i][j] == 0) and (bruh[j] == 1)) or ((x[i][j] == 1) and (bruh[j] == 0)) or (x[i][j] == -1)):
                    #((bruh[j] != -1) and (bruh[j] != x[i][j])):
                    count += 1
            if count == n:
                key = 1
                break
    bruh[n] = key
    return bruh
def putOne(n, k, x, pp):
    for i in range(k):
        if x[i][pp] == 1:
            for j in range(n):
                x[i][j] = -1
        if x[i][pp] == 0:
            x[i][pp] = -1
    return x
def putZero(n, k, x, pp):
    for i in range(k):
        if x[i][pp] == 0:
            for j in range(n):
                x[i][j] = -1
        if x[i][pp] == 1:
            x[i][pp] = -1
    return x
n, k = map(int, input().split())
x = [[] for j in range(100)]
for i in range(k):
    x[i] = list(map(int, input().split()))
count = 0
while True:
    mas = OneForAll(n, k, x)
    #print(mas)
    if mas[n] == 1:
        print("YES")
        break
    count = 0
    for i in range(n):
        if mas[i] != -1:
            count += 1
    if count == 0:
        print("NO")
        break
    for i in range(n):
        if mas[i] == 0:
            x = putZero(n, k, x, i)
        if mas[i] == 1:
            x = putOne(n, k, x, i)
    #print("-------")
    #for i in range(k):
        #print(*x[i])
