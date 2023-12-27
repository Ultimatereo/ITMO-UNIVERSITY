def nextChoose(a, n, k):
    b = [0]*(k + 1)
    for i in range(k):
        b[i] = a[i]
    b[k] = n + 1
    i = k - 1
    while (i >= 0) and (b[i+1] - b[i] < 2):
        i -= 1
    if i >= 0:
        b[i] += 1
        for j in range(i + 1, k):
            b[j] = b[j-1] + 1
        for t in range(k):
            a[t] = b[t]
        return a
    else:
        return [-1]

f = open("nextchoose.in", "r")
n, k = map(int, f.readline().split())
a = list(map(int, f.readline().split()))
f.close()
f = open("nextchoose.out", "w")
f.write(' '.join(str(v) for v in nextChoose(a, n, k)))
f.close()
