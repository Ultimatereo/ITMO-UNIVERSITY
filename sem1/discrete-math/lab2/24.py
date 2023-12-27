def prevPerm(a):
    b = []
    for i in a:
        b.append(i)
    n = len(a)
    max = -1
    for i in range(n - 2, -1, -1):
        if a[i] > a[i+1]:
            max = i + 1
            for j in range(i + 1, n):
                if (a[j] > a[max]) and(a[j] < a[i]):
                    max = j
            b[i], b[max] = b[max], a[i]
            b = b[:i+1] + b[n - 1:i:-1]
            return b
    return [0]*n
def nextPerm(a):
    b = []
    for i in a:
        b.append(i)
    n = len(a)
    min = 1000000000
    for i in range(n - 2, -1, -1):
        if a[i] < a[i + 1]:
            min = i + 1
            for j in range(i + 1, n):
                if (a[j] < a[min]) and (a[j] > a[i]):
                    min = j
            b[i], b[min] = b[min], b[i]
            b = b[:i+1] + b[n - 1:i:-1]
            return b
    return [0]*n
f = open("nextperm.in", "r")
n = int(f.readline())
a = list(map(int, f.readline().split()))
f.close()
f = open("nextperm.out", "w")
f.write(' '.join(str(v) for v in prevPerm(a)))
f.write('\n')
f.write(' '.join(str(v) for v in nextPerm(a)))
f.close()
