def nextMPerm(b):
    n = len(a)
    i = n - 2
    while (i >= 0) and (b[i] >= b[i + 1]):
        i -= 1
    if i >= 0:
        j = i + 1
        while (j < n - 1) and (b[j + 1] > b[i]):
            j += 1
        b[i], b[j] = b[j], b[i]
        b = b[:i + 1] + b[n - 1: i: -1]
        return b
    else:
        return [0]*n
    
f = open("nextmultiperm.in", "r")
n = int(f.readline())
a = list(map(int, f.readline().split()))
f.close()
f = open("nextmultiperm.out", "w")
f.write(' '.join(str(v) for v in nextMPerm(a)))
f.close()
