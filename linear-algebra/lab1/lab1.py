def sum(a, b):
    c = [[0] * len(a[0]) for _ in range(len(a))]
    for i in range(len(c)):
        for j in range(len(c[0])):
            c[i][j] = a[i][j] + b[i][j]
    return c
def multiconst(a, const):
    c = [[0] * len(a[0]) for _ in range(len(a))]
    for i in range(len(c)):
        for j in range(len(c[0])):
            c[i][j] = const * a[i][j]
    return c
def multi(a, b):
    c = [[0] * len(b[0]) for _ in range(len(a))]
    for i in range(len(a)):
        for j in range(len(b[0])):
            for k in range(len(b)):
                c[i][j] += a[i][k] * b[k][j]
    return c
def trans(a):
    c = [[0] * len(a) for _ in range(len(a[0]))]
    for i in range(len(a)):
        for j in range(len(a[0])):
            c[j][i] = a[i][j]
    return c

f = open("input.txt", 'r')

alpha, beta = map(float, f.readline().split())

na, ma = map(int, f.readline().split())
A = [[0.0] * ma for _ in range(na)]
line = f.readline()
array = list(map(float, line.split()))
count = 0
for i in range(na):
    for j in range(ma):
        A[i][j] = array[count]
        count += 1

na, ma = map(int, f.readline().split())
B = [[0.0] * ma for _ in range(na)]
line = f.readline()
array = list(map(float, line.split()))
count = 0
for i in range(na):
    for j in range(ma):
        B[i][j] = array[count]
        count += 1
        
na, ma = map(int, f.readline().split())
C = [[0.0] * ma for _ in range(na)]
line = f.readline()
array = list(map(float, line.split()))
count = 0
for i in range(na):
    for j in range(ma):
        C[i][j] = array[count]
        count += 1
        
na, ma = map(int, f.readline().split())
D = [[0.0] * ma for _ in range(na)]
line = f.readline()
array = list(map(float, line.split()))
count = 0
for i in range(na):
    for j in range(ma):
        D[i][j] = array[count]
        count += 1

na, ma = map(int, f.readline().split())
F = [[0.0] * ma for _ in range(na)]
line = f.readline()
array = list(map(float, line.split()))
count = 0
for i in range(na):
    for j in range(ma):
        F[i][j] = array[count]
        count += 1
f.close()
f = open("output.txt", "w")
try:
    X = sum(multi(multi(C, trans(sum(multiconst(A, alpha), multiconst(trans(B), beta)))), D), multiconst(F, -1))
    f.write("1" + '\n')
    f.write(str(len(X)) + " " + str(len(X[0])) + '\n')
    for i in range(len(X)):
        f.write(" ".join(map(str, X[i])) + ' ')
except IndexError:
    f.write("0" + '\n')
finally:
    f.close()
