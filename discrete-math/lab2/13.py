from math import factorial
def numToPerm(n, k):
    permutation = [0]*(n+1)
    was = [False]*(n+1)
    for i in range(1, n + 1):
        alreadyWas = k // factorial(n - i)
        k %= factorial(n - i)
        curFree = 0
        for j in range(1, n + 1):
            if was[j] == False:
                curFree += 1;
                if curFree == alreadyWas + 1:
                    permutation[i] = j
                    was[j] = True
    return permutation[1:]

f = open("testin.txt", "r")
n, k = map(int, f.readline().split())
f.close()
f = open("testout.txt", "w")
a = numToPerm(n, k)
for j in range(len(a)):
    f.write(str(a[j]) + " ")
f.close()
