global g
def vectorsRecurse(g, n):
    k = len(g)
    if n <= 0:
        return g
    else:
        new = []
        for i in range(k):
            char = "0" + g[i]
            new.append(char)
        for i in range(k):
            if g[i][0] == "0":
                char = "1" + g[i]
                new.append(char)
        g = new
    return vectorsRecurse(g, n - 1)
def vectors(n):
    g = vectorsRecurse(["0", "1"], n - 1)
    return g

f = open("vectors.in", "r")
n = int(f.readline())
f.close
a = vectors(n)
f = open("vectors.out", "w")
f.write(str(len(a)) + '\n')
for i in range(len(a)):
    f.write(a[i] + '\n')
f.close()
