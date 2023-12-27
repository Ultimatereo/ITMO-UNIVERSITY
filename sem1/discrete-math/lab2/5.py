def gray_code_recurse (g,n, base):
    k = len(g)
    if n <= 0:
        return

    else:
        beginIndex = k - 1
        endIndex = -1
        step = -1
        for t in range(1, base):
            for i in range (beginIndex, endIndex, step):
                char = str(t) + g[i]
                g.append(char)
            if beginIndex == k - 1:    
                beginIndex = 0
                endIndex = k
                step = 1
            else:
                beginIndex = k - 1
                endIndex = -1
                step = -1
        for i in range (k - 1, -1 , -1):
            g[i]= "0" + g[i]
        print(g)
        gray_code_recurse (g, n-1, base)
        
def gray_code(n, base):
    g = []
    for i in range(base):
        g.append(str(i))
    gray_code_recurse(g, n-1, base)
    return g

f = open("telemetry.in", "r")
n, base = map(int, f.readline().split())
f.close
a = gray_code(n, base)
f = open("telemetry.out", "w")
for i in range(len(a)):
    f.write(a[i] + '\n')
f.close()
