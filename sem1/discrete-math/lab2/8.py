def gen(f, n, k, prefix):
    if len(prefix) == k:
        for i in range(k):
            f.write(str(prefix[i]) + " ")
        f.write('\n')
        prefix = []
    else:
        if len(prefix) == 0:
            beginIndex = 1
        else:
            beginIndex = prefix[-1] + 1
        
        temp = 0
        if len(prefix) == k - 1:
            temp = 1 
        for i in range(beginIndex, n + temp):
            gen(f, n, k, prefix + [i])

f = open("testin.txt", "r")
n, k = map(int, f.readline().split())
f.close()
f = open("testout.txt", "w")
gen(f, n, k, [])
f.close()
