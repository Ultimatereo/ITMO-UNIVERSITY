def gen(f, n, prefix):
    if len(prefix) == 0:
        beginIndex = 1
        f.write('\n')
    else:
        beginIndex = prefix[-1] + 1
        for i in range(len(prefix)):
            f.write(str(prefix[i]) + " ")
        f.write('\n')
    for i in range(beginIndex, n + 1):
        gen(f, n, prefix + [i])

f = open("subsets.in", "r")
n = int(f.readline())
f.close()
f = open("subsets.out", "w")
gen(f, n, [])
f.close()
