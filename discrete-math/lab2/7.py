def gen(f, n, prefix):
    if len(prefix) == n:
        for i in range(n):
            f.write(prefix[i] + " ")
        f.write('\n')
    for i in range(1, n + 1):
        if str(i) not in prefix:
            gen(f, n, prefix + str(i))


f = open("testin.txt", "r")
n = int(f.readline())
f.close()
f = open("testout.txt", "w")
gen(f, n, "")
f.close()
