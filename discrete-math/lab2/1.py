f = open("allvectors.in", "r")
n = int(f.readline())
f.close()
f = open("allvectors.out", "w")

for i in range(2**n):
    k = bin(i)[2:]
    t = n - len(k)
    f.write("0"*t + k + '\n')
f.close()
