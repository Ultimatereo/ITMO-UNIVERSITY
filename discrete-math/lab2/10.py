def partition(a, n, p):
    print(a, n)
    if n:
        for i in range(n+1, 0, -1):
            if i <= p:
                partition(a+[i], n-i, i)
    else:
        print(*a, sep='+')
 
 
partition([], 4, 4)

f = open("testin.txt", "r")
n = int(f.readline())
f.close()
f = open("testout.txt", "w")
f.close()
