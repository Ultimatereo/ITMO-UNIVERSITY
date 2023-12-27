def Binc(bcs,n,k):
    if (k>n):   return 0
    if k>n//2:  k=n-k
    if k==0:    return 1
    if k==1:    return n
    while len(bcs)<n-3:
        for i in range(len(bcs),n-3):
            r=[]
            for j in range(2,i//2+3):
                r.append(Binc(bcs,i+3,j-1)+Binc(bcs,i+3,j))
            bcs.append(r)
    r=bcs[n-4]
    if len(r)<k-1:
        for i in range(len(r),k-1):
            r.append(Binc(bcs,n-1,k-1)+Binc(bcs,n-1,k))
    return bcs[n-4][k-2] 

bcs = []
def numToChoose(n, k, m):
    choose = []
    next = 1
    while k > 0:
        if m < Binc(bcs, n - 1, k - 1):
            choose = choose + [next]
            k -= 1
        else:
            m -= Binc(bcs, n - 1, k - 1)
        n -= 1
        next += 1
    return choose

f = open("testin.txt", "r")
n, k, m = map(int, f.readline().split())
f.close()
f = open("testout.txt", "w")
a = numToChoose(n, k, m)
for i in range(len(a)):
    f.write(str(a[i]) + " ")
f.close()
