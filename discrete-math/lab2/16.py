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

def chooseToNum(n, k, choose):
    choose.insert(0, 0)
    num = 0
    for i in range(1, k + 1):
        for j in range(choose[i - 1] + 1, choose[i]):
            num += Binc(bcs, n - j, k - i)
    return num

f = open("testin.txt", "r")
n, k = map(int, f.readline().split())
choose = list(map(int, f.readline().split()))
f.close()
f = open("testout.txt", "w")
f.write(str(chooseToNum(n, k, choose)))
f.close()    
