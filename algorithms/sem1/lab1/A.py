def merge(a,b):
    n = len(a)
    m = len(b)
    i = 0
    j = 0
    c = []
    while i<n or j<m:
        if j==m or (i<n and a[i]<b[j]):
            c.append(a[i])
            i+=1
        else:
            c.append(b[j])
            j+=1
    return c
def sort(a):
    n = len(a)
    if n <= 1:
        return a
    al = a[:n//2]
    ar = a[n//2:]
    al = sort(al)
    ar = sort(ar)
    return merge(al, ar)
n = int(input())
a = list((map(int, input().split())))
print(*sort(a))
