def merge(a,b):
    count = 0
    n = len(a)
    m = len(b)
    i = 0
    j = 0
    c = []
    while i<n or j<m:
        if j==m or (i<n and a[i]<b[j]):
            c.append(a[i])
            count += j
            i+=1
        else:
            c.append(b[j])
            j+=1
    return count, c
count = 0
k = int(input())
a = list(map(int, input().split()))
 
def sort(a):
    global count
    global k
    n = len(a)
    if n <= 1:
        return 0, a
    al = a[:n//2]
    ar = a[n//2:]
    al = sort(al)[1]
    ar = sort(ar)[1]
    count += merge(al,ar)[0]
    if (len(merge(al, ar)[1]) == k):
        return count
    else:
        return merge(al, ar)
 
print(sort(a))
