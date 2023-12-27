def find(a):
    n = len(a)
    prev = [0]*n
    d = [0]*n
 
    for i in range(n):
        d[i] = 1
        prev[i] = -1
        for j in range(i):
            if a[j] < a[i] and d[j] + 1 > d[i]:
                d[i] = d[j] + 1
                prev[i] = j
    pos = 0
    length = d[0]
    for i in range(n):
        if d[i] > length:
            pos = i
            length = d[i]
            
    answer = []
    while pos != -1:
        answer = [a[pos]] + answer
        pos = prev[pos]
    return answer
 
n = int(input())
a = list(map(int, input().split()))
b = find(a)
print(len(b))
print(' '.join(str(v) for v in b))
