def sort(a):
    c = [0]*101
    n = len(a)
    for i in range(n):
        c[a[i]] += 1
    pos = 0
    for number in range(101):
        for i in range(c[number]):
            a[pos] = number
            pos += 1
    return a
n = int(input())
a = list(map(int, input().split()))
print(*sort(a))
