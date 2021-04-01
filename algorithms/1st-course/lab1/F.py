n, m = map(int, input().split())
a = list(map(int, input().split()))
b = list(map(int, input().split()))
for x in b:
    left = 0
    right = n - 1
    while left < right:
        middle = (left + right) // 2
        if a[middle] < x:
            left = middle + 1
        else:
            right = middle
 
    if left > 0 and a[left] != x and abs(a[left - 1] - x) <= abs(a[left] - x):
        print(a[left - 1])
    else:
        print(a[left])
