n, x, y = map(int, input().split())
L, R = 0, (n - 1) * max(x, y)
while R > L + 1:
    M = (R + L) // 2
    if (M // x + M // y) < n - 1:
        L = M
    else:
        R = M
print(R + min(x, y))
