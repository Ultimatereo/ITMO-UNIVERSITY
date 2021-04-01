n, m = map(int, input().split())
n, m = max(n, m), min(n, m)
 
dp = [None] * n
for i in range(n):
    dp[i] = [None] * (m + 1)
    for j in range(m + 1):
        dp[i][j] = [0] * (2 ** (m + 1))
for i in range(2 ** (m + 1)):
    dp[0][0][i] = 1
 
for i in range(n):
    for j in range(m):
        for k in range(2 ** (m + 1)):
            for t in range(2):
                if j == 0 or (k >> (j - 1)) & 7 != t * 7:
                    q = (k & ~(1 << j)) | (t << j)
                    dp[i][j + 1][q] += dp[i][j][k]
    if i < n - 1:
        for k in range(2 ** (m + 1)):
            q = (k & ~(1 << m)) << 1
            dp[i + 1][0][q] += dp[i][m][k]
 
print(max(dp[n - 1][m]))
