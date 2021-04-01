n, m = map(int, input().split())
table = [None] * n
dp = [None] * n
block = [0] * (n + 1)
for i in range(n):
    table[i] = input()
    dp[i] = [None] * (m + 1)
    for j in range(m):
        dp[i][j] = [0] * (1 << m)
        if table[i][j] == 'X':
            block[i] += 1 << j
    dp[i][m] = [0] * (1 << m)
 
dp[0][0][block[0]] = 1
 
for i in range(n):
    for j in range(m):
        for p in range(1 << m):
            if p & (1 << j) != 0:
                dp[i][j + 1][p - (1 << j)] += dp[i][j][p]
            else:
                dp[i][j + 1][p + (1 << j)] += dp[i][j][p]
                if j < m - 1 and p & (1 << (j + 1)) == 0:
                    dp[i][j + 1][p + (1 << (j + 1))] += dp[i][j][p]
    if i < n - 1:
        for p in range(1 << m):
            if p & block[i + 1] == 0:
                dp[i + 1][0][p | block[i + 1]] = dp[i][m][p]
 
 
print(dp[n - 1][m][0])
