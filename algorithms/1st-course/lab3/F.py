N = 100
c = [0]*(200)
dp = [[0]*(200) for _ in range(200)]
n = int(input())
for i in range(1, n + 1):
    c[i] = int(input())
for i in range(n + 1):
    for j in range(i + 1, n + 1):
        dp[i][j] = 9999999999999999999999999999999
        
dp[1][0] = c[1]
if c[1] > N:
    dp[1][1] = c[1]
else:
    dp[1][1] = 9999999999999999999999999999999
for i in range(2, n + 1):
    dp[i][0] = 9999999999999999999999999999999
for i in range(2, n + 1):
    for j in range(n + 1):
        if c[i] <= N:
            dp[i][j] = min(dp[i - 1][j] + c[i], dp[i - 1][j + 1])
        elif j > 0:
            dp[i][j] = min(dp[i - 1][j - 1] + c[i], dp[i - 1][j + 1])
        else:
            dp[i][j] = dp[i - 1][j + 1]

col = 0
while (col < n) and (dp[n][col] >= dp[n][col + 1]) and (dp[n][col + 1] > 0):
    col += 1
print(dp[n][col])
print(col, end = " ")

line = n
days = []
while line > 0:
    if dp[line][col] == dp[line - 1][col + 1]:
        days = [line] + days
        line -= 1
        col += 1
        continue
    line -= 1
    if c[line + 1] > N:
        col -= 1
print(len(days))
for i in range(len(days)):
    print(days[i])
        


