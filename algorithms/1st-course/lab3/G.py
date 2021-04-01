s = str(input())
n = len(s)
MMM = 9999999999999999999999
 
dp = [[0]*(n) for _ in range(n)]
pp = [[0]*(n) for _ in range(n)]
 
for j in range(n):
    dp[j][j] = 1
    for i in range(j - 1, -1, -1):
        if (s[i] == '(' and s[j] == ')') or (s[i] == '[' and s[j] == ']') or (s[i] == '{' and s[j] == '}'):
            minimum = dp[i + 1][j - 1]
        else:
            minimum = MMM
        MIN = -1
        for k in range(i, j):
            if dp[k + 1][j] + dp[i][k] < minimum:
                minimum = dp[k + 1][j] + dp[i][k]
                MIN = k
        pp[i][j] = MIN
        dp[i][j] = minimum
answer = ""
def find(left, right):
    if left >= right:
        return ""
    if dp[left][right] == 0:
        return s[left:right + 1]
    if dp[left][right] - 1 != i - 1:
        if pp[left][right] == -1:
            return s[left] + find(left + 1, right - 1) + s[right]
        else:
            return find(left, pp[left][right]) + find(pp[left][right] + 1, right)
 
print(find(0, n - 1))
