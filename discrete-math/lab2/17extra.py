mas = [[1]]
def dp(n, m):
    global mas
    if m < 0: return 0
    if m > n: return 0
    if n > len(mas):
        dp(n - 1, n - 1)
    if n == len(mas):
        pp = [0]*(n+1)
        for j in range(n + 1):
            pp[j] = dp(n - 1, j - 1) + dp(n - 1, j + 1)
        mas.append(pp)
    return mas[n][m]

def numToSeq(n, k):
    depth = 0
    s = ""
    for i in range(2*n):
        if dp((2*n - (i + 1)), depth + 1) >= k:
            s += '('
            depth += 1
        else:
            k -= dp((2*n - (i + 1)), depth + 1)
            s += ')'
            depth -= 1
    return s

f = open("num2brackets.in", "r")
n, k = map(int, f.readline().split())
f.close()
f = open("num2brackets.out", "w")
f.write(numToSeq(n, k + 1))
f.close()
