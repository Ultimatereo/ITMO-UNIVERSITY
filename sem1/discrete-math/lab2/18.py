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

def seqToNum(s):
    n = len(s)//2
    num = 0
    depth = 0
    for i in range(2*n):
        if s[i] == '(':
            depth += 1
        else:
            num += dp(2*n - i - 1, depth + 1)
            depth -= 1
    return num

f = open("brackets2num.in", "r")
s = str(f.readline().strip())
f.close()
f = open("brackets2num.out", "w")
f.write(str(seqToNum(s)))
f.close()
