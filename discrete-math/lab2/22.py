mas = [[2]]
def dp(n, m):
    global mas
    if m == n or n == 0: return 1
    if m > n: return 0
    if m == 0: return dp(n, 1)
    if n - 2 > len(mas):
        dp(n - 1, n - 2)
    if n - 2 == len(mas):
        pp = [1]*(len(mas) + 1)
        for j in range(len(mas) - 1, -1, -1):
            pp[j] = pp[j + 1] + dp(n - j - 1, j + 1)
        mas.append(pp)
    return mas[n - 2][m - 1]

def partToNum(part):
    num = 0
    last = 1
    N = sum(part)
    s = 0
    for i in range(len(part)):
        print(i)
        for j in range(last, part[i]):
            print(N - s - j, j)
            num += dp(N - s - j, j)
        s += part[i]
        last = part[i]
    return num

#f = open("testin.txt", "r")
#a = list(map(int, f.readline().split('+')))
#f.close()
#f = open("testout.txt", "w")
#f.write(str(partToNum(a)))
#f.close()
