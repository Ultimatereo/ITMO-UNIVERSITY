mas = [[2]]
def dp(n, m):
    global mas
    if n == 0: return 1
    if m == n: return 1
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

def numToPart(N, num):
    prefix = []
    while True:
        #print("----------")
        if prefix == []:
            last = 1
        else:
            last = prefix[-1]
        #print(prefix, last)
        for i in range(last, N - sum(prefix) + 1):
            #print("i = " + str(i) + " " + str(num))
            value = dp(N - sum(prefix) - i, i)
            #print(value)
            if num - value <= 0:
                prefix.append(i)
                #print(prefix, num)
                break
            else:
                num -= value
                
        if sum(prefix) == N:
            break
    return prefix

f = open("num2part.in", "r")
N, num = map(int, f.readline().split())
f.close()
f = open("num2part.out", "w")
f.write('+'.join(str(v) for v in numToPart(N, num + 1)))
f.close()
