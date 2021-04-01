def getNum(s):
    length = len(s)
    n = length//2
    num = 0
    dp = [[0 for j in range(n + 1)] for i in range(length + 1)]
    dp[0][0] = 1
    for i in range(1, length + 1):
        for j in range(n + 1):
            if i == 0:
                dp[i][j] = 0
                continue
            if j > 0:
                dp[i][j] += dp[i-1][j-1]
            if j + 1 <= n:
                dp[i][j] += dp[i-1][j+1]
    pp = 0
    mas = []
    for i in range(length):
        cur = s[i]
        if cur == "(":
            pp += 1
            mas.append("(")
        else:
            if pp < n and (((length - i - 1 - (pp + 1)) // 2)) >= 0:
                num += dp[length - i - 1][pp + 1] << ((length - i - 1 - (pp + 1)) // 2)
            if cur == ")":
                mas.pop()
                pp -= 1
            else:
                if (len(mas) != 0 and mas[-1] == "("):
                    num += dp[length - i - 1][pp - 1] << ((length - i - 1 - (pp - 1)) // 2)
                if cur == "[":
                    pp += 1
                    mas.append("[")
                else:
                    if pp < n and (((length - i - 1 - (pp + 1)) // 2)) >= 0:
                        num += dp[length - i - 1][pp + 1] << ((length - i - 1 - (pp + 1)) // 2)
                    mas.pop()
                    pp -= 1
    return num           

f = open("brackets2num2.in", "r")
s = str(f.readline().strip())
f.close()
f = open("brackets2num2.out", "w")
f.write(str(getNum(s)))
f.close()
