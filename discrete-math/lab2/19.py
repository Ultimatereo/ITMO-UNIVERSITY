def numTo2Brackets(n, k):
    answer = ""
    l = 2*n
    dp = [[0 for j in range(n + 1)] for i in range(l + 1)]
    dp[0][0] = 1
    for i in range(1, l + 1):
        for j in range(n + 1):
            if i == 0:
                dp[i][j] = 0
                continue
            if j > 0:
                dp[i][j] += dp[i-1][j-1]
            if j + 1 <= n:
                dp[i][j] += dp[i-1][j+1]
    pp = 0
    newl = 0
    openB = [" "]*l

    for i in range(l - 1, -1, -1):
        cur = 0
        dif = 0
        if pp < n:
            if pp < -1:
                dif = 0
            elif i - pp - 1 >= 0:
                print(i - pp - 1)
                dif = dp[i][pp + 1] << ((i - pp - 1)//2)
            cur = dif
        else:
            cur = 0
        if cur >= k:
            answer = answer + "("
            openB[newl] = "("
            newl += 1
            pp += 1
            continue
        
        k -= cur
        if newl > 0:
            if openB[newl - 1] == "(":
                if pp > 0:
                    dif = dp[i][pp - 1] << ((i - pp + 1)//2)
                    cur = dif
                else:
                    cur = 0
            else:
                cur = 0
        else:
            cur = 0
        if cur >= k:
            answer = answer + ")"
            newl -= 1
            pp -= 1
            continue
        
        k -= cur
        if pp < n:
            if pp < -1:
                dif = 0
            elif i - pp - 1 >= 0:
                dif = dp[i][pp + 1] << ((i - pp - 1)//2)
            cur = dif
        else:
            cur = 0
        if cur >= k:
            answer = answer + "["
            openB[newl] = "["
            newl += 1
            pp += 1
            continue

        k -= cur
        answer = answer + "]"
        newl -= 1
        pp -= 1
    return answer
    
f = open("num2brackets2.in", "r")
n, k = map(int, f.readline().split())
f.close()
f = open("num2brackets2.out", "w")
f.write(numTo2Brackets(n, k + 1))
f.close()
