def Lev(s1, s2):
    m = len(s1)
    n = len(s2)
    d = [[0 for j in range(n + 1)] for i in range(m + 1)]
    for j in range(1, n + 1):
        d[0][j] = d[0][j - 1] + 1
    for i in range(1, m + 1):
        d[i][0] = d[i - 1][0] + 1
        for j in range(1, n + 1):
            if s1[i - 1] != s2[j - 1]:
                d[i][j] = min(d[i - 1][j], d[i][j - 1], d[i - 1][j - 1]) + 1
            else:
                d[i][j] = d[i - 1][j - 1]
    return d[m][n]
 
s1 = str(input())
s2 = str(input())
print(Lev(s1, s2))
