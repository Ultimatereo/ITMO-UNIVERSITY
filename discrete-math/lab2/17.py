def dp(n, m):
    d = [[0 for i in range(m + 1)] for j in range(n + 1)]
    d[0][0] = 1
    for i in range(1, n + 1):
        for j in range(1, m + 1):
            d[i][j] = d[i-1][j-1] + d[i-1][j+1]
    return d[n][m]

    
def catalan(n): 
    if n <= 1: 
        return 1
    res = 0
    
    for i in range(n): 
        res += catalan(i) * catalan(n-i-1)
        
    return res 

def numTobrackets(n, k):
    depth = 0
    s = ""
    for i in range(2*n):
        if d[2*n - (i + 1)][depth + 1] >= k:
            s += '('
            depth += 1
        else:
            k -= d[2*n - (i + 1)][depth + 1]
            s + ')'
            depth -= 1
    return s
