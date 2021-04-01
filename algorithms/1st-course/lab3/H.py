powers = [1]
for i in range(13):
    powers.append(powers[-1] * 2)
 
 
def isBitOne(i, n):
    return n & powers[i] != 0
 
 
def bitCount(n):
    count = 0
    while n > 0:
        count += n % 2
        n //= 2
    return count
 
 
n = int(input())
MMM = 99999999999999999
inp = [None] * n
for i in range(n):
    inp[i] = [int(x) for x in input().split()]
 
dp = [None] * powers[n]
for i in range(powers[n]):
    dp[i] = [MMM] * n
 
for i in range(n):
    dp[powers[i]][i] = 0
 
for mask in range(powers[n]):
    for i in range(n):
        if isBitOne(i, mask):
            if powers[i] != mask:
                minn = MMM
                for j in range(n):
                    if isBitOne(j, mask):
                        minn = min(minn, dp[mask ^ powers[i]][j] + inp[j][i])
                dp[mask][i] = minn
 
minn = MMM
prev = powers[n] - 1
for i in range(n):
    if dp[prev][i] < minn:
        minn = dp[prev][i]
        k = i
print(minn)
ans = []
 
 
def find(prevv, kk):
    ans.append(kk + 1)
    next = prevv ^ powers[kk]
    if next != 0:
        for i in range(n):
            if i == kk:
                continue
            if dp[prevv][kk] == dp[next][i] + inp[kk][i]:
                find(next, i)
                return
 
 
find(prev, k)
print(*ans[::-1], sep=' ')
