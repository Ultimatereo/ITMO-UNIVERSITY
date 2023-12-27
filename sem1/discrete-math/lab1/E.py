n = int(input())
b2 = [0]*(2**n)
string = [0]*(2**n)
for i in range(2**n):
    b2[i], string[i] = map(str, input().split())
    string[i] = int(string[i])
     
lin = [0]*(2**n)
lin[0] = string[0]
k = 2**n
 
for t in range(1, 2**n):
    for i in range(k - 1):
        string[i] = (string[i] + string[i + 1])%2
    k -= 1
    lin[t] = string[0]
     
for i in range(2**n):
    print(b2[i], lin[i])
