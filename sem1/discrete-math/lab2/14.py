from math import factorial
def permToNum(n, a):
    mas = [False] * (n + 1)
    answer = 0
    for i in range(n):
        count = 0
        for j in range(1, a[i]):
            if mas[j] == False:
                count += 1
        answer += count * factorial(n - i - 1)
        #print(a[i], count, answer,)
        mas[a[i]] = True
    return answer

f = open("perm2num.in", "r")
n = int(f.readline())
a = list(map(int, f.readline().split()))
f.close()
f = open("perm2num.out", "w")
f.write(str(permToNum(n, a)))
f.close()                
                
            
            
