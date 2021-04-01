n = int(input())
facts = [1]*5
for i in range(n):
    count, string = map(str, input().split())
    count = int(count)
    string = list(string)
    if facts[0] == 1: 
        if string[0] == "1": #Сохраняет ноль
            facts[0] = 0
    
    if facts[1] == 1:
        if string[-1] == "0": #Сохраняет единицу
            facts[1] = 0
            
    if facts[2] == 1: #Самодвойственность
        for i in range(2**(count-1)):
            if string[i] == string[-i-1]:
                facts[2] = 0
                break
            
    if facts[3] == 1: #Monotone
        for i in range(2**count):
            if facts[3] == 0:
                break
            for j in range(i + 1, 2**count):
                if (i & j) == i and string[i] == "1" and string[j] == "0":
                    facts[3] = 0
                    break
        
    if facts[4] == 1: #Линейная через треугольник
        lin = [0]*(2**count)
        lin[0] = string[0]
        k = 2**count
        for t in range(1, 2**count):
            for i in range(k - 1):
                string[i] = str((int(string[i]) + int(string[i + 1]))%2)
            lin[t] = string[0]
            k -= 1
        for i in range(1, 2**count):
            if (i&(i-1) != 0) and lin[i] == "1":
                facts[4] = 0
                break
if 1 in facts:
    print("NO")
else:
    print("YES")
