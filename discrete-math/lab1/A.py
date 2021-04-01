n = int(input())
a = [0]*n
b = [0]*n
a_facts = [1]*5
b_facts = [1]*5
c = []
for i in range(n):
    c.append([0]*n)
for i in range(n):
    a[i] = list(map(int, input().split()))

i = 0
while i < n:
    if a[i][i] == 0:
        a_facts[0] = 0
        break
    i += 1


i = 0
while i < n:
    if a[i][i] == 1:
        a_facts[1] = 0
        break
    i += 1

for i in range(n - 1):
    if a_facts[2] == 0:
        break
    for j in range(i + 1, n):
        if a[i][j] != a[j][i]:
            a_facts[2] = 0
            break

for i in range(n - 1):
    if a_facts[3] == 0:
        break
    for j in range(i + 1, n):
        if a[i][j] == 1 and a[j][i] == 1:
            a_facts[3] = 0
            break

for i in range(n):
    if a_facts[4] == 0:
        break
    for j in range(n):
        if a_facts[4] == 0:
            break
        for k in range(n):
            if (a[i][j] == 1) and (a[j][k] == 1) and (a[i][k] == 0):
                a_facts[4] = 0
                break

        
for i in range(n): #b elements
    b[i] = list(map(int, input().split()))
i = 0
while i < n:
    if b[i][i] == 0:
        b_facts[0] = 0
        break
    i += 1


i = 0
while i < n:
    if b[i][i] == 1:
        b_facts[1] = 0
        break
    i += 1

for i in range(n - 1):
    if b_facts[2] == 0:
        break
    for j in range(i + 1, n):
        if b[i][j] != b[j][i]:
            b_facts[2] = 0
            break

for i in range(n - 1):
    if b_facts[3] == 0:
        break
    for j in range(i + 1, n):
        if b[i][j] == 1 and b[j][i] == 1:
            b_facts[3] = 0
            break

for i in range(n):
    if b_facts[4] == 0:
        break
    for j in range(n):
        if b_facts[4] == 0:
            break
        for k in range(n):
            if (b[i][j] == 1) and (b[j][k] == 1) and (b[i][k] == 0):
                b_facts[4] = 0
                break
        
for i in range(n):
    for j in range(n):
        for k in range(n):
            if (a[i][j] == 1) and (b[j][k] == 1):
                c[i][k] = 1

                
print(*a_facts)
print(*b_facts)
for i in range(n):
    print(*c[i])
