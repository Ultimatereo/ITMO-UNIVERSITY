import random

n = random.randint(1, 50)
m = random.randint(1, 50)
mas = [[] for _ in range(n)]
for i in range(n):
    for j in range(m):
        mas[i].append(random.choice(['-', '#', '.']))

max_dots = n * m
count = 0
for i in range(n):
    for j in range(m):
        if mas[i][j] == '.':
            if count < max_dots:
                count += 1
            else:
                mas[i][j] = random.choice(['-', '#'])
start = (0, 0)
end = (0, 0)
while start == end:
    start = (random.randint(0, n - 1), random.randint(0, m - 1))
    end = (random.randint(0, n - 1), random.randint(0, m - 1))
mas[start[0]][start[1]] = 'A'
mas[end[0]][end[1]] = 'B'


f = open("test.txt", "w")
f.write(str(n) + " " + str(m) + "\n")
for line in mas:
    f.write(''.join(line))
    f.write("\n")
f.close()
