answer = []
def partition(a, n, p):
    global answer
    if n:
        for i in range(n+1, 0, -1):
            if i <= p:
                partition(a+[i], n-i, i)
    else:
        answer.append(a[::-1])
    return
 


f = open("testin.txt", "r")
n = int(f.readline())
f.close()
partition([], n, n)
f = open("testout.txt", "w")
for i in range(len(answer) - 1, -1, -1):
    f.write(str(answer[i][0]))
    for j in range(1, len(answer[i])):
        f.write('+' + str(answer[i][j]))
    f.write('\n')
f.close()
