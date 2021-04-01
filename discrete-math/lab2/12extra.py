def gen(n, k):
    #print(n, k)
    if k == 1:
        a = [0]*n
        for i in range(n):
            a[i] = i + 1
        return [[a]]
    elif n == k:
        answer = []
        for i in range(1, n + 1):
            b = [i]
            answer.append(b)
        return [answer]
        
    else:
        answer = gen(n - 1, k - 1)
        for i in range(len(answer)):
            answer[i] = [[n]] + answer[i]
        bb = gen(n - 1, k)
        #print(bb)
        for part in bb:
            for i in range(len(part)):
                pp = part[i] + [n]
                #print("pp ", pp)
                answer.append(part[:i] + [pp] + part[i + 1:])
        return answer

f = open("testin.txt", "r")
n, k = map(int, f.readline().split())
f.close()
f = open("testout.txt", "w")
a = gen(n, k)
for mas in a:
    for part in mas:
        f.write(str(part) + " ")
    f.write('\n')
f.close()
        
