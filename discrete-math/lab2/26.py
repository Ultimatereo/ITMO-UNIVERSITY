def nextSetPart(a):
    used = []
    fl = False
    max = -1
    for i in range(len(a) - 1, -1, -1):
        #print("i = ", i)
        #print(a, used)
        if (len(used) != 0) and (used[max] > a[i][-1]):
            min = max
            for k in range(len(used)):
                if used[k] < used[min] and used[k] > a[i][-1]:
                    min = k
            a[i].append(used[min])
            used = used[:min + 1]
            break

        for j in range(len(a[i]) - 1, -1, -1):
            #print("j = ", j)
            #print(a, used)
            if (len(used) != 0) and (j != 0) and (used[max] > a[i][j]):
                min = max
                for k in range(len(used)):
                    if used[k] < used[min] and used[k] > a[i][j]:
                        min = k
                used[min], a[i][j] = a[i][j], used[min]
                fl = True
                break
            
            used.append(a[i][j])
            a[i].pop(j)
            if len(a[i]) == 0:
                a.pop()
        if fl:
            break
          
    used = sorted(used)
    #print(a, used)
    for i in range(len(used)):
        a.append([used[i]])
    return a
 
  
f = open("nextsetpartition.in", "r")
t = open("nextsetpartition.out", "w")
k = list(map(int, f.readline().split()))
while k != [0,0]:
    a = [list(map(int, f.readline().split()))]
    for i in range(k[1] - 1):
        a.append(list(map(int, f.readline().split())))
    answer = nextSetPart(a)
    t.write(str(k[0]) + " " + str(len(answer)))
    t.write('\n')
    for i in range(len(answer)):
        for j in range(len(answer[i])):
            t.write(str(answer[i][j]) + " ")
        t.write('\n')
    t.write('\n')
    f.readline()
    k = list(map(int, f.readline().split()))
f.close()
t.close()
        
