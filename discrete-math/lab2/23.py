def toBin(a, pp):
    answer = ""
    for i in range(len(a), pp):
        answer += "0"
    answer += str(a)
    return answer

f = open("nextvector.in", "r")
t = str(f.readline().strip())
pp = len(t)
a = int(t, 2)
f.close()


f = open("nextvector.out", "w")
if a == 0:
    f.write("-")
else:
    f.write(toBin(str(bin(a - 1))[2:], pp))
    
f.write('\n')
 
if len(str(bin(a + 1))) - 2 > pp:
    f.write("-")
else:
    f.write(toBin(str(bin(a + 1))[2:], pp))
f.close()
