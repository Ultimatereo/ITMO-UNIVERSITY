def nextPart(b):
    b[len(b) - 1] -= 1
    b[len(b) - 2] += 1
    if b[len(b)-2] > b[len(b) - 1]:
        b[len(b) - 2] += b[len(b) - 1]
        b.pop()
    else:
        while b[len(b) - 2] * 2 <= b[len(b) - 1]:
            b.append(b[len(b) - 1] - b[len(b) - 2])
            b[len(b) - 2] = b[len(b) - 3]
    return b
    
f = open("testin.txt", "r")
t = str(f.readline().strip())
sum = t[:t.index("=")]
a = list(map(int, t[t.index("=") + 1:].split('+')))
f.close()
f = open("testout.txt", "w")
if len(a) == 1:
    f.write("No solution")
else:
    f.write(sum + "=")
    f.write('+'.join(str(v) for v in nextPart(a)))
f.close()
