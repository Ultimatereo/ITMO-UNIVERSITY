def next(s):
    counterClose = 0
    counterOpen = 0
    for i in range(len(s) - 1, -1, -1):
        if s[i] == '(':
            counterOpen += 1
            if counterClose > counterOpen:
                break
        else:
            counterClose += 1
    s = s[:len(s) - counterOpen - counterClose]
    if s == "":
        return ""
    else:
        s += ')'
        for i in range(counterOpen):
            s += '('
        for i in range(counterClose - 1):
            s += ')'
        return s

def order(f, n):
    s = ""
    for i in range(n):
        s = s + '('
    for i in range(n):
        s = s + ')'
    f.write(s)
    f.write('\n')
    while s != "":
        s = next(s)
        f.write(s)
        f.write('\n')
    return

f = open("testin.txt", "r")
n = int(f.readline())
f.close()
f = open("testout.txt", "w")
order(f, n)
f.close()
