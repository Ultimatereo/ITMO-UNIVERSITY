def next(f, s):
    k = len(s) - 1
    for i in range(len(s)):
        if s[i] == '(' or s[i] == ')':
            k = i
        else:
            break
    s = s[:k + 1]
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
        f.write("-")
    else:
        f.write(s)
        f.write(')')
        for i in range(counterOpen):
            f.write('(')
        for i in range(counterClose - 1):
            f.write(')')

f = open("nextbrackets.in", "r")
s = f.readline()
f.close()
f = open("nextbrackets.out", "w")
next(f, s)
f.close()
