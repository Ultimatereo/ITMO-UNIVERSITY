def convert_base(num, to_base=10, from_base=10):
    if isinstance(num, str):
        n = int(num, from_base)
    else:
        n = int(num)
    alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    if n < to_base:
        return alphabet[n]
    else:
        return convert_base(n // to_base, to_base) + alphabet[n % to_base]

def digitCircleShift(v):
    answer = ""
    for i in range(len(v)):
        value = (int(v[i]) + 1)%3
        answer+= str(value)
    return answer   
def anti_gray(n):
    answer = []
    for i in range(3**(n - 1)):
        t = convert_base(i, to_base = 3)
        t = (n - len(t))*"0" + t
        answer.append(t)
        t = digitCircleShift(t)
        answer.append(t)
        t = digitCircleShift(t)
        answer.append(t)
    return answer
f = open("antigray.in", "r")
n = int(f.readline())
f.close()
f = open("antigray.out", "w")
a = anti_gray(n)
for i in range(3**n):
    f.write(a[i] + '\n')
f.close()
