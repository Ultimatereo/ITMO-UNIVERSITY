def chain_code(n):
    current = '0' * n
    result = [current]
    while True:
        prefix = current[1:]
        if prefix + '1' not in result:
            current = prefix + '1'
        elif prefix + '0' not in result:
            current = prefix + '0'
        else:
            break
        result.append(current)
    return result
f = open("chaincode.in", "r")
n = int(f.readline())
f.close()
f = open("chaincode.out", "w")
a = chain_code(n)
for i in range(len(a)):
    f.write(a[i] + '\n')
f.close()
