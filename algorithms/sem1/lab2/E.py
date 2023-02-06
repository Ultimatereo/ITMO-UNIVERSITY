def f(s):
    stack = []
    for i in s.split():
        try:
            i = int(i)
            stack.append(i)
        except ValueError:
            a, b = stack.pop(), stack.pop()
            if i == '+':
                r = a + b
            elif i == '*':
                r = a * b
            elif i == '-':
                r = b - a
            else:
                raise NotImplemented
            stack.append(r)
    return stack[0]
print(f(input()))
