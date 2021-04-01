c = float(input())
l = 0
r = c
m = (r+l)/2
for i in range(200):
    if m*m + m**0.5 >= c:
        r = m
    else:
        l = m
    m = (l + r) / 2
print(r)
