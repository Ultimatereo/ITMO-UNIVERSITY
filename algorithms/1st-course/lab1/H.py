def good(x):
    return (x//h)*(x//w) >= n
w, h, n = map(int, input().split())
r = 1
while (r//h)*(r//w) < n:
    r *= 2
l = r//2
while r - l > 1:
    m = (r+l)//2
    if (m//h)*(m//w) >= n:
        r = m
    else:
        l = m
print(r)
