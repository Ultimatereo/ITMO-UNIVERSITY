vp, vf = map(int, input().split())
a = float(input())
def t(x):
    global vf, vp, a
    return ((x*x + (1-a)*(1-a))**0.5)/vp + ((a*a + (1-x)*(1-x))**0.5)/vf
l = 0
r = 1
f = 0
g = 0
for i in range(200):
    f = l + (r - l) / 3
    g = r - (r - l) / 3
    if (t(f) < t(g)):
        r = g
    else:
        l = f
print(l)
