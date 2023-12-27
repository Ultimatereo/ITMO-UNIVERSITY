def gray_code_recurse (g,n):
    k=len(g)
    if n<=0:
        return
 
    else:
        for i in range (k-1,-1,-1):
            char='1'+g[i]
            g.append(char)
        for i in range (k-1,-1,-1):
            g[i]='0'+g[i]
        gray_code_recurse (g,n-1)
         
def gray_code(n):
    g=['0','1']
    gray_code_recurse(g,n-1)
    return g
 
f = open("testin.txt", "r")
n = int(f.readline())
f.close
a = gray_code(n)
f = open("testout.txt", "w")
for i in range(len(a)):
    f.write(a[i] + '\n')
f.close()
