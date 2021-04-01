def dp(n):
    mas = [0,1,1,1,1,1,1,1,0,1]
    for i in range(1, n):
        pp = [0]*10
        pp[0] = (mas[4] + mas[6])
        pp[1] = (mas[8] + mas[6]) 
        pp[2] = (mas[7] + mas[9]) 
        pp[3] = (mas[4] + mas[8])
        pp[4] = (mas[0] + mas[9] + mas[3])
        pp[5] = 0
        pp[6] = (mas[0] + mas[1] + mas[7])
        pp[7] = (mas[2] + mas[6]) 
        pp[8] = (mas[1] + mas[3]) 
        pp[9] = (mas[2] + mas[4]) 
        mas = pp
    return(sum(mas) % 1000000000)
 
n = int(input())
print(dp(n))
