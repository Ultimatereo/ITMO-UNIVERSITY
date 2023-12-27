import itertools

f = open("test.txt", "r")
n, m, p = map(int, f.readline().split())
orders = [list(map(int, f.readline().split())) for _ in range(n)]
costs = [int(f.readline()) for _ in range(m)]
sales = [list(map(int, f.readline().split())) for _ in range(p)]
f.close()

pair = {}
sale_to_num = {}
for i in range(p):
    pair[sales[i][0]] = sales[i][1]
    pair[sales[i][1]] = sales[i][0]
    sale_to_num[sales[i][0]] = i
    sale_to_num[sales[i][1]] = i

best_income = 0
indices = [i for i in range(1, m + 1)]

for num in range(1, m + 1):
    for comb in itertools.combinations(indices, num):
        got = 0
        for order in orders:
            items = order[2:]
            flag = True
            for item in items:
                if item not in comb:
                    flag = False
                    break
            if flag:
                got += order[0]
        lost = 0
        taken = [False for _ in range(m + 1)]
        for item in comb:
            if taken[item]:
                continue
            if item in pair and pair[item] in comb:
                if not taken[item]:
                    lost += sales[sale_to_num[item]][2]
                    taken[item] = True
                    taken[pair[item]] = True
            else:
                lost += costs[item - 1]
        best_income = max(best_income, got - lost)

f = open("dumb.txt", "w")
f.write(str(best_income))
f.close()
