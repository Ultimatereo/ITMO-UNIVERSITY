import random

while True:
    try:
        n = random.randint(1, 6)
        m = random.randint(4, 6)

        orders = []
        for i in range(n):
            cost = random.randint(1, 10)
            items = []
            while len(items) == 0:
                items = []
                for j in range(1, m + 1):
                    if random.randint(1, 2) == 1:
                        items.append(j)
            mas = [cost, len(items)]
            for item in items:
                mas.append(item)
            orders.append(mas)

        costs = [random.randint(1, 10) for _ in range(m)]

        sales = []
        pair = {}
        taken = [False for _ in range(m)]
        for i in range(1, m // 2 + 1):
            rr = random.randint(m // 2 + 1, m)
            if not taken[rr - 1]:
                pair[i] = rr
                pair[rr] = i
                taken[rr - 1] = True
                taken[i - 1] = True
                sales.append([
                    i, rr,
                    random.randint(max(costs[i - 1], costs[rr - 1]) + 1, costs[i - 1] + costs[rr - 1] - 1)
                ])
        p = len(sales)

        f = open("test.txt", "w")
        f.write(" ".join([str(n), str(m), str(p)]) + "\n")
        for order in orders:
            f.write(" ".join(map(str, order)) + "\n")
        for cost in costs:
            f.write(str(cost) + "\n")
        for sale in sales:
            f.write(" ".join(map(str, sale)) + "\n")
        f.close()
        break
    except:
        continue


