from itertools import combinations


def check(combination) -> bool:
    for i in range(len(combination)):
        for j in range(i + 1, len(combination)):
            first = combination[i]
            second = combination[j]
            if first > n or second <= n:
                continue
            second -= n
            if (first, second) not in edges:
                return False
    return True


def print_correct(combination, ff):
    men = []
    women = []
    for i in range(len(combination)):
        if combination[i] <= n:
            men.append(str(combination[i]))
        else:
            women.append(str(combination[i] - n))
    ff.write(str(len(men)) + " " + str(len(women)))
    ff.write("\n")
    ff.write(" ".join(men))
    ff.write("\n")
    ff.write(" ".join(women))
    ff.write("\n")
    ff.write("\n")


f = open("test.txt", "r")
ff = open("dumb.txt", "w")
# k = int(input())
k = int(f.readline())
for _ in range(k):
    # n, m = map(int, input().split())
    n, m = map(int, f.readline().split())
    edges = set()
    for i in range(n):
        # line = list(map(int, input().split()))
        line = list(map(int, f.readline().split()))
        for j in range(len(line) - 1):
            edges.add((i + 1, line[j]))
    flag = False
    for answer in range(n + m, -1, -1):
        for combination in combinations(range(1, n + m + 1), answer):
            if check(combination):
                ff.write(str(answer) + "\n")
                print_correct(combination, ff)
                flag = True
                break
        if flag:
            break
f.close()