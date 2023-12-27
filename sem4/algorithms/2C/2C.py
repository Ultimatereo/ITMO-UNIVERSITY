def get_min_cut(number_of_vertices, raw_edges, start, end):
    start -= 1
    end -= 1
    number_of_edges = len(raw_edges)
    def dfs(v, dmin) -> int:
        if v == end or mark[v]:
            return dmin
        mark[v] = True
        for i in range(len(g[v])):
            u = g[v][i][0]
            cost = g[v][i][1]
            f = g[v][i][2]
            j = g[v][i][3]
            if not mark[u] and cost - f > 0:
                d = dfs(u, min(dmin, cost - f))
                if d > 0:
                    g[v][i][2] += d
                    g[u][j][2] -= d
                    return d
        return 0
    BIG = 1_000_000_000_000
    edges = [[-1 for _ in range(2)] for _ in range(number_of_edges)]
    g = [[] for _ in range(number_of_vertices)]
    for i in range(number_of_edges):
        aa, bb, cc_front, cc_back = raw_edges[i]
        g[aa - 1].append([bb - 1, cc_front, 0, len(g[bb - 1])])
        edges[i][0] = aa - 1
        edges[i][1] = len(g[aa - 1]) - 1
        g[bb - 1].append([aa - 1, cc_back, 0, len(g[aa - 1]) - 1])

    answer = 0
    while True:
        mark = [False for _ in range(number_of_vertices)]
        delta = dfs(start, BIG)
        if delta != 1:
            break
        answer += delta
    if delta > 1:
        return [0, BIG, []]
    mark = [False for _ in range(number_of_vertices)]
    dfs(start, BIG)
    answer_list = []
    for i in range(number_of_edges):
        edge = edges[i]
        if mark[edge[0]] and not mark[g[edge[0]][edge[1]][0]]:
            answer_list.append(i + 1)
    return [len(answer_list), answer, answer_list]


# f = open("test.txt", "r")
# n, m = map(int, f.readline().split())
# mas = [list(f.readline().strip()) for _ in range(n)]
# f.close()
n, m = map(int, input().split())
mas = [list(input()) for _ in range(n)]
coord_to_number = {}
number_to_coord = []
start_coord = (0, 0)
end_coord = (n - 1, m - 1)
inf = 3_000_000
raw_edges = []
cur = 1


for i in range(n):
    for j in range(m):
        if mas[i][j] == '.':
            coord_to_number[(i, j)] = [cur, cur + 1]
            raw_edges.append([cur, cur + 1, 1, 0])
            number_to_coord.append((i, j))
            number_to_coord.append((i, j))
            if i > 0 and mas[i - 1][j] != '#':
                raw_edges.append([coord_to_number[(i - 1, j)][1], cur, inf, 0])
                raw_edges.append([cur + 1, coord_to_number[(i - 1, j)][0], inf, 0])
            if j > 0 and mas[i][j - 1] != '#':
                raw_edges.append([coord_to_number[(i, j - 1)][1], cur, inf, 0])
                raw_edges.append([cur + 1, coord_to_number[(i, j - 1)][0], inf, 0])
            cur += 2
        elif mas[i][j] == '-' or mas[i][j] == 'A' or mas[i][j] == 'B':
            coord_to_number[(i, j)] = [cur, cur + 1]
            raw_edges.append([cur, cur + 1, inf, 0])
            number_to_coord.append((i, j))
            number_to_coord.append((i, j))
            if i > 0 and mas[i - 1][j] != '#':
                raw_edges.append([coord_to_number[(i - 1, j)][1], cur, inf, 0])
                raw_edges.append([cur + 1, coord_to_number[(i - 1, j)][0], inf, 0])
            if j > 0 and mas[i][j - 1] != '#':
                raw_edges.append([coord_to_number[(i, j - 1)][1], cur, inf, 0])
                raw_edges.append([cur + 1, coord_to_number[(i, j - 1)][0], inf, 0])
            cur += 2
            if mas[i][j] == 'A':
                start_coord = (i, j)
            if mas[i][j] == 'B':
                end_coord = (i, j)
answer = -1
if len(raw_edges) != 0:
    number_of_v = cur - 1
    size, answer_min_cost, answer_list = get_min_cut(number_of_v, raw_edges, coord_to_number[start_coord][1], coord_to_number[end_coord][0])
    if answer_min_cost < inf:
        for edge in answer_list:
            coord = number_to_coord[raw_edges[edge - 1][0]]
            mas[coord[0]][coord[1]] = '+'
        answer = size

# f = open("smart.txt", "w")
# f.write(str(answer) + "\n")
# if answer != -1:
#     for line in mas:
#         f.write(''.join(line))
#         f.write("\n")
# f.close()

print(answer)
if answer != -1:
    for line in mas:
        print(''.join(line))
