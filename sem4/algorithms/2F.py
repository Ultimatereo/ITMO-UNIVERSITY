BIG = 1_000_000_000_000


# def shortest_path(node1, node2):
#     path = [[node1]]
#     path_index = 0
#     previous_nodes = {node1}
#
#     if node1 == node2:
#         return path[0]
#
#     while path_index < len(path):
#         current_path = path[path_index]
#         last_node = current_path[-1]
#         for next_node in range(number_of_vertices):
#             if c[last_node][next_node] - f[last_node][next_node] > 0:
#                 if next_node == node2:
#                     current_path.append(node2)
#                     return current_path
#                 if next_node not in previous_nodes:
#                     new_path = current_path[:]
#                     new_path.append(next_node)
#                     path.append(new_path)
#                     previous_nodes.add(next_node)
#         path_index += 1
#     return []
#
#
# def get_max_flow() -> int:
#     answer = 0
#     while True:
#         path = shortest_path(start, end)
#         if not path:
#             return answer
#         delta = BIG
#         for i in range(len(path) - 1):
#             delta = min(delta, c[path[i]][path[i + 1]] - f[path[i]][path[i + 1]])
#         for i in range(len(path) - 1):
#             f[path[i]][path[i + 1]] += delta
#             f[path[i + 1]][path[i]] -= delta
#         answer += delta


n, low, high, m = map(int, input().split())
h = list(map(int, input().split()))
f = [[0 for _ in range(m + 2)] for _ in range(m + 2)]
c = [[0 for _ in range(m + 2)] for _ in range(m + 2)]
number_of_vertices = m + 2
start = 0
end = m + 1
level_to_ind = {}
for i in range(1, m):
    c[i][i + 1] = 1
    c[i + 1][i] = 1
for i in range(m):
    if h[i] not in level_to_ind:
        level_to_ind[h[i]] = [i]
    else:
        level_to_ind[h[i]].append(i)
    if h[i] < low:
        c[0][i + 1] = BIG
    elif h[i] > high:
        c[i + 1][m + 1] = BIG

for key in level_to_ind:
    level = level_to_ind[key]
    for i in range(len(level) - 1):
        c[level[i] + 1][level[i + 1] + 1] = BIG
        c[level[i + 1] + 1][level[i] + 1] = BIG


def dfs(v, dmin) -> int:
    if v == end or mark[v]:
        return dmin
    mark[v] = True
    for u in range(number_of_vertices):
        if not mark[u] and c[v][u] - f[v][u] > 0:
            d = dfs(u, min(dmin, c[v][u] - f[v][u]))
            if d > 0:
                f[v][u] += d
                f[u][v] -= d
                return d
    return 0


answer = 0
while True:
    mark = [False for _ in range(number_of_vertices)]
    delta = dfs(start, BIG)
    if delta == 0:
        break
    answer += delta
print(answer)