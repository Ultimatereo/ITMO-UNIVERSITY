BIG = 1_000_000_000_000

n, m, p = map(int, input().split())
number_of_vertices = n + m + p + 2
start = 1
end = n + m + p + 2
s = 0
number_of_edges = p + m + n
c = [[0 for _ in range(number_of_vertices)] for _ in range(number_of_vertices)]
f = [[0 for _ in range(number_of_vertices)] for _ in range(number_of_vertices)]
for i in range(n):
    order = list(map(int, input().split()))
    c[m + p + 1 + i][m + p + n + 1] = order[0]
    s += order[0]
    for j in range(2, len(order)):
        item = order[j]
        c[item][m + p + 1 + i] = BIG
costs = [int(input()) for _ in range(m)]
before = [1 for _ in range(m + 1)]
for i in range(p):
    sale = list(map(int, input().split()))
    c[0][m + 1 + i] = sale[2]
    before[sale[0]] = m + 2 + i
    before[sale[1]] = m + 2 + i

for i in range(1, m + 1):
    c[before[i] - 1][i] = costs[i - 1]

start -= 1
end -= 1


def shortest_path(node1, node2):
    path = [[node1]]
    path_index = 0
    previous_nodes = {node1}

    if node1 == node2:
        return path[0]

    while path_index < len(path):
        current_path = path[path_index]
        last_node = current_path[-1]
        for next_node in range(number_of_vertices):
            if c[last_node][next_node] - f[last_node][next_node] > 0:
                if next_node == node2:
                    current_path.append(node2)
                    return current_path
                if next_node not in previous_nodes:
                    new_path = current_path[:]
                    new_path.append(next_node)
                    path.append(new_path)
                    previous_nodes.add(next_node)
        path_index += 1
    return []


def get_max_flow() -> int:
    answer = 0
    while True:
        path = shortest_path(start, end)
        if not path:
            return answer
        delta = BIG
        for i in range(len(path) - 1):
            delta = min(delta, c[path[i]][path[i + 1]] - f[path[i]][path[i + 1]])
        for i in range(len(path) - 1):
            f[path[i]][path[i + 1]] += delta
            f[path[i + 1]][path[i]] -= delta
        answer += delta

print(s - get_max_flow())
