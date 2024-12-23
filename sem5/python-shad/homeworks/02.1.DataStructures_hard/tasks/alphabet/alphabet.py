def extract_alphabet(
        graph: dict[str, set[str]]
) -> list[str]:
    """
    Extract alphabet from graph
    :param graph: graph with partial order
    :return: alphabet
    """
    alphabet = []
    visited = set()

    def dfs(node: str) -> None:
        if node in visited:
            return
        visited.add(node)
        for neighbor in graph.get(node, []):
            dfs(neighbor)
        alphabet.append(node)

    for node in graph.keys():
        dfs(node)

    return alphabet[::-1]


def build_graph(
        words: list[str]
) -> dict[str, set[str]]:
    """
    Build graph from ordered words. Graph should contain all letters from words
    :param words: ordered words
    :return: graph
    """
    graph: dict[str, set[str]] = {}
    for word in words:
        for char in word:
            if char not in graph:
                graph[char] = set()
    for i in range(len(words) - 1):
        word1 = words[i]
        word2 = words[i + 1]

        for j in range(min(len(word1), len(word2))):
            if word1[j] != word2[j]:
                if word1[j] not in graph:
                    graph[word1[j]] = set()
                graph[word1[j]].add(word2[j])
                break
    return graph


#########################
# Don't change this code
#########################

def get_alphabet(
        words: list[str]
) -> list[str]:
    """
    Extract alphabet from sorted words
    :param words: sorted words
    :return: alphabet
    """
    graph = build_graph(words)
    return extract_alphabet(graph)

#########################
