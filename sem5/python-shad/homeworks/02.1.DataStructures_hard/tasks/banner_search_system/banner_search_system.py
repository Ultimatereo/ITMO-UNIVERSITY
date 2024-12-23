import heapq
import typing as tp
from collections import defaultdict, Counter


def normalize(
        text: str
) -> str:
    """
    Removes punctuation and digits and convert to lower case
    :param text: text to normalize
    :return: normalized query
    """
    answer = "".join(char for char in text if char.isalpha() or char.isspace())
    return answer.lower()


def get_words(
        query: str
) -> list[str]:
    """
    Split by words and leave only words with letters greater than 3
    :param query: query to split
    :return: filtered and split query by words
    """
    return [word for word in query.split() if len(word) > 3]


def build_index(
        banners: list[str]
) -> dict[str, list[int]]:
    """
    Create index from words to banners ids with preserving order and without repetitions
    :param banners: list of banners for indexation
    :return: mapping from word to banners ids
    """
    index = defaultdict(list)

    for i, banner in enumerate(banners):
        words = Counter(get_words(normalize(banner)))
        for word in words:
            index[word].append(i)

    return index


def get_banner_indices_by_query(
        query: str,
        index: dict[str, list[int]]
) -> list[int]:
    """
    Extract banners indices from index, if all words from query contains in indexed banner
    :param query: query to find banners
    :param index: index to search banners
    :return: list of indices of suitable banners
    """
    words = get_words(normalize(query))
    sorted_lists = [index[word] for word in words]
    if not sorted_lists:
        return []

    cur_element = -1
    cur_counter = 0
    answer = []
    heap: list[tp.Any] = []

    for i, lst in enumerate(sorted_lists):
        if lst:
            heapq.heappush(heap, (lst[0], i, 0))

    while heap:
        val, list_idx, idx = heapq.heappop(heap)
        if cur_element != val:
            if cur_counter == len(sorted_lists):
                answer.append(cur_element)
            cur_element = val
            cur_counter = 0
        cur_counter += 1
        if idx + 1 < len(sorted_lists[list_idx]):
            heapq.heappush(heap, (sorted_lists[list_idx][idx + 1], list_idx, idx + 1))
    if cur_counter == len(sorted_lists):
        answer.append(cur_element)
    return answer


#########################
# Don't change this code
#########################

def get_banners(
        query: str,
        index: dict[str, list[int]],
        banners: list[str]
) -> list[str]:
    """
    Extract banners matched to queries
    :param query: query to match
    :param index: word-banner_ids index
    :param banners: list of banners
    :return: list of matched banners
    """
    indices = get_banner_indices_by_query(query, index)
    return [banners[i] for i in indices]

#########################
