import heapq
import typing as tp


def merge(sorted_lists: tp.Sequence[tp.Sequence[int]]) -> list[int]:
    """
    :param sorted_lists: sequence of sorted sequences
    :return: merged sorted list
    """
    if not sorted_lists:
        return []

    merged = []
    heap: list[tp.Any] = []

    for i, lst in enumerate(sorted_lists):
        if lst:
            heapq.heappush(heap, (lst[0], i, 0))

    while heap:
        val, list_idx, idx = heapq.heappop(heap)
        merged.append(val)

        if idx + 1 < len(sorted_lists[list_idx]):
            heapq.heappush(heap, (sorted_lists[list_idx][idx + 1], list_idx, idx + 1))

    return merged
