from collections.abc import Generator
from typing import Any


def transpose(matrix: list[list[Any]]) -> list[list[Any]]:
    """
    :param matrix: rectangular matrix
    :return: transposed matrix
    """
    if len(matrix) == 0:
        return []
    return [[matrix[i][j] for i in range(len(matrix))] for j in range(len(matrix[0]))]


def uniq(sequence: list[Any]) -> Generator[Any, None, None]:
    """
    :param sequence: arbitrary sequence of comparable elements
    :return: generator of elements of `sequence` in
    the same order without duplicates
    """
    seen = set()
    for item in sequence:
        if item not in seen:
            seen.add(item)
            yield item


def dict_merge(*dicts: dict[Any, Any]) -> dict[Any, Any]:
    """
    :param dicts: flat dictionaries to be merged
    :return: merged dictionary
    """
    return {k: v for d in dicts for k, v in d.items()}


def product(lhs: list[int], rhs: list[int]) -> int:
    """
    :param rhs: first factor
    :param lhs: second factor
    :return: scalar product
    """
    return sum([lhs[i] * rhs[i] for i in range(len(lhs))])
