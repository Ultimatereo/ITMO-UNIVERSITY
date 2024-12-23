import typing as tp


def traverse_dictionary_immutable(
        dct: tp.Mapping[str, tp.Any],
        prefix: str = "") -> list[tuple[str, int]]:
    """
    :param dct: dictionary of undefined depth with integers or other dicts as leaves with same properties
    :param prefix: prefix for key used for passing total path through recursion
    :return: list with pairs: (full key from root to leaf joined by ".", value)
    """
    result: list[tuple[str, int]] = []
    for key in dct:
        if isinstance(dct[key], dict):
            res = traverse_dictionary_immutable(dct[key], prefix + str(key) + '.')
            for el in res:
                result.append(el)
        else:
            result.append((prefix + str(key), dct[key]))
    return result


def traverse_dictionary_mutable(
        dct: tp.Mapping[str, tp.Any],
        result: list[tuple[str, int]],
        prefix: str = "") -> None:
    """
    :param dct: dictionary of undefined depth with integers or other dicts as leaves with same properties
    :param result: list with pairs: (full key from root to leaf joined by ".", value)
    :param prefix: prefix for key used for passing total path through recursion
    :return: None
    """
    for key in dct:
        if isinstance(dct[key], dict):
            traverse_dictionary_mutable(dct[key], result, prefix + str(key) + '.')
        else:
            result.append((prefix + str(key), dct[key]))


def traverse_dictionary_iterative(
        dct: tp.Mapping[str, tp.Any]
) -> list[tuple[str, int]]:
    """
    :param dct: dictionary of undefined depth with integers or other dicts as leaves with same properties
    :return: list with pairs: (full key from root to leaf joined by ".", value)
    """
    result = []
    for key in dct:
        result.append((str(key), dct[key]))
    while True:
        change_results = []
        flag: bool = True
        for i in range(len(result)):
            pair = result[i]
            if isinstance(pair[1], dict):
                flag = False
                new_res = []
                for key in pair[1]:
                    new_res.append((pair[0] + '.' + str(key), pair[1][key]))
                for el in new_res:
                    change_results.append(el)
            else:
                change_results.append(pair)
        result = change_results
        if flag:
            break
    return result
