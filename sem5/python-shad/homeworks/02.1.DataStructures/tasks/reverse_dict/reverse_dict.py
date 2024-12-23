import typing as tp


def revert(dct: tp.Mapping[str, str]) -> dict[str, list[str]]:
    """
    :param dct: dictionary to revert in format {key: value}
    :return: reverted dictionary {value: [key1, key2, key3]}
    """
    answer: dict[str, list[str]] = {}
    for key in dct:
        if dct[key] not in answer:
            answer[dct[key]] = []
        answer[dct[key]].append(key)
    return answer
