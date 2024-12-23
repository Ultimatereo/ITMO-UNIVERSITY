def merge_iterative(lst_a: list[int], lst_b: list[int]) -> list[int]:
    """
    Merge two sorted lists in one sorted list
    :param lst_a: first sorted list
    :param lst_b: second sorted list
    :return: merged sorted list
    """
    ind1 = 0
    ind2 = 0
    answer = []
    while ind1 != len(lst_a) or ind2 != len(lst_b):
        if ind1 == len(lst_a):
            answer.append(lst_b[ind2])
            ind2 += 1
        elif ind2 == len(lst_b):
            answer.append(lst_a[ind1])
            ind1 += 1
        elif lst_a[ind1] <= lst_b[ind2]:
            answer.append(lst_a[ind1])
            ind1 += 1
        else:
            answer.append(lst_b[ind2])
            ind2 += 1
    return answer


def merge_sorted(lst_a: list[int], lst_b: list[int]) -> list[int]:
    """
    Merge two sorted lists in one sorted list using `sorted`
    :param lst_a: first sorted list
    :param lst_b: second sorted list
    :return: merged sorted list
    """
    return sorted(lst_a + lst_b)
