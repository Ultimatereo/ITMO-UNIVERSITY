def filter_list_by_list(lst_a: list[int] | range, lst_b: list[int] | range) -> list[int]:
    """
    Filter first sorted list by other sorted list
    :param lst_a: first sorted list
    :param lst_b: second sorted list
    :return: filtered sorted list
    """
    answer = []
    ind1 = 0
    ind2 = 0
    while ind1 < len(lst_a) and ind2 < len(lst_b):
        if lst_b[ind2] < lst_a[ind1]:
            ind2 += 1
        elif lst_b[ind2] == lst_a[ind1]:
            ind1 += 1
        else:
            answer.append(lst_a[ind1])
            ind1 += 1
    while ind1 < len(lst_a):
        answer.append(lst_a[ind1])
        ind1 += 1
    return answer
