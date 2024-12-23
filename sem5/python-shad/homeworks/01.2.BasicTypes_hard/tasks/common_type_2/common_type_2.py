import typing as tp

_bool = type(True)
_int = type(1)
_float = type(0.0)
_range = type(range(1))
_tuple = type((0, 0))
_list = type([0, 0])
_str = type("")
nothing = ["", None]
convert_nothing = {_list: [], _int: 0, _float: 0.0, _bool: False, _str: ""}


def get_common_type(type1: type, type2: type) -> type:
    """
    Calculate common type according to rule, that it must have the most adequate interpretation after conversion.
    Look in tests for adequacy calibration.
    :param type1: one of [bool, int, float, complex, list, range, tuple, str] types
    :param type2: one of [bool, int, float, complex, list, range, tuple, str] types
    :return: the most concrete common type, which can be used to convert both input values
    """
    # bool <--- bool, int[0 or 1]
    # int  <--- int!, bool
    # float <--- float!, int, bool
    # list <--- list!, tuple, range
    # tuple <--- tuple!, range
    # str - default
    if check_bool(type1, type2) or check_bool(type2, type1):
        return _bool
    if check_for_int(type1, type2) or check_for_int(type2, type1):
        return _int
    if check_for_float(type1, type2) or check_for_float(type2, type1):
        return _float
    if check_for_tuple(type1, type2) or check_for_tuple(type2, type1):
        return _list
    if check_for_list(type1, type2) or check_for_list(type2, type1):
        return _list
    return str


def check_for_list(type1: type, type2: type) -> bool:
    return type1 == _list and type2 in [_list, _tuple, _range, _str, _int, _float, _bool]


def check_for_tuple(type1: type, type2: type) -> bool:
    return type1 == _tuple and type2 in [_tuple, _range, _str, _int, _float, _bool]


def check_for_float(type1: type, type2: type) -> bool:
    return type1 == _float and type2 in [_float, _int, _bool]


def check_for_int(type1: type, type2: type) -> bool:
    return type1 == _int and type2 in [_int, _bool]


def check_bool(type1: type, type2: type) -> bool:
    return type1 == _bool and type2 in [_bool, _int]


def convert_to_list(x: tp.Any) -> list[tp.Any]:
    if x in nothing:
        return []
    if type(x) in [_str, _int, _float, _bool]:
        return [x]
    return list(x)


def convert_to_common_type(data: list[tp.Any]) -> list[tp.Any]:
    """
    Takes list of multiple types' elements and convert each element to common type according to given rules
    :param data: list of multiple types' elements
    :return: list with elements converted to common type
    """
    if all(item is None or item == "" for item in data):
        return ["" for _ in range(len(data))]
    res = _str
    for item in data:
        if item not in nothing:
            res = type(item)
            break
    for i in range(1, len(data)):
        if data[i] not in nothing:
            res = get_common_type(res, type(data[i]))
    if res == _list:
        return list(map(convert_to_list, data))
    return list(map(lambda x: res(x) if x not in nothing else convert_nothing[res], data))
