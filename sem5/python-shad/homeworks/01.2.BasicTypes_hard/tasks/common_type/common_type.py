_bool = type(True)
_int = type(1)
_float = type(0.0)
_complex = type(2j)
_range = type(range(1))
_tuple = type((0, 0))
_list = type([0, 0])
_str = type("")


def get_common_type(type1: type, type2: type) -> type:
    """
    Calculate common type according to rule, that it must have the most adequate interpretation after conversion.
    Look in tests for adequacy calibration.
    :param type1: one of [bool, int, float, complex, list, range, tuple, str] types
    :param type2: one of [bool, int, float, complex, list, range, tuple, str] types
    :return: the most concrete common type, which can be used to convert both input values
    """
    # bool <--- bool
    # int  <--- int!, bool
    # float <--- float!, int, bool
    # complex <--- complex!, float, int, bool
    # list <--- list!, tuple, range
    # tuple <--- tuple!, range
    # str - default
    # priority = {_bool: 0, _int: 1, _float: 2, _complex: 3, _range: 4, _tuple: 5, _list: 6, _str: 7}

    if check_bool(type1, type2) or check_bool(type2, type1):
        return _bool
    if check_for_int(type1, type2) or check_for_int(type2, type1):
        return _int
    if check_for_float(type1, type2) or check_for_float(type2, type1):
        return _float
    if check_for_complex(type1, type2) or check_for_complex(type2, type1):
        return _complex
    if check_for_tuple(type1, type2) or check_for_tuple(type2, type1):
        return _tuple
    if check_for_list(type1, type2) or check_for_list(type2, type1):
        return _list
    return str


def check_for_list(type1: type, type2: type) -> bool:
    return type1 == _list and type2 in [_list, _tuple, _range]


def check_for_tuple(type1: type, type2: type) -> bool:
    return (type1 == _tuple and type2 in [_tuple, _range]) or (type1 == _range and type2 == _range)


def check_for_complex(type1: type, type2: type) -> bool:
    return type1 == _complex and type2 in [_complex, _float, _int, _bool]


def check_for_float(type1: type, type2: type) -> bool:
    return type1 == _float and type2 in [_float, _int, _bool]


def check_for_int(type1: type, type2: type) -> bool:
    return type1 == _int and type2 in [_int, _bool]


def check_bool(type1: type, type2: type) -> bool:
    return type1 == _bool and type2 == _bool
