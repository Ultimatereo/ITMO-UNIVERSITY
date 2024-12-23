def problem01() -> dict[int, str]:
    return {
        5: "Operator + is not supported for None",
        7: "Mypy is not that smart to notice that 'a' in dictionary "
           "so it would consider that None can be returned"
    }


def problem02() -> dict[int, str]:
    return {
        5: "It is list of objects and it's not guaranteed that += works for object."
    }


def problem03() -> dict[int, str]:
    return {
        9: "Set is invariant and you can't pass to foo anything but set of ints.",
        13: "Same reason."
    }


def problem04() -> dict[int, str]:
    return {
        9: "This case is covariant and you can use any subtype of int however float is not one"
    }


def problem05() -> dict[int, str]:
    return {
        11: "Function returns string not B"
    }


def problem06() -> dict[int, str]:
    return {
        15: "S is subtype of T in typing sense and C is subclass of B so if in B we want to define a function using S()"
            " and in C we are not gonna redefine it then we may face some problems cuz of the more general value."
    }


def problem07() -> dict[int, str]:
    return {
        25: "Return type should be covariant to B and A is the opposite of it.",
        27: "Callable arguments are contravariant and B is not usable here.",
        28: "Same reason."
    }


def problem08() -> dict[int, str]:
    return {
        6: "Iterable doesn't necessarily have len method",
        18: "Class A doesn't have iter method thus it's not iterable",
        24: "Class B is iterable but doesn't suit generic str it is iterable with ints instead."
    }


def problem09() -> dict[int, str]:
    return {
        32: "Protocol doesn't guarantee that contains is implemented in Fooable",
        34: "No foo supported in []",
        37: "Class C doesn't suit protocol cuz it doesn't have len method",
        38: "No len supported in foo"
    }


def problem10() -> dict[int, str]:
    return {
        18: "Str doesn't support float conversion",
        29: "A[float] is not covariant to A[int] cuz float is not covariant to int"
    }
