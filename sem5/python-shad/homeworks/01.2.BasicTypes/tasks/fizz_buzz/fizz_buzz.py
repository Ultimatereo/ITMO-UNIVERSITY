def get_fizz_buzz(n: int) -> list[int | str]:
    """
    If value divided by 3 - "Fizz",
       value divided by 5 - "Buzz",
       value divided by 15 - "FizzBuzz",
    else - value.
    :param n: size of sequence
    :return: list of values.
    """
    mas: list[int | str] = []
    for i in range(1, n + 1):
        if i % 15 == 0:
            mas.append("FizzBuzz")
        elif i % 5 == 0:
            mas.append("Buzz")
        elif i % 3 == 0:
            mas.append("Fizz")
        else:
            mas.append(i)
    return mas
