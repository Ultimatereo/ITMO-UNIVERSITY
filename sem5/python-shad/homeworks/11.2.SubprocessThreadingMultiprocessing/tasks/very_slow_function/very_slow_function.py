import time
from threading import Thread
from multiprocessing import Pool


def very_slow_function(x: int) -> int:
    """Function which calculates square of given number really slowly
    :param x: given number
    :return: number ** 2
    """
    time.sleep(0.3)
    return x ** 2


def calc_squares_simple(bound: int) -> list[int]:
    """Function that calculates squares of numbers in range [0; bound)
    :param bound: positive upper bound for range
    :return: list of squared numbers
    """
    return [very_slow_function(x) for x in range(bound)]


def calc_squares_multithreading(bound: int) -> list[int]:
    """Function that calculates squares of numbers in range [0; bound)
    using threading.Thread
    :param bound: positive upper bound for range
    :return: list of squared numbers
    """
    result = []

    def calculate_square(x):  # type: ignore
        result.append(very_slow_function(x))

    threads = []
    for x in range(bound):
        thread = Thread(target=calculate_square, args=(x,))
        thread.start()
        threads.append(thread)

    for thread in threads:
        thread.join()

    return result


def calc_squares_multiprocessing(bound: int) -> list[int]:
    """Function that calculates squares of numbers in range [0; bound)
    using multiprocessing.Pool
    :param bound: positive upper bound for range
    :return: list of squared numbers
    """
    with Pool() as pool:
        result = pool.map(very_slow_function, range(bound))
    return result
