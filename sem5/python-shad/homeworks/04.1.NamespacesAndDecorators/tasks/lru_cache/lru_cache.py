from collections import OrderedDict
from collections.abc import Callable
from functools import wraps
from typing import TypeVar, ParamSpec
import typing as tp

P = ParamSpec('P')
T = TypeVar('T')


def cache(max_size: int) -> Callable[[Callable[P, T]], Callable[P, T]]:
    """
    Returns decorator, which stores result of function
    for `max_size` most recent function arguments.
    :param max_size: max amount of unique arguments to store values for
    :return: decorator, which wraps any function passed
    """
    cache_dict: OrderedDict[tp.Any, tp.Any] = OrderedDict()

    def decorator(func: Callable[P, T]) -> Callable[P, T]:
        @wraps(func)
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
            key = args + tuple(kwargs.items())
            if key in cache_dict:
                value = cache_dict.pop(key)
                cache_dict[key] = value
            else:
                value = func(*args, **kwargs)
                if len(cache_dict) >= max_size:
                    cache_dict.popitem(last=False)
                cache_dict[key] = value
            return value

        return wrapper

    return decorator
