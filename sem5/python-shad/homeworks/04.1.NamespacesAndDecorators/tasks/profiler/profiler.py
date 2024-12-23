from datetime import datetime
from functools import wraps


def profiler(func):  # type: ignore
    """
    Returns profiling decorator, which counts calls of function
    and measure last function execution time.
    Results are stored as function attributes: `calls`, `last_time_taken`
    :param func: function to decorate
    :return: decorator, which wraps any function passed
    """

    @wraps(func)
    def wrapper(*args, **kwargs):  # type: ignore
        if wrapper.counter == 0:
            wrapper.calls = 0
            wrapper.last_time_taken = 0.0
        start_time = datetime.now()
        wrapper.counter += 1
        result = func(*args, **kwargs)
        wrapper.counter -= 1
        end_time = datetime.now()

        execution_time = (end_time - start_time).total_seconds()
        wrapper.calls += 1
        wrapper.last_time_taken = execution_time
        return result

    wrapper.calls = 0
    wrapper.last_time_taken = 0.0
    wrapper.counter = 0
    return wrapper
