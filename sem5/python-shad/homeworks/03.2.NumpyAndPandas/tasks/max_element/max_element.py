import numpy as np
import numpy.typing as npt


def max_element(array: npt.NDArray[np.int_]) -> int | None:
    """
    Return max element before zero for input array.
    If appropriate elements are absent, then return None
    :param array: array,
    :return: max element value or None
    """
    max_el = -99999999999999999999999
    for i in range(1, len(array)):
        if array[i] > max_el and array[i - 1] == 0:
            max_el = array[i]
    if max_el == -99999999999999999999999:
        return None
    return max_el
