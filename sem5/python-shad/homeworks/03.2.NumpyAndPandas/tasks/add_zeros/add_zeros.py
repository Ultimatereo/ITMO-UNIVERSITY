import numpy as np
import numpy.typing as npt


def add_zeros(x: npt.NDArray[np.int_]) -> npt.NDArray[np.int_]:
    """
    Add zeros between values of given array
    :param x: array,
    :return: array with zeros inserted
    """
    # Calculate the new size of the array with zeros
    new_size = max(2 * len(x) - 1, 0)

    # Create an array of zeros with the new size
    resulto = np.zeros(new_size, dtype=np.int_)

    # Copy the elements from the original array into the new array with zeros
    resulto[::2] = x

    return resulto
