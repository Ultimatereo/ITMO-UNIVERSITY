import numpy as np
import numpy.typing as npt


def vander(array: npt.NDArray[np.float_ | np.int_]) -> npt.NDArray[np.float_]:
    """
    Create a Vandermod matrix from the given vector.
    :param array: input array,
    :return: vandermonde matrix
    """
    n = len(array)
    # Create a 1-D array with powers of x
    powers = np.arange(n)

    # Use broadcasting to compute the Vandermonde matrix
    answer = array[:, np.newaxis] ** powers

    return answer
