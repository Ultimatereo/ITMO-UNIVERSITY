import numpy as np
import numpy.typing as npt


def nearest_value(matrix: npt.NDArray[np.float_], value: float) -> float | None:
    """
    Find nearest value in matrix.
    If matrix is empty return None
    :param matrix: input matrix
    :param value: value to find
    :return: nearest value in matrix or None
    """
    if matrix.size == 0:
        return None

    # Calculate the absolute differences between each element in the matrix and the target value.
    differences = np.abs(matrix - value)

    # Find the index of the element with the minimum absolute difference.
    nearest_index = np.argmin(differences)

    # Retrieve the nearest value using the index.
    nearest = matrix.flat[nearest_index]

    return float(nearest)
