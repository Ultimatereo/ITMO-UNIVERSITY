import numpy as np
import numpy.typing as npt


def nonzero_product(matrix: npt.NDArray[np.int_]) -> int | None:
    """
    Compute product of nonzero diagonal elements of matrix
    If all diagonal elements are zeros, then return None
    :param matrix: array,
    :return: product value or None
    """
    if len(matrix) == 0:
        return None
    count = 0
    m = 1
    for i in range(min(len(matrix), len(matrix[0]))):
        if matrix[i][i] != 0:
            count += 1
            m *= matrix[i][i]
    if count == 0:
        return None
    return m
