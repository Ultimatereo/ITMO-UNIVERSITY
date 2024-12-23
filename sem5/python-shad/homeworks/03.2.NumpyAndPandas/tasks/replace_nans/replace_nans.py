import numpy as np
import numpy.typing as npt


def replace_nans(matrix: npt.NDArray[np.float_]) -> npt.NDArray[np.float_]:
    """
    Replace all nans in matrix with average of other values.
    If all values are nans, then return zero matrix of the same size.
    :param matrix: matrix,
    :return: replaced matrix
    """
    # Check if all values in the matrix are NaN
    if np.all(np.isnan(matrix)):
        return np.zeros_like(matrix)  # Return a zero matrix of the same size

    # Calculate the mean of non-NaN values
    mean_value = np.nanmean(matrix)

    # Replace NaN values with the calculated mean
    matrix[np.isnan(matrix)] = mean_value

    return matrix
