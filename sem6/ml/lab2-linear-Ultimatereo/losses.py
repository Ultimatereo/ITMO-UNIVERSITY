import numpy as np


def get_loss(loss_name):
    match loss_name:
        case 'linear':
            return linear_loss
        case 'logarithmic':
            return logarithmic_loss
        case 'square':
            return square_loss
        case 'sigmoid':
            return sigmoid_loss
        case _:
            raise Exception('Unknown loss specified: '
                            'linear, logarithmic, square or sigmoid are available.')


def get_loss_gradient(loss_name):
    match loss_name:
        case 'linear':
            return linear_loss_gradient
        case 'logarithmic':
            return logarithmic_loss_gradient
        case 'square':
            return square_loss_gradient
        case 'sigmoid':
            return sigmoid_loss_gradient
        case _:
            raise Exception('Unknown loss specified: '
                            'linear, logarithmic, square or sigmoid are available.')


def linear_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        value += max(0, -np.dot(X[i], w) * y[i])
    return value / n


def logarithmic_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        value += np.log2(1 + np.exp(-np.dot(X[i], w) * y[i]))
    return value / n


def square_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        m = np.dot(X[i], w) * y[i]
        value += (1 - m) * (1 - m)
    return value / n


def sigmoid_loss(X, y, w):
    value = 0
    n = X.shape[0]
    for i in range(n):
        value += 2 / (1 + np.exp(np.dot(X[i], w) * y[i]))
    return value / n


def linear_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        if np.dot(X[i], w) * y[i] <= 0:
            value -= y[i] * X[i]
    return value


def logarithmic_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        value += X[i] * y[i] / (1 + np.exp(y[i] * np.dot(X[i], w)))
    return -value


def square_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        value -= 2 * y[i] * (1 - y[i] * np.dot(X[i], w)) * X[i]
    return value


def sigmoid_loss_gradient(X, y, w):
    value = np.zeros(X.shape[1])
    n = X.shape[0]
    for i in range(n):
        sigmoid = 1 / (1 + np.exp(y[i] * np.dot(X[i], w)))
        value -= 2 * y[i] * sigmoid * (1 - sigmoid) * X[i]
    return value

