import numpy as np


def gradient(f, x, h=10e-6):
    grad = np.zeros(len(x))
    delta_x = x.copy()
    for i in range(len(x)):
        delta_x[i] += h
        grad[i] = (f(delta_x) - f(x)) / h
        delta_x[i] -= h
    return grad


def ternary_search_wolfe(f, x, p, left=0, right=10, c1=1e-4, c2=0.9, eps=1e-4):
    alpha = 0
    while right - left > eps:
        a = (left * 2 + right) / 3
        b = (right * 2 + left) / 3
        alpha = (left + right) / 2
        grad_f_x = gradient(f, x)
        if f(x + alpha * p) <= f(x) + c1 * alpha * np.dot(grad_f_x, p):
            if np.dot(gradient(f, x + alpha * p), p) >= c2 * np.dot(grad_f_x, p):
                return alpha
        if f(x + a * p) < f(x + b * p):
            right = b
        else:
            left = a
    return alpha
