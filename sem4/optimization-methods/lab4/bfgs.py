import numpy as np
from calc import gradient, ternary_search_wolfe


def bfgs(f, x_0, epochs=10000):
    iter_cnt = 0
    n = len(x_0)
    grad = gradient(f, x_0)
    p = -grad
    s = ternary_search_wolfe(f, x_0, p) * p
    y = gradient(f, x_0 + s) - grad
    I = np.identity(n)
    H = np.dot(y, s) / np.dot(y, y) * I
    x = x_0
    points = np.zeros((epochs + 1, n))
    points[0] = x_0
    while np.linalg.norm(s) > 1e-6 and iter_cnt < epochs:
        iter_cnt += 1
        p = np.dot(-H, grad)
        s = ternary_search_wolfe(f, x, p) * p
        x += s
        next_grad = gradient(f, x)
        y = next_grad - grad
        rho = 1 / np.dot(y, s)
        H = np.dot(I - rho * np.outer(s, y), np.dot(H, I - rho * np.outer(y, s))) + rho * np.outer(s, s)
        grad = next_grad
        points[iter_cnt] = x
    return points, iter_cnt
