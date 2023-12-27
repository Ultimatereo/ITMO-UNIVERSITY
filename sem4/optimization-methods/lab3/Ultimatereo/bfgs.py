import time

import numpy as np
from matplotlib import pyplot as plt

from calc import gradient, ternary_search_wolfe
from paint_contour import paint_contour


def bfgs(f, x0, max_iter=1000, eps=1e-6):
    num_epoch = max_iter
    grad_count = 0
    points = np.zeros((max_iter + 1, 2))
    n = len(x0)
    x = x0
    B = np.eye(n)  # initial approximation of inverse Hessian
    grad = gradient(f, x)
    grad_count += 1
    for epoch in range(max_iter):
        points[epoch] = x
        p = -np.dot(B, grad)
        alpha, gr_c = ternary_search_wolfe(f, x, p)
        grad_count += gr_c
        x_new = x + alpha * p
        s = x_new - x
        grad_new = gradient(f, x_new)
        grad_count += 1
        y = grad_new - grad
        if np.linalg.norm(y) < eps:
            points[epoch + 1] = x_new
            num_epoch = epoch + 1
            break
        tt = np.dot(y, s)
        rho = 1 / tt
        B = (np.eye(n) - rho * np.outer(s, y)) @ B @ (np.eye(n) - rho * np.outer(y, s)) + rho * np.outer(s, s)
        x, grad = x_new, grad_new

    return points[:num_epoch].copy(), num_epoch, grad_count


f1 = lambda x: (1 - x[0]) ** 2 + 100 * (x[1] - x[0] ** 2) ** 2
f2 = lambda x: 0.01 * x[0] ** 2 + x[1] ** 2
f3 = lambda x: (1.5 - x[0] + x[0] * x[1]) ** 2 + (2.25 - x[0] + x[0] * x[1] ** 2) ** 2 + (
        2.625 - x[0] + x[0] * x[1] ** 3) ** 2
f4 = lambda x: 0.01*(0.25 * np.sqrt(np.exp(x[0]) + np.exp(-x[0]) + np.exp(x[1]) + np.exp(-x[1])) - x[0] ** 2 - 0.5 * x[0])
f = [f1, f2, f3, f4]
guesses = [[-2, 2], [-3, 3], [0, 0], [0, 3]]
accuracy = 1000
x_dif = 10
y_dif = 10
plt.rcParams["figure.figsize"] = (10, 10)
for i in range(len(f)):
    start = time.time()
    points, num_epoch, grad_count = bfgs(f[i], guesses[i], eps=1e-4)
    end = time.time()
    print("Time: " + str(end - start) + " seconds.")
    print("For " + str(i + 1) + " function it took " + str(num_epoch) + " iterations!")
    point = points[-1]
    print("Point: " + str(point))
    print("Grad Count: " + str(grad_count) + ".")
    print()
    # Draw depending on the point
    # paint_contour(point[0] - x_dif, point[0] + x_dif,
    #               point[1] - y_dif, point[1] + y_dif,
    #               accuracy, points, f[i])
    # Draw fixed
    if i == 3:
        paint_contour(-15, 15,
                      -15, 15,
                      accuracy, points, f[i])
    else:
        paint_contour(-5, 5,
                      -5, 5,
                      accuracy, points, f[i])
    plt.show()

