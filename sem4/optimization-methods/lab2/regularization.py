import numpy as np
import matplotlib.pyplot as plt
from paint_contour import paint_contour
from create_function import create_function

plt.rcParams["figure.figsize"] = (10, 10)


# Создаёт полиномиальную зависимость заданной степени с заданным разбросом
def random_polynomial(degree, x_scale, points_number, variation):
    X = (np.random.rand(points_number) - np.random.rand(points_number)) * x_scale
    Y = np.random.rand(points_number) * variation
    for n in range(degree + 1):
        Y += np.random.rand(1) * X ** n
    return X, Y


# Считает значение заданного слагаемого в заданной точке
def calc_summand(x, y, point):
    value = -y
    for i in range(len(point)):
        value += point[i] * x ** i
    return value * value


# Считает значение полинома в заданной точке
def calc_polynomial(x, coefs):
    y = 0
    for n in range(len(coefs)):
        y += coefs[n] * x ** n
    return y


# Считает градиент заданного слагаемого в заданной точке
def get_gradient(x, y, point, h=10e-6):
    gradient = np.zeros(len(point))
    for i in range(len(point)):
        delta = point.copy()
        delta[i] += h
        gradient[i] = (calc_summand(x, y, delta) - calc_summand(x, y, point)) / h
    return gradient


def get_norm_gradient(point, norm, h=10e-6):
    gradient = np.zeros(len(point))
    for i in range(len(point)):
        delta = point.copy()
        delta[i] += h
        gradient[i] = (np.linalg.norm(delta, ord=norm) ** norm - np.linalg.norm(point, ord=norm) ** norm) / h
    return gradient


# Считает градиент суммы заданных слагаемых (батча)
def sum_gradient(X, Y, point, summand_numbers):
    gradient = np.zeros(len(point))
    for i in summand_numbers:
        gradient += get_gradient(X[i], Y[i], point)
    return gradient


def gradient_descent(X, Y, start_point, learning_rate, epochs, batch_size, l1, l2):
    points = np.zeros((epochs, len(start_point)))
    points[0] = start_point
    lr = learning_rate
    for epoch in range(1, epochs):
        summand_numbers = [(epoch * batch_size + j) % len(X) for j in range(batch_size)]
        points[epoch] = points[epoch - 1] - lr * (sum_gradient(X, Y, points[epoch - 1], summand_numbers)
                                                  + l1 * get_norm_gradient(points[epoch - 1], 1)
                                                  + l2 * get_norm_gradient(points[epoch - 1], 2))
        if epoch % 100 == 0:
            lr *= 0.5
    return points


n = 4
x_scale = 2.5
learning_rate = 0.000009
epochs = 1000
batch_size = 1
l1 = 1
l2 = 0
degree = 5

start_point = np.array([1.33, 2.37, -3.06, -1.3, 0.902, 0.1])

X = np.array([-1.5, -0.4, 1, 2])
Y = np.array([-0.8, 0, 0.3, 1])
descent_points = gradient_descent(X, Y, start_point, learning_rate, epochs, batch_size, l1, l2)

fig, ax = plt.subplots()
ax.set_ylim(-2, 2)
ax.scatter(X, Y)

min_point = descent_points[-1]
X_values = np.linspace(-x_scale, x_scale, 200)
ax.plot(X_values, calc_polynomial(X_values, min_point), color='red')
plt.title("Polynomial regression with regularization\nDegree = " + str(degree) +
          ", L1 = " + str(l1) + ", L2 = " + str(l2))

plt.savefig("regularization/regression_regularization_L1=" + str(l1) + "_L2=" + str(l2) + "degree=" + str(degree) + ".png")
plt.cla()
