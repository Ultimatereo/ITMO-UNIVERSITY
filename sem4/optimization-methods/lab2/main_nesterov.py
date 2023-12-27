import numpy as np
import matplotlib.pyplot as plt

plt.rcParams["figure.figsize"] = (10, 10)
n = 1000
X = np.random.rand(n) * 10
Y = 5 * X + np.random.rand(n) * 20
gamma = 0.5
fig, ax = plt.subplots()
ax.scatter(X, Y)

point = np.zeros(2)
h = 10e-6
lr = 0.0009
v = np.zeros(2)


def gradient(point, j, X):
    return np.array(
        [2 * X[j] * (point[0] * X[j] + point[1] - Y[j]),
         2 * (point[0] * X[j] + point[1] - Y[j])])


for epoch in range(1, 100):
    j = np.random.randint(0, n)
    v = gamma * v + (1 - gamma) * gradient(point - lr*gamma*v, j, X)
    point = point - lr * v
print(point)
ax.plot([0, 10], [point[1], 10 * point[0] + point[1]], color='red', linewidth=5)
plt.show()

