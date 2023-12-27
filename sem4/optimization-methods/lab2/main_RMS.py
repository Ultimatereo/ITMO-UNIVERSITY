import numpy as np
import matplotlib.pyplot as plt

plt.rcParams["figure.figsize"] = (10, 10)
n = 1000
X = np.random.rand(n) * 10
Y = 5 * X + np.random.rand(n) * 20
fig, ax = plt.subplots()
ax.scatter(X, Y)

point = np.zeros(2)
eps = 10e-6
lr = 1
v = np.zeros(2)
alpha = 0.1


def gradient(point, j, X):
    return np.array(
        [2 * X[j] * (point[0] * X[j] + point[1] - Y[j]),
         2 * (point[0] * X[j] + point[1] - Y[j])])


s = 0
for epoch in range(1, 100):
    j = np.random.randint(0, n)
    gr = gradient(point, j, X)
    s = alpha * s + (1 - alpha) * (gr * gr)
    point = point - lr * gr / (np.sqrt(s + eps))
print(point)
ax.plot([0, 10], [point[1], 10 * point[0] + point[1]], color='red', linewidth=5)
plt.show()
