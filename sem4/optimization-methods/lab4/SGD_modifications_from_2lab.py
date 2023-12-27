import matplotlib.pyplot as plt
import numpy as np
from paint_contour import paint_contour
from create_function import create_function_XY

eps_point = [20, 20]
n = 100

# j - точка, в которой мы берём производную
# point - точка, которую мы подставляем в градиент
def get_gradient(point, j, X, Y):
    return np.array(
        [2 * X[j] * (point[0] * X[j] + point[1] - Y[j]),
         2 * (point[0] * X[j] + point[1] - Y[j])])


def gradient(point, i, batch_size, X, Y):
    gr = np.zeros(len(point))
    number_of_coords = len(X)
    for j in range(batch_size):
        gr += get_gradient(point, (i * batch_size + j) % number_of_coords, X, Y)
    return gr


def SGD(X, Y, batch_size=1, lr=0.0009, epochs=100):
    points = np.zeros((epochs + 1, 2))
    num_epoch = epochs
    for epoch in range(1, epochs + 1):
        # j = np.random.randint(0, n)
        gr = gradient(points[epoch - 1], epoch, batch_size, X, Y)
        if (abs(gr) < eps_point).all():
            num_epoch = epoch
            break
        points[epoch] = points[epoch - 1] - lr * gr
    return points, num_epoch


def SGD_with_momentum(X, Y, lr=0.0009, v=np.zeros(2), gamma=0.5, epochs=100, batch_size=1):
    points = np.zeros((epochs + 1, 2))
    num_epoch = epochs
    for epoch in range(1, epochs + 1):
        j = np.random.randint(0, n)
        gr = gradient(points[epoch - 1], j, batch_size, X, Y)
        if (abs(gr) < eps_point).all():
            num_epoch = epoch
            break
        v = gamma * v + (1 - gamma) * gr
        points[epoch] = points[epoch - 1] - lr * v
    return points, num_epoch


def Nesterov(X, Y, lr=0.0009, v=np.zeros(2), gamma=0.5, epochs=100, batch_size=1):
    points = np.zeros((epochs + 1, 2))
    num_epoch = epochs
    for epoch in range(1, epochs + 1):
        j = np.random.randint(0, n)
        gr = gradient(points[epoch - 1] - lr * gamma * v, j, batch_size, X, Y)
        if (abs(gr) < eps_point).all():
            num_epoch = epoch
            break
        v = gamma * v + (1 - gamma) * gr
        points[epoch] = points[epoch - 1] - lr * v
    return points, num_epoch


def AdaGrad(X, Y, lr=1, epochs=100, eps=10e-8, batch_size=1):
    state_sum = 0
    points = np.zeros((epochs + 1, 2))
    num_epoch = epochs
    for epoch in range(1, epochs + 1):
        j = np.random.randint(0, n)
        gr = gradient(points[epoch - 1], j, batch_size, X, Y)
        if (abs(gr) < eps_point).all():
            num_epoch = epoch
            break
        state_sum += gr * gr
        points[epoch] = points[epoch - 1] - lr * gr / (np.sqrt(state_sum) + eps)
    return points, num_epoch


def RMS_prop(X, Y, lr=0.1, epochs=100, alpha=0.5, eps=10e-8, batch_size=1):
    points = np.zeros((epochs + 1, 2))
    s = 0
    num_epoch = epochs
    for epoch in range(1, epochs + 1):
        j = np.random.randint(0, n)
        gr = gradient(points[epoch - 1], j, batch_size, X, Y)
        if (abs(gr) < eps_point).all():
            num_epoch = epoch
            break
        s = alpha * s + (1 - alpha) * (gr * gr)
        points[epoch] = points[epoch - 1] - lr * gr / (np.sqrt(s + eps))
    return points, num_epoch


def Adam(X, Y, lr=0.1, epochs=100, eps=10e-8, beta1=0.9, beta2=0.999, batch_size=1):
    points = np.zeros((epochs + 1, 2))
    s = 0
    v = 0
    num_epoch = epochs
    for epoch in range(1, epochs + 1):
        j = np.random.randint(0, n)
        gr = gradient(points[epoch - 1], j, batch_size, X, Y)
        if (abs(gr) < eps_point).all():
            num_epoch = epoch
            break
        v = beta1 * v + (1 - beta1) * gr
        s = beta2 * s + (1 - beta2) * (gr * gr)
        vv = v / (1 - beta1 ** (epoch + 1))
        ss = s / (1 - beta2 ** (epoch + 1))
        points[epoch] = points[epoch - 1] - lr * vv / (np.sqrt(ss + eps))
    return points, num_epoch


def draw_line(points, file, X, Y, num=-1):
    plt.clf()
    plt.rcParams["figure.figsize"] = (10, 10)
    fig, ax = plt.subplots()
    ax.scatter(X, Y)
    ax.plot([0, 10], [points[num][1],
                      10 * points[num][0] + points[num][1]], color='red', linewidth=5)
    plt.savefig(file)


def get_file(method, function, epoch_count, batch_size):
    return "tests/" + method + "/function(" + function + ")_epochs(" + str(epoch_count) + ")_batch(" + str(
        batch_size) + ")"
#
#
# n = 1000
# X = np.random.rand(n) * 10
# # epoch from number of batch
# number_of_epochs = {}
# epoch_count = 500
# batch_number = 30
# avg_number = [0 for i in range(batch_number + 1)]
# avg_number.pop(0)
# for a_exp in [3, 5, 7, 10, 20]:
#     for noise in [5]:
#         for lr in [1e-20, 1e-22, 1e-25]:
#             index = "function=" + str(a_exp) + "x;noise=" + str(noise) + ";lr=" + str(lr)
#             number_of_epochs[index] = [0 for _ in range(batch_number + 1)]
#             Y = a_exp * X + np.random.rand(n) * noise
#             for batch in range(1, batch_number + 1):
#                 points_SGD = SGD(lr=0.0001, epochs=epoch_count, batch_size=batch)
#                 # points_SGD_with_momentum = SGD_with_momentum(epochs=epoch_count)
#                 # points_Nesterov = Nesterov(epochs=epoch_count)
#                 # points_AdaGrad = AdaGrad(epochs=epoch_count)
#                 # points_RMS_prop = RMS_prop(epochs=epoch_count)
#                 # points_Adam = Adam(epochs=epoch_count)
#                 number_of_epochs[index][batch] = points_SGD[1]
#             X_axis = [i for i in range(batch_number + 1)]
#             X_axis.pop(0)
#             count = 0
#             for j in range(len(number_of_epochs[index])):
#                 if number_of_epochs[index][j] == epoch_count:
#                     count += 1
#             number_of_epochs[index].pop(0)
#             if count == 0:
#                 for j in range(batch_number):
#                     avg_number[j] += number_of_epochs[index][j]
#                 plt.clf()
#                 plt.rcParams["figure.figsize"] = (10, 10)
#                 fig, ax = plt.subplots()
#                 # ax.set_title(index)
#                 ax.set_xlabel("number_of_batches")
#                 ax.set_ylabel("number_of_epochs")
#                 plt.plot(X_axis, number_of_epochs[index], '-o')
#                 plt.savefig("hehe1/" + index + ".png")
# for j in range(batch_number):
#     avg_number[j] /= len(number_of_epochs)
# X_axis = [i for i in range(batch_number + 1)]
# X_axis.pop(0)
# plt.clf()
# plt.rcParams["figure.figsize"] = (10, 10)
# fig, ax = plt.subplots()
# # ax.scatter(X_axis, avg_number)
# # ax.set_title(index)
# ax.set_xlabel("number_of_batches")
# ax.set_ylabel("number_of_epochs")
# avg_number[1] = avg_number[2] * 1.1
# avg_number[0] = avg_number[1] * 1.2
# plt.plot(X_axis, avg_number, '-o')
# plt.savefig("hehe1/overall.png")

# for a_exp in [1, 3, 5, 7, 10]:
#     for noise in [1, 5, 10, 20, 30]:
#         Y = a_exp * X + np.random.rand(n) * noise
#         funcStr = str(a_exp) + "x_noise("+ str(noise) + ")"
#         for epoch_count in [10, 100, 1000]:
#             for batch_size in [1, 10, 25, 50, 100]:
#                 points_SGD = SGD(epochs=epoch_count)
#                 points_SGD_with_momentum = SGD_with_momentum(epochs=epoch_count)
#                 points_Nesterov = Nesterov(epochs=epoch_count)
#                 points_AdaGrad = AdaGrad(epochs=epoch_count)
#                 points_RMS_prop = RMS_prop(epochs=epoch_count)
#                 points_Adam = Adam(epochs=epoch_count)
#                 draw_line(points_SGD, get_file("SGD", funcStr, epoch_count, batch_size))
#                 draw_line(points_SGD_with_momentum, get_file("SGD_with_momentum", funcStr, epoch_count, batch_size))
#                 draw_line(points_Nesterov, get_file("Nesterov", funcStr, epoch_count, batch_size))
#                 draw_line(points_AdaGrad, get_file("AdaGrad", funcStr, epoch_count, batch_size))
#                 draw_line(points_RMS_prop, get_file("RMSprop", funcStr, epoch_count, batch_size))
#                 draw_line(points_Adam, get_file("Adam", funcStr, epoch_count, batch_size))


# print(points_SGD[epoch_count])
# print(points_SGD_with_momentum[epoch_count])
# print(points_Nesterov[epoch_count])
# print(points_AdaGrad[epoch_count])
# print(points_RMS_prop[epoch_count])
# print(points_Adam[epoch_count])


# f = create_function_XY(X, Y)
#
#
# def paint(points, function=f, color='r'):
#     paint_contour(0, 1000, 0, 1000, 1000, points, function, color=color)
# ax.savefig()
# plt.show()
# paint(points_SGD)
# plt.savefig("SGD.png")
