from time import time
import torch
import numpy as np
from torch import gradient as torch_grad
from calc import gradient as our_grad
from numpy import gradient as np_grad
from numdifftools import Gradient as ndGrad


def quadratic(x):
    return x[0]**2


def quadratic_2(x):
    return x[0] ** 2 + x[1] ** 2


def f(x):
    return x[0] ** 2 + 2 * x[0] * x[1] - 4 * x[1] ** 2


def rozenbrock(x):
    return (1 - x[0]) ** 2 + 100 * (x[1] - x[0] ** 2) ** 2


print('rozenbrock:')
points = [[-2, 0], [0, 0], [2, 0], [-2, 2], [0, 2], [2, 2]]
X = [-2, 0, 2]
Y = [0, 2]
res = [rozenbrock(x) for x in points]
res_X = res[:3]
res_Y = res[3:]
print(points)
print(res)
print(res_X, res_Y)
t = torch.tensor([res_X, res_Y])
# coordinates = (torch.tensor(X), torch.tensor(Y))
# values = torch.tensor(res, )

start_time = time()
res_grad_torch = torch_grad(t, dim=1)
time_torch = (time() - start_time) * 10 ** 3

start_time = time()
res_grad_our_h1 = [our_grad(rozenbrock, x) for x in points]
time_our_h1 = time() * 10 ** 3 - start_time * 10 ** 3


start_time = time()
res_grad_our_h2 = [our_grad(rozenbrock, x, h=1e-7) for x in points]
time_our_h2 = time() * 10 ** 3 - start_time * 10 ** 3

start_time = time()
res_grad_np = np_grad([res_X, res_Y])
time_numpy = time() * 10 ** 3 - start_time * 10 ** 3

start_time = time()
grad_numdiff = ndGrad(rozenbrock)
res_grad_numdiff = [grad_numdiff(x) for x in points]
time_numdiff = time() * 10 ** 3 - start_time * 10 ** 3

print('Torch: ', res_grad_torch, 'time=', time_torch)
print('Our: h=10e^-6', res_grad_our_h1, 'time=', time_our_h1)
print('Our: h=1e^-7', res_grad_our_h2, 'time=', time_our_h2)
print('Numpy: ', res_grad_np, 'time=', time_numpy)
print('Numdifftools: ', res_grad_numdiff, 'time=', time_numdiff)
