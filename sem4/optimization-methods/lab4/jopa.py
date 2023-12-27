import numpy as np
from scipy.optimize import minimize
from LBFGS import l_bfgs
from bfgs import bfgs
from numdifftools import Jacobian, Hessian


def f(x):
    return (1 - x[0])**2 + 100 * (x[1] - x[0]**2)**2


def fun_der(x, a):
    return Jacobian(lambda x: f(x, a))(x).ravel()


def fun_hess(x, a):
    return Hessian(lambda x: f(x, a))(x)


a = 2.5
start_point = np.array([-2, 2])
print(minimize(f, start_point, method="BFGS"))
print('########################################################')
print(l_bfgs(f, [-2, 2]))
