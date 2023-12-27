import numpy as np
import scipy.optimize as sp_opt


# dogleg/Powell
# BFGS ?
# L-BFGS-B


def f(x):
    return 0.25 * np.sqrt(np.exp(x[0]) + np.exp(-x[0]) + np.exp(x[1]) + np.exp(-x[1])) - x[0] ** 2 - 0.5 * x[0]


d = dict()
d['Powell'] = dict()
d['L-BFGS-B'] = dict()
start_points = [[3, -1], [9, -1], [-3, -7], [17, 7], [10.75, -1.5]]
bounds_x = [[-5, 15], [8, 13], [-5, 0], [15, 20], [10.25, 20]]
bounds_y = [[-5, 15], [-3, 3], [-10, -5], [5, 10], [-10, 0]]
for i in range(len(bounds_x)):
    start_point = np.array(start_points[i])
    x_min = bounds_x[i][0]
    x_max = bounds_x[i][1]
    y_min = bounds_y[i][0]
    y_max = bounds_y[i][1]
    key = str(x_min) + ' ' + str(y_min)
    bounds = sp_opt.Bounds(lb=[x_min, y_min], ub=[x_max, y_max])
    res = sp_opt.minimize(f, start_point, bounds=bounds, method='Powell')
    t = 0
    if 'njev' in res:
        t = res['njev']
    d['Powell'][key] = [res['x'], res['nit'], res['nfev'], t]

    res = sp_opt.minimize(f, start_point, bounds=bounds, method='L-BFGS-B')
    t = 0
    if 'njev' in res:
        t = res['njev']
    d['L-BFGS-B'][key] = [res['x'], res['nit'], res['nfev'], t]
print(d['Powell'])
print(d['L-BFGS-B'])
