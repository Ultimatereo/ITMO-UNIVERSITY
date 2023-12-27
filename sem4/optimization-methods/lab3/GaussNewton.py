import numpy as np
from scipy.optimize import curve_fit, approx_fprime

def residual(params, x, y_meas):
    a, b, c = params
    y_pred = a * np.exp(-b * x) + c
    return y_pred - y_meas

def gauss_newton(residual, x, y_meas, params, maxiter=50, eps=1e-6):
    """
    Gauss-Newton метод оптимизации
    """
    for i in range(maxiter):
        r = residual(params, x, y_meas)
        J = approx_fprime(params, residual, epsilon=eps)
        p = np.linalg.lstsq(J, -r, rcond=None)[0]
        params += p
        if np.sum(np.abs(p)) < eps:
            break
    return params

def powell_dogleg(residual, x, y_meas, params, maxiter=50, eps=1e-6):
    """
    Powell Dog Leg метод оптимизации
    """
    x0 = params
    for i in range(maxiter):
        r = residual(x0, x, y_meas)
        J = approx_fprime(x0, residual, epsilon=eps)
        g = J.T @ r
        B = J.T @ J
        p_h = -g @ g / (g @ B @ g) * g
        p_gn = np.linalg.lstsq(B, -g, rcond=None)[0]
        if np.linalg.norm(p_gn) <= 2 * np.linalg.norm(p_h):
            p = p_gn
            alpha = 1.0
        else:
            p = p_h + (np.linalg.norm(p_h) ** 2 - np.linalg.norm(p_gn) ** 2) / (2 * (p_h @ (p_gn - p_h)))
            alpha = np.linalg.norm(p_h) / (np.linalg.norm(p_h - p))
        x1 = x0 + alpha * p
        if np.sum(np.abs(x1 - x0)) < eps:
            break
        x0 = x1
    return x1

# Данные для регрессии
x = np.array([0, 1, 2, 3, 4, 5])
y_meas = np.array([1, 0.6, 0.45, 0.35, 0.28, 0.24])

# Начальные значения параметров
p0 = np.array([1.0, 0.1, 1.0])

# Решение задачи с помощью встроенной функции curve_fit
p_opt, _ = curve_fit(lambda x, a, b, c: a * np.exp(-b * x) + c, x, y_meas, p0=p0)

# Решение задачи с помощью методов Gauss-Newton и Powell Dog Leg
p_gn = gauss_newton(residual, x, y_meas, p0)
p_pd = powell_dogleg(residual, x, y_meas, p0)

print("Решение с помощью curve_fit: ", p_opt)
print("Решение с помощью метода Gauss-Newton: ", p_gn)
print("Решение с помощью метода Powell-Dogleg: ", p_pd)
