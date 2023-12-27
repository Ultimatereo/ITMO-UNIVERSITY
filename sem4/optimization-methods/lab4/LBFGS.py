import collections
import calc
import numpy as np


def l_bfgs(f, start_point, epoch=1000,  m=10):
    # инициируем некоторые константы и начальные значения
    grad_eval = 0
    fun_eval = 0
    grad_eps = 1e-7
    x = start_point
    dim = len(start_point)
    g = calc.gradient(f, x)
    grad_eval += 1
    fun_eval += 2*len(x)
    points = np.zeros((epoch + 1, dim))
    points[0] = start_point

    alpha = np.zeros(m)
    beta = np.zeros(m)

    # заводим дэк для хранения не более m изменений значений
    last_updates_s = collections.deque()
    last_updates_y = collections.deque()
    last_updates_ro = collections.deque()

    # инициируем стартовое смещение
    z = 0.018 * calc.gradient(f, x)
    count_epochs = 0
    for k in range(1, epoch+1):

        # получаем новое значение х и высчитываем на delta_x и delta_gradient
        x = x - z
        points[k] = x
        g_1 = calc.gradient(f, x)
        grad_eval += 1
        fun_eval += 2*len(x)
        s = points[k] - points[k - 1]
        y = g_1 - g
        ro = 1 / (np.dot(y, s))
        g = g_1

        # если норма градиента меньше эпсилон, то стоп
        if np.linalg.norm(g) < grad_eps:
            print("Потребовалось итераций " + str(k))
            count_epochs = k
            break
        if k > m:
            # в дэке должно быть не больше m элементов
            last_updates_s.popleft()
            last_updates_y.popleft()
            last_updates_ro.popleft()

        # добавляем элементы в дэк
        last_updates_s.append(s)
        last_updates_y.append(y)
        last_updates_ro.append(ro)

        # вычисляем новое смещение
        q = g
        size = len(last_updates_s)
        for i in range(size - 1, -1, -1):
            alpha[i] = last_updates_ro[i] * np.dot(last_updates_s[i], q)
            q = q - alpha[i] * last_updates_y[i]
        gamma = np.dot(last_updates_s[size-1], last_updates_y[size-1]) / np.dot(last_updates_y[size - 1],
                                                                                last_updates_y[size - 1])
        matrix_h = gamma * np.identity(dim)
        z = np.dot(matrix_h, q)
        for i in range(size):
            beta[i] = last_updates_ro[i] * np.dot(last_updates_y[i], z)
            z = z + last_updates_s[i] * (alpha[i] - beta[i])
    return x, count_epochs, fun_eval, grad_eval



