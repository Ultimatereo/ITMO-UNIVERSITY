import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
import os
from SGD_modifications_from_2lab import *

eps_point = [20, 20]
modes = {'SGD': 0.0009, 'Momentum': 0.0009, 'Nesterov': 0.0009, 'Adam': 1, 'AdaGrad': 1, 'RMSProp': 0.1}


def array_to_torch(A):
    return torch.from_numpy(np.array([[a] for a in A])).float()


def get_mini_batch(X, Y, i, batch_size):
    X_mini = []
    Y_mini = []
    for j in range(batch_size):
        X_mini.append(X[(i * batch_size + j) % X.size()[0]].item())
        Y_mini.append(Y[(i * batch_size + j) % Y.size()[0]].item())
    return array_to_torch(X_mini), array_to_torch(Y_mini)


def SGD_PT(X, Y, lr, mode='SGD', epochs=100, log=False, initial_guess=None, batch_size = 1):
    # Модель линейной регрессии
    # Если взять, что у нас Y = wX + b, то w = weight в нашей модели, а b - bias
    if initial_guess is None:
        initial_guess = [0, 0]
    points = np.zeros((epochs + 1, 2))
    model = nn.Linear(1, 1)
    model.weight.data = torch.tensor([[float(initial_guess[0])]])
    model.bias.data = torch.tensor([[float(initial_guess[1])]])
    # Функция потерь
    criterion = nn.MSELoss()
    optimizer = ''
    # Оптимизатор и скорость обучения
    # Ordinary SGD
    if mode == 'SGD':
        optimizer = optim.SGD(model.parameters(), lr=lr)
    # SGD with momentum
    elif mode == 'Momentum':
        optimizer = optim.SGD(model.parameters(), lr=lr, momentum=0.9)
    # SGD Nesterov
    elif mode == 'Nesterov':
        optimizer = optim.SGD(model.parameters(), lr=lr, nesterov=True, momentum=0.9)
    # AdaGrad
    elif mode == 'AdaGrad':
        optimizer = optim.Adagrad(model.parameters(), lr=lr)
    # RMSProp
    elif mode == 'RMSProp':
        optimizer = optim.RMSprop(model.parameters(), lr=lr)
    # Adam
    elif mode == 'Adam':
        optimizer = optim.Adam(model.parameters(), lr=lr)
    num_epoch = epochs
    for epoch in range(epochs):
        # Подсчитываем ошибку модели

        X_mini, Y_mini = get_mini_batch(X, Y, np.random.randint(0, X.size()[0]), batch_size)
        outputs = model(X_mini)
        loss = criterion(outputs, Y_mini)

        # Обнуляем градиент, если мы не хотим накапливать градиент
        optimizer.zero_grad()
        # Обновляем градиент
        loss.backward()

        gradient_values = [param.grad.item() for param in model.parameters()]
        if abs(gradient_values[0]) < eps_point[0] and abs(gradient_values[1]) < eps_point[1]:
            print('Epoch [{}/{}], Loss: {:.4f}'.format(epoch + 1, epochs, loss.item()))
            print('Predicted:', model.weight.item(), model.bias.item())
            num_epoch = epoch
            break
        # Делаем шаг градиентного спуска
        optimizer.step()
        points[epoch + 1] = np.array([model.weight.item(), model.bias.item()])
        if log:
            # Выводим промежуточные результаты
            if (epoch + 1) % 10 == 0:
                print('Epoch [{}/{}], Loss: {:.4f}'.format(epoch + 1, epochs, loss.item()))
                print('Predicted:', model.weight.item(), model.bias.item())
    return points, num_epoch


n = 100
X = np.random.rand(n) * 10
# epoch from number of batch
number_of_epochs = {}
# epoch_count = 500
# batch_number = 30
for a_exp in [1, 3, 5, 7, 10]:
    for noise in [1, 5, 10, 20, 30]:
        Y = a_exp * X + np.random.rand(n) * noise
        funcStr = str(a_exp) + "x_noise(" + str(noise) + ")"
        for batch in [1, 10, 25, 50, 100]:
            for epoch_count in [10, 100, 1000]:
                # points_SGD, nSGD = SGD(X, Y, epochs=epoch_count, batch_size=batch)
                # points_SGD_with_momentum, nMomentum = SGD_with_momentum(X, Y, epochs=epoch_count, batch_size=batch)
                # points_Nesterov, nNesterov = Nesterov(X, Y, epochs=epoch_count, batch_size=batch)
                # points_AdaGrad, nAdaGrad = AdaGrad(X, Y, epochs=epoch_count, batch_size=batch)
                # points_RMS_prop, nRMS = RMS_prop(X, Y, epochs=epoch_count, batch_size=batch)
                # points_Adam, nAdam = Adam(X, Y, epochs=epoch_count, batch_size=batch)
                # draw_line(points_SGD, get_file("SGD", funcStr, epoch_count, batch), X, Y, nSGD)
                # draw_line(points_SGD_with_momentum, get_file("Momentum", funcStr, epoch_count, batch), X, Y, nMomentum)
                # draw_line(points_Nesterov, get_file("Nesterov", funcStr, epoch_count, batch), X, Y, nNesterov)
                # draw_line(points_AdaGrad, get_file("AdaGrad", funcStr, epoch_count, batch), X, Y, nAdaGrad)
                # draw_line(points_RMS_prop, get_file("RMSprop", funcStr, epoch_count, batch), X, Y, nRMS)
                # draw_line(points_Adam, get_file("Adam", funcStr, epoch_count, batch), X, Y, nAdam)
                for mode in modes:
                    if not os.path.exists("tests/" + mode + "PyTorch"):
                        os.makedirs("tests/" + mode + "PyTorch")
                    print(funcStr)
                    print("ЕБАТЬ ДА ЭТО ЖЕ ЧЕРТОВ", mode)
                    points, num = SGD_PT(array_to_torch(X),
                                    array_to_torch(Y), modes[mode], mode=mode, epochs=epoch_count, batch_size=batch)
                    print(num)
                    # print(points[:num + 1])
                    draw_line(points, get_file(mode + "PyTorch", funcStr, epoch_count, batch), X, Y, num)
                    print('-----------')
