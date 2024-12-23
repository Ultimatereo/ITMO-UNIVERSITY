import random
import timeit

class Matrix(list):
    @classmethod
    def zeros(cls, shape):
        return cls([[0] * shape[1] for i in range(shape[0])])

    @classmethod
    def random(cls, shape):
        M, (n_rows, n_cols) = cls(), shape
        for i in range(n_rows):
            M.append([random.randint(-255, 255)
                      for j in range(n_cols)])
        return M

    def transpose(self):
        n_rows, n_cols = self.shape
        return self.__class__(zip(*self))

    @property
    def shape(self):
        return (len(self), len(self[0]))


def matrix_product(X, Y):
    n_xrows, n_xcols = X.shape
    n_yrows, n_ycols = Y.shape
    Z = Matrix.zeros((n_xrows, n_ycols))
    Yt = Y.transpose()  # better cell extraction
    for i, (Xi, Zi) in enumerate(zip(X, Z)):
        getX = Xi.__getitem__
        for k, Ytk in enumerate(Yt):
            Zi[k] = sum(getX(j) * Ytk[j] for j in range(n_xcols))
    return Z


if __name__ == '__main__':
    shape = (100, 100)
    X = Matrix.random(shape)
    Y = Matrix.random(shape)

    start = timeit.default_timer()
    for i in range(100):
        matrix_product(X, Y)
    end = timeit.default_timer()
    print('Completed in {} seconds'.format(end - start))
