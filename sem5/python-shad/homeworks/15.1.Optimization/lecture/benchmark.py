import random

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
        return ((0, 0) if not self else
                (len(self), len(self[0])))


def matrix_product(X, Y):
    n_xrows, n_xcols = X.shape
    n_yrows, n_ycols = Y.shape
    
    assert n_xcols == n_yrows, "Incompatible matrix dimensions"
    
    Z = Matrix.zeros((n_xrows, n_ycols))
    
    for i in range(n_xrows):
        for j in range(n_xcols):
            for k in range(n_ycols):
                Z[i][k] += X[i][j] * Y[j][k]
    return Z


def main():
    shape = (100, 100)

    X = Matrix.random(shape)
    Y = Matrix.random(shape)

    for i in range(100):
        matrix_product(X, Y)


if __name__ == '__main__':
    main()