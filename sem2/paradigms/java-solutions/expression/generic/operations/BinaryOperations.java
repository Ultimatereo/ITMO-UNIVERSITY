package expression.generic.operations;

public interface BinaryOperations<T> {
    T add(T a, T b);

    T divide(T a, T b);

    T subtract(T a, T b);

    T multiply(T a, T b);

    T log(T a, T b);

    T pow(T a, T b);

    T max(T a, T b);

    T min(T a, T b);

    T shiftLeft(T a, T b);

    T shiftRight(T a, T b);

    T shiftRightRight(T a, T b);
}
