package expression.generic.operations;

public interface UnaryOperations<T> {
    T negate(T a);

    T l0(T a);

    T t0(T a);

    T abs(T a);

    T count(T a);
}
