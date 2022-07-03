package expression.generic.operations;

public interface Operations<T> extends BinaryOperations<T>, UnaryOperations<T> {

    T parseConst(String constant);
}