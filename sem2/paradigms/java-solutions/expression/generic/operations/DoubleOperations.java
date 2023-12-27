package expression.generic.operations;

public class DoubleOperations implements Operations<Double> {

    @Override
    public Double add(Double a, Double b) {
        return a + b;
    }

    @Override
    public Double divide(Double a, Double b) {
        return a / b;
    }

    @Override
    public Double subtract(Double a, Double b) {
        return a - b;
    }

    @Override
    public Double multiply(Double a, Double b) {
        return a * b;
    }

    @Override
    public Double log(Double a, Double b) {
        return Math.log(b) / Math.log(a);
    }

    @Override
    public Double pow(Double a, Double b) {
        return Math.pow(a, b);
    }

    @Override
    public Double max(Double a, Double b) {
        return Math.max(a, b);
    }

    @Override
    public Double min(Double a, Double b) {
        return Math.min(a, b);
    }

    @Override
    public Double shiftLeft(Double a, Double b) {
        return null;
    }

    @Override
    public Double shiftRight(Double a, Double b) {
        return null;
    }

    @Override
    public Double shiftRightRight(Double a, Double b) {
        return null;
    }

    @Override
    public Double negate(Double a) {
        return -a;
    }

    @Override
    public Double l0(Double a) {
        return null;
    }

    @Override
    public Double t0(Double a) {
        return null;
    }

    @Override
    public Double abs(Double a) {
        return Math.abs(a);
    }

    @Override
    public Double parseConst(String constant) {
        return Double.parseDouble(constant);
    }

    @Override
    public Double count(Double a) {
        return (double) Long.bitCount(Double.doubleToLongBits(a));
    }
}
