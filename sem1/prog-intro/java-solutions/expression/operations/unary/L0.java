package expression.operations.unary;

import expression.UltimateExpression;

public class L0 extends AbstractUnaryOperation {

    public L0(UltimateExpression expression) {
        super("l0", expression);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int res = expression.evaluate(x, y, z);
        String bin = Integer.toBinaryString(res);
        return res == 0 ? 32 : 32 - bin.length();
    }
}
