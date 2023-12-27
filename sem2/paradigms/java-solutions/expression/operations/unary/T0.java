package expression.operations.unary;

import expression.UltimateExpression;

public class T0 extends AbstractUnaryOperation {

    public T0(UltimateExpression expression) {
        super("t0", expression);
    }

    @Override
    public int evaluate(int x, int y, int z) {
        int res = expression.evaluate(x, y, z);
        String bin = Integer.toBinaryString(res);
        int ans = 0;
        while (ans < bin.length()) {
            if (bin.charAt(bin.length() - 1 - ans) == '0') {
                ans++;
            } else {
                break;
            }
        }
        return res == 0 ? 32 : ans;
    }
}
