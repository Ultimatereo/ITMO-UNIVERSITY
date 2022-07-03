package expression.generic;

import expression.generic.operations.*;

import java.util.Map;

public class GenericTabulator implements Tabulator {

    private static final Map<String, Operations<?>> modes = Map.of(
            "i", new IntegerOperations(true),
            "u", new IntegerOperations(false),
            "d", new DoubleOperations(),
            "bi", new BigIntegerOperations(),
            "s", new ShortOperations(),
            "l", new LongOperations()
    );


    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        if (modes.containsKey(mode)) {
            return fillMatrix(expression, x1, x2, y1, y2, z1, z2, modes.get(mode));
        } else {
            throw new AssertionError("Mode " + mode + " is not supported!");
        }
    }

    private <T> Object[][][] fillMatrix(String expression, int x1, int x2, int y1, int y2, int z1, int z2,
                                        Operations<T> opMode) {
        TripleExpression<T> tripleExpression = new TripleExpressionParser<>(expression, opMode).parse();
        Object[][][] result = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
//        System.err.println(tripleExpression);
        for (int i = 0; i <= x2 - x1; i++) {
            for (int j = 0; j <= y2 - y1; j++) {
                for (int k = 0; k <= z2 - z1; k++) {
                    try {
                        result[i][j][k] = tripleExpression.evaluate(opMode.parseConst(Integer.toString(x1 + i))
                                , opMode.parseConst(Integer.toString(y1 + j)), opMode.parseConst(Integer.toString(z1 + k)));
                    } catch (Exception e) {
                        result[i][j][k] = null;
                    }
                }
            }
        }

        return result;

    }
}
