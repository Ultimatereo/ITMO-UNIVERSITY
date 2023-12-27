import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class RandomBooleanExpressionGenerator {
    private static final String[] VARIABLES = {"A", "B", "C"};
    private static final String[] OPERATORS = {"->", "|", "&", "_|_"};
    private static final Random RANDOM = new Random();

    public static void main(String[] args) {
        String randomExpression = generateRandomExpression();
        System.out.println("Random Boolean Expression: " + randomExpression);
    }


    public static String generateRandomExpression() {
        while (true) {
            int depth = RANDOM.nextInt(5) + 1;
            String expression = generateExpression(depth);
            Solution.ExpressionNode en = new Solution.ExpressionNode(
                    new Solution.ParserExpression(expression).parseExpression());
            boolean flag = true;
            for (int i = 0; i < Math.pow(2, VARIABLES.length); i++) {
                boolean[] values = Solution.getValues(i, VARIABLES.length);
                Map<String, Boolean> valuesMap = new HashMap<>();
                int j = 0;
                for (String variable : VARIABLES) {
                    valuesMap.put(variable, values[j]);
                    j += 1;
                }
                boolean value = en.evaluate(valuesMap);
                if (!value) {
                    flag = false;
                }
            }
            if (flag) return expression;
        }
    }

    public static String generateExpression(int depth) {
        if (depth == 1) {
            return VARIABLES[RANDOM.nextInt(VARIABLES.length)];
        } else {
            String leftExpression = generateExpression(depth - 1);
            String operator = OPERATORS[RANDOM.nextInt(OPERATORS.length)];
            String rightExpression = generateExpression(depth - 1);

            return switch (operator) {
                case "->" -> "(" + leftExpression + "->" + rightExpression + ")";
                case "|" -> "(" + leftExpression + "|" + rightExpression + ")";
                case "&" -> "(" + leftExpression + "&" + rightExpression + ")";
                default -> "_|_";
            };
        }
    }
}
