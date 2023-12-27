package expression;

public class Main {
    public static void main(String[] args) {
        int answer = new Add(
                new Subtract(
                        new Multiply(new Variable("x"), new Variable("x")),
                        new Multiply(new Const(2), new Variable("x"))),
                new Const(1)).evaluate(Integer.parseInt(args[0]));
        System.out.println(answer);
        System.out.println(new Subtract(
                new Const(1),
                new Subtract(
                        new Const(2),
                        new Const(3)
                )
        ).hashCode());
        System.out.println(new Add(
                new Const(1),
                new Divide(
                        new Const(2),
                        new Const(3)
                )
        ).hashCode());
    }
}
