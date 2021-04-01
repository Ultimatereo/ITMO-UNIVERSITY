package expression;

public class Main {
    public static void main(String[] args) {
        Expression k = new Multiply(
                new Subtract(new Variable("x"), new Const(1)),
                new Subtract(new Variable("x"), new Const(1)));
        System.out.println(k.evaluate(Integer.parseInt("3")));
    }
}
