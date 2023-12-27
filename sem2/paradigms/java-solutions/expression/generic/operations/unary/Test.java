package expression.generic.operations.unary;

public class Test {
    public static void main(String[] args) {
        short a = 1;
        short count = 0;
        for (int i = 0; i < 16; i++) {
            System.err.println((1 << i) + " " + (a ^ (1 << i)));
            if ((a & (1 << i)) != 0) {
                count++;
            }
        }
        System.err.println(" " + count);
    }
}
