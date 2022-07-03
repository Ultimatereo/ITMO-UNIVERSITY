package md2html;

public class Triple implements Comparable<Triple> {
    private final int value1;
    private final int value2;
    private final int value3;

    public Triple(int value1, int value2, int value3) {
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    }

    public int getValue1() {
        return value1;
    }

    public int getValue2() {
        return value2;
    }

    public int getValue3() {
        return value3;
    }

    @Override
    public int compareTo(Triple t) {
        return this.value1 - t.value1;
    }

    @Override
    public String toString() {
        return "Triple{" +
                "value1=" + value1 +
                ", value2=" + value2 +
                ", value3=" + value3 +
                '}';
    }
}
