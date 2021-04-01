package md2html;

import java.util.Arrays;
class StackAN {

    private int size = 0;
    private int[] array = new int[4];

    void add(int value) {
        if (size == array.length) {
            array = Arrays.copyOf(array, array.length * 2);
        }
        array[size] = value;
        size += 1;
    }

    void pop() {
        size -= 1;
    }

    int getSize() {
        return size;
    }
}
