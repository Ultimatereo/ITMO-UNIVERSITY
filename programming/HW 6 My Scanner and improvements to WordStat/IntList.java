import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class IntList implements Iterable<Integer> {

    private int array[];
    private int size;

    private void init() {
        array = new int[8];
        size = 0;
    }

    public IntList() {
        init();
    }

    public IntList(int a) {
        init();
        add(a);
    }


    public String toString() {
        return Arrays.toString(array);
    }
    
    public void add(int value) {
        addToList(value);
    }

    private void addToList(int value) {
        if (size == array.length) {
            array = Arrays.copyOf(array, 2*array.length);
        }
        array[size++] = value;
    }

    public void set(int i, int value) {
        setToList(i, value);
    }

    private void setToList(int i, int value) {
        array[i] = value;
    }

    public int size() {
        return this.size;
    } 

    public int get(int ind) {
        return array[ind];
    }

    public int back() {
        if (size == 0) {
            throw new NoSuchElementException();
        }
        return array[size - 1];
    }

    @Override
    public Iterator<Integer> iterator() {
        return new MyIterator();
    }

    private class MyIterator implements Iterator<Integer> {

        private int index = 0;

        @Override
        public boolean hasNext() {
            return index < size();
        }

        @Override
        public Integer next() {
            return get(index++);
        }
    }
}
