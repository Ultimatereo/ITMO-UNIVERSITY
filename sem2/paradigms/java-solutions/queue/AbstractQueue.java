package queue;

import java.util.Objects;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int numberOfElements;

    @Override
    public int size() {
        return numberOfElements;
    }

    @Override
    public boolean isEmpty() {
        return numberOfElements == 0;
    }

    @Override
    public void clear() {
        numberOfElements = 0;
        clearImpl();
    }

    protected abstract void clearImpl();

    @Override
    public Object element() {
        if (isEmpty()) {
            throw new AssertionError("You can't ask for the first element in empty queue");
        }
        return elementImpl();
    }

    protected abstract Object elementImpl();

    @Override
    public void enqueue(Object element) {
        Objects.requireNonNull(element);
        enqueueImpl(element);
        numberOfElements++;
    }

    protected abstract void enqueueImpl(Object element);

    @Override
    public Object dequeue() {
        if (isEmpty()) {
            throw new AssertionError("You can't use dequeue to empty queue!");
        }
        Object x = dequeueImpl();
        numberOfElements--;
        return x;
    }

    protected abstract Object dequeueImpl();

    // :NOTE: lastIndexIf и indexIf всё ещё имеют очень похожий код.
    //        но решение с EqualsPredicate интересное
    @Override
    public int indexIf(Predicate<Object> predicate) {
        if (isEmpty()) {
            return -1;
        }
        Object element = element();
        indexIfImpl();
        for (int i = 0; i < size(); i++) {
            if (predicate.test(element)) {
                return i;
            }
            element = getPrev(i);
        }
        return -1;
    }

    protected Object getPrev(int index) {
        if (index < 0 || index >= size() - 1) {
            return null;
        }
        return getPrevImpl(index);
    }

    protected abstract Object getPrevImpl(int index);

    protected abstract void indexIfImpl();

    @Override
    public int lastIndexIf(Predicate<Object> predicate) {
        if (isEmpty()) {
            return -1;
        }
        Object element = getTail();
        lastIndexIfImpl();
        for (int i = size() - 1; i >= 0; i--) {
            if (predicate.test(element)) {
                return i;
            }
            element = getNext(i);
        }
        return -1;
    }

    protected Object getNext(int index) {
        if (index <= 0 || index > size() - 1) {
            return null;
        }
        return getNextImpl(index);
    }

    protected abstract Object getNextImpl(int index);

    protected abstract void lastIndexIfImpl();
    @Override
    public Object getTail() {
        if (isEmpty()) {
            throw new AssertionError("You can't ask for the last element in empty queue");
        }
        return getTailImpl();
    }

    protected abstract Object getTailImpl();

    @Override
    public int indexOf(Object element) {
        return indexIf(new EqualsPredicate(element));
    }

    @Override
    public int lastIndexOf(Object element) {
        return lastIndexIf(new EqualsPredicate(element));
    }

    private static class EqualsPredicate implements Predicate<Object> {
        private final Object element;

        private EqualsPredicate(Object element) {
            this.element = element;
        }

        @Override
        public boolean test(Object o) {
            return o.equals(element);
        }
    }
}
