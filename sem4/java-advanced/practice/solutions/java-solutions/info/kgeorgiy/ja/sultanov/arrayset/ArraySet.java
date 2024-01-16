package info.kgeorgiy.ja.sultanov.arrayset;

import java.util.*;

public class ArraySet<E extends Comparable<E>> extends AbstractSet<E> implements SortedSet<E> {
    private final List<E> array;
    private final Comparator<? super E> comparator;

    public ArraySet(final Comparator<? super E> comparator) {
        this(List.of(), comparator);
    }

    public ArraySet() {
        this(List.of(), null);
    }

    public ArraySet(final Collection<? extends E> c) {
        this(c, null);
    }

    public ArraySet(final Collection<? extends E> c, final Comparator<? super E> comparator) {
        TreeSet<E> treeSet = new TreeSet<>(comparator);
        treeSet.addAll(c);
        array = new ArrayList<>();
        array.addAll(treeSet);
        this.comparator = comparator;
    }

    private ArraySet(List<E> array, Comparator<? super E> comparator) {
        this.array = array;
        this.comparator = comparator;
    }

    @Override
    public Iterator<E> iterator() {
        return array.iterator();
    }


    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }


    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        if (compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }
        int from = getIndex(fromElement);
        int to = getIndex(toElement);
        return new ArraySet<>(array.subList(from, to), this.comparator);
    }

    private int getIndex(E fromElement) {
        int from = Collections.binarySearch(array, fromElement, comparator);
        if (from < 0) {
            from = -from - 1;
        }
        return from;
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return new ArraySet<>(array.subList(0, getIndex(toElement)), this.comparator);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return new ArraySet<>(array.subList(getIndex(fromElement), array.size()), this.comparator);
    }

    private int compare(E e1, E e2) {
        if (comparator == null) {
            return e1.compareTo(e2);
        }
        return comparator.compare(e1, e2);
    }

    @Override
    public E first() {
        if (size() == 0) {
            throw new NoSuchElementException();
        }
        return array.get(0);
    }

    @Override
    public E last() {
        if (size() == 0) {
            throw new NoSuchElementException();
        }
        return array.get(array.size() - 1);
    }

    @Override
    public int size() {
        return array.size();
    }

    @Override
    public boolean isEmpty() {
        return size() == 0;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(Object o) {
        return Collections.binarySearch(array, (E) Objects.requireNonNull(o), comparator) >= 0;
    }

    @Override
    public Object[] toArray() {
        return array.toArray();
    }

    @Override
    public <T1> T1[] toArray(T1[] a) {
        return array.toArray(a);
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean add(E e) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean remove(Object o) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }
}
