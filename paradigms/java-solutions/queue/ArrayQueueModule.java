package queue;

import java.util.Objects;

/*
Model: a[1]..a[n]
Invariant: for i=1..n: a[i] != null

Let immutable(n): for i=1..n: a'[i] == a[i]

Pred: element != null
Post: n' = n + 1 & a'[1] = element && forall i:1..n : a'[i + 1] = a[i]
enqueue(element) – добавить элемент в очередь;

Pred: True
Post: n == 0 && R = null || n > 0 && R = a[0]
element – первый элемент в очереди;

Pred: n >= 1
Post: n' = n - 1 && immutable(n') && R = a[n]
dequeue – удалить и вернуть первый элемент в очереди;

Pred: True
Post: R == n && n' == n && immutable(n)
size – текущий размер очереди;

Pred: True
Post: R == (n == 0) && n' == n && immutable(n)
isEmpty – является ли очередь пустой;

Pred: True
Post: n' = 0
clear – удалить все элементы из очереди.

Pred: True
Post: R = min(i) - 1 : a[i] == element
indexOf(element) - возвращает индекс первого вхождения элемента в очередь, если отсутствует, то -1

Pred: True
Post: R = max(i) - 1 : a[i] == element
lastIndexOf(element) - возвращает индекс последнего вхождения элемента в очередь, если отсутствует, то -1
*/
public class ArrayQueueModule {
    private static int size = 2;
    private static int numberOfElements = 0;
    private static int head = 0;
    private static int tail = 0;
    private static Object[] elements = new Object[size];

    public static int indexOf(Object element) {
        Objects.requireNonNull(element);
        for (int i = 0; i < size(); i++) {
            if (element.equals(elements[(head + i) % size])) {
                return findIndexInArray((head + i) % size);
            }
        }
        return -1;
    }

    private static int findIndexInArray(int x) {
        if (x < head) {
            return size - head + x;
        }
        return x - head;
    }

    public static int lastIndexOf(Object element) {
        Objects.requireNonNull(element);
        for (int i = 1; i <= size(); i++) {
            if (element.equals(elements[(size + tail - i) % size])) {
                return findIndexInArray((tail - i) % size);
            }
        }
        return -1;
    }

    public static boolean isEmpty() {
        return numberOfElements == 0;
    }

    public static Object element() {
        return elements[head];
    }

    public static int size() {
        return numberOfElements;
    }

    public static void clear() {
        //System.err.println("clear");
        head = 0;
        tail = 0;
        size = 2;
        elements = new Object[size];
        numberOfElements = 0;
    }

    public static void enqueue(Object element) {
        Objects.requireNonNull(element);
        //System.err.println("enqueue " + element + " " + head + " " + tail);
        ensureCapacity();
        elements[tail] = element;
        tail = (tail + 1) % size;
        numberOfElements++;
    }

    private static void ensureCapacity() {
        if (numberOfElements == size) {
            Object[] newElements = new Object[size * 2];
            for (int i = 0; i < size; i++) {
                newElements[i] = elements[(head + i) % size];
            }
            head = 0;
            tail = size;
            size *= 2;
            elements = newElements;
        }
    }

    public static Object dequeue() {
        if (isEmpty()) {
            return null;
        }
        Object x = elements[head];
        head = (head + 1) % size;
        numberOfElements--;
        return x;
    }

}
