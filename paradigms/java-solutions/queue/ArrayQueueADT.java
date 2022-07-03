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
public class ArrayQueueADT {
    private int size = 2;
    private int numberOfElements = 0;
    private int head = 0;
    private int tail = 0;
    private Object[] elements = new Object[size];

    public static int indexOf(ArrayQueueADT queue, Object element) {
        Objects.requireNonNull(element);
        for (int i = 0; i < size(queue); i++) {
            if (element.equals(queue.elements[(queue.head + i) % queue.size])) {
                return findIndexInArray(queue, (queue.head + i) % queue.size);
            }
        }
        return -1;
    }

    private static int findIndexInArray(ArrayQueueADT queue, int x) {
        if (x < queue.head) {
            return queue.size - queue.head + x;
        }
        return x - queue.head;
    }

    public static int lastIndexOf(ArrayQueueADT queue, Object element) {
        Objects.requireNonNull(element);
        for (int i = 1; i <= size(queue); i++) {
            if (element.equals(queue.elements[(queue.size + queue.tail - i) % queue.size])) {
                return findIndexInArray(queue, (queue.tail - i) % queue.size);
            }
        }
        return -1;
    }

    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.numberOfElements == 0;
    }

    public static Object element(ArrayQueueADT queue) {
        return queue.elements[queue.head];
    }

    public static int size(ArrayQueueADT queue) {
        return queue.numberOfElements;
    }

    public static void clear(ArrayQueueADT queue) {
        //System.err.println("clear");
        queue.head = 0;
        queue.tail = 0;
        queue.size = 2;
        queue.elements = new Object[queue.size];
        queue.numberOfElements = 0;
    }

    public static void enqueue(ArrayQueueADT queue, Object element) {
        Objects.requireNonNull(element);
        //System.err.println("enqueue " + element + " " + head + " " + tail);
        ensureCapacity(queue);
        queue.elements[queue.tail] = element;
        queue.tail = (queue.tail + 1) % queue.size;
        queue.numberOfElements++;
    }

    private static void ensureCapacity(ArrayQueueADT queue) {
        if (queue.numberOfElements == queue.size) {
            Object[] newElements = new Object[queue.size * 2];
            for (int i = 0; i < queue.size; i++) {
                newElements[i] = queue.elements[(queue.head + i) % queue.size];
            }
            queue.head = 0;
            queue.tail = queue.size;
            queue.size *= 2;
            queue.elements = newElements;
        }
    }

    public static Object dequeue(ArrayQueueADT queue) {
        //System.err.println("dequeue " + head + " " + tail);
        if (isEmpty(queue)) {
            return null;
        }
        Object x = queue.elements[queue.head];
        queue.head = (queue.head + 1) % queue.size;
        queue.numberOfElements--;
        return x;
    }

}
