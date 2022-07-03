package queue;

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
Post: exist i: a[i] == element && R = min(i) - 1 : a[i] == element || don't exist i: a[i] == element && R = -1
indexOf(element) - возвращает индекс первого вхождения элемента в очередь, если отсутствует, то -1

Pred: True
Post: exist i: a[i] == element && R = max(i) - 1 : a[i] == element || don't exist i: a[i] == element && R = -1
lastIndexOf(element) - возвращает индекс последнего вхождения элемента в очередь, если отсутствует, то -1
*/
public class ArrayQueue extends AbstractQueue {
    private int size = 2;
    private int head = 0;
    private int tail = 0;
    private Object[] elements = new Object[size];

    // :NOTE: почему 16? достаточно 1 или 2
    // Изменил на 2. Хотя не очень понятно, почему бы нам не взять и 16 за дефолтный размер очереди.
    // :NOTE: вынести общий код поиска позиции из indexOf и lastIndexOf
    // Done
    // :NOTE: Та же проблема с дублированием кода в двух методах (ДЗ-4 зачтено)
    // Done
    // :NOTE: Всё ещё не до конца вынесен общий код из indexOf и lastIndexOf
    // Done
    // :NOTE: нет тестов
    // Done
    // :NOTE: нет контрактов
    // Done

    @Override
    public Object elementImpl() {
        return elements[head];
    }

    @Override
    public void clearImpl() {
        head = 0;
        tail = 0;
        size = 2;
        elements = new Object[size];
    }

    @Override
    public void enqueueImpl(Object element) {
        ensureCapacity();
        elements[tail] = element;
        tail = (tail + 1) % size;
    }

    private void ensureCapacity() {
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

    @Override
    public Object dequeueImpl() {
        Object x = elements[head];
        head = (head + 1) % size;
        return x;
    }

    @Override
    protected Object getPrevImpl(int index) {
        return elements[(head + index + 1) % size];
    }

    @Override
    protected void indexIfImpl() {
    }

    @Override
    protected Object getNextImpl(int index) {
        return elements[(head + index - 1 + size) % size];
    }

    @Override
    protected void lastIndexIfImpl() {
    }

    @Override
    protected Object getTailImpl() {
        return elements[(tail + size - 1) % size];
    }
}
