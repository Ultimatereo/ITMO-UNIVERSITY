package queue;

import java.util.function.Predicate;

/*
Model: a[1]..a[n]
Invariant: for i=1..n: a[i] != null

Let immutable(n): for i=1..n: a'[i] == a[i]

Pred: element != null
Post: n' = n + 1 & a'[1] = element && forall i:1..n : a'[i + 1] = a[i]
enqueue(element) – добавить элемент в очередь;

Pred: n > 0
Post: R = a[1]
element – первый элемент в очереди;

Pred: n > 0
Post: R = a[n]
getTail – последний элемент в очереди;

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
Post: exist i: predicate.test(a[i]) == true && R = min(i) - 1 : predicate.test(a[i]) == true || don't exist i: predicate.test(a[i]) == true && R = -1
indexIf(predicate) - возвращает индекс первого вхождения элемента в очередь, если отсутствует, то -1

Pred: True
Post: exist i: predicate.test(a[i]) == true && R = max(i) - 1 : predicate.test(a[i]) == true || don't exist i: predicate.test(a[i]) == true && R = -1
lastIndexIf(predicate) - возвращает индекс последнего вхождения элемента в очередь, если отсутствует, то -1
*/
public interface Queue {
    boolean isEmpty();

    Object element();

    Object getTail();

    int size();

    void clear();

    void enqueue(Object element);

    Object dequeue();

    int indexIf(Predicate<Object> predicate);

    int lastIndexIf(Predicate<Object> predicate);

    int indexOf(Object element);

    int lastIndexOf(Object element);
}
