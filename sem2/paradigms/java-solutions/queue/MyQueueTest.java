package queue;

import java.util.function.Predicate;

// :NOTE: * лучше с помощью assert сравнивать полученные результаты с эталонными (на будущее)
// :NOTE: не тестируются методы lastIndexOf и indexOf
// На самом деле тестируются
public class MyQueueTest {
    static Predicate<Object> equals25 = i -> (Integer) i == 25;

    public static void fillModule() {
        for (int i = -9; i < 10; i++) {
            ArrayQueueModule.enqueue(i * i);
        }
    }

    public static void dumpModule() {
        while (!ArrayQueueModule.isEmpty()) {
            System.out.println(
                    ArrayQueueModule.size() + " " + ArrayQueueModule.element() + " " + ArrayQueueModule.indexOf(25) +
                            " " + ArrayQueueModule.lastIndexOf(25) + " " +
                            ArrayQueueModule.dequeue()
            );
        }
        System.out.println("End");
    }

    public static void fillADT(ArrayQueueADT queue) {
        for (int i = -9; i < 10; i++) {
            ArrayQueueADT.enqueue(queue, i * i);
        }
    }

    public static void dumpADT(ArrayQueueADT queue) {
        while (!ArrayQueueADT.isEmpty(queue)) {
            System.out.println(
                    ArrayQueueADT.size(queue) + " " + ArrayQueueADT.element(queue) + " " + ArrayQueueADT.indexOf(queue, 25) +
                            " " + ArrayQueueADT.lastIndexOf(queue, 25) + " " +
                            ArrayQueueADT.dequeue(queue)
            );
        }
        System.out.println("End");
    }

    public static void fillArrayQueue(Queue queue) {
        for (int i = -9; i < 10; i++) {
            queue.enqueue(i * i);
        }
    }

    public static void dumpArrayQueue(Queue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " + queue.element() + " " + queue.indexIf(equals25) +
                    " " + queue.lastIndexIf(equals25) + " " +
                    queue.dequeue());
        }
    }

    public static void main(String[] args) {
        fillModule();
        dumpModule();
        System.out.println("--------");
        fillModule();
        ArrayQueueModule.clear();
        dumpModule();
        System.out.println("--------");

        ArrayQueueADT queue = new ArrayQueueADT();
        fillADT(queue);
        dumpADT(queue);
        System.out.println("--------");
        fillADT(queue);
        ArrayQueueADT.clear(queue);
        dumpADT(queue);
        System.out.println("--------");

        Queue queue1 = new ArrayQueue();
        fillArrayQueue(queue1);
        dumpArrayQueue(queue1);
        System.out.println("--------");
        fillArrayQueue(queue1);
        ArrayQueueADT.clear(queue);
        dumpArrayQueue(queue1);
        System.out.println("--------");

        Queue queue2 = new LinkedQueue();
        fillArrayQueue(queue2);
        dumpArrayQueue(queue2);
        System.out.println("--------");
        fillArrayQueue(queue2);
        ArrayQueueADT.clear(queue);
        dumpArrayQueue(queue2);
        System.out.println("--------");
    }
}
