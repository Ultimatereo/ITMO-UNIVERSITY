package queue;

public class LinkedQueue extends AbstractQueue {
    private Node head = null;
    private Node tail = null;
    private Node current = null;

    @Override
    public Object elementImpl() {
        return head.value;
    }

    @Override
    public void clearImpl() {
        head = null;
        tail = null;
    }

    @Override
    public void enqueueImpl(Object element) {
        Node temp = new Node(element, tail, null);
        if (numberOfElements == 0) {
            tail = temp;
            head = temp;
            return;
        }
        tail.setPrev(temp);
        tail = temp;
    }

    @Override
    public Object dequeueImpl() {
        Object x = head.value;
        head = head.prev;
        if (head != null) {
            head.setNext(null);
        }
        return x;
    }

    @Override
    protected Object getPrevImpl(int index) {
        current = current.prev;
        return current.value;
    }

    @Override
    protected void indexIfImpl() {
        current = head;
    }

    @Override
    protected Object getNextImpl(int index) {
        current = current.next;
        return current.value;
    }

    @Override
    protected void lastIndexIfImpl() {
        current = tail;
    }

    @Override
    protected Object getTailImpl() {
        return tail.value;
    }

    // :NOTE: избавиться от дублирования кода в методах lastIndexIf и indexIf
    // Done
    private static class Node {
        private final Object value;
        private Node next;
        private Node prev;

        public Node(Object value, Node next, Node prev) {
            this.value = value;
            this.next = next;
            this.prev = prev;
        }

        public void setNext(Node next) {
            this.next = next;
        }

        public void setPrev(Node prev) {
            this.prev = prev;
        }
    }
}
