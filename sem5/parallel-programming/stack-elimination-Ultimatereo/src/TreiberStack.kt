import java.util.concurrent.atomic.AtomicReference

/**
 * @author Sultanov Mirzomansurkhon
 */
class TreiberStack<E> : Stack<E> {
    // Initially, the stack is empty.
    private val top = AtomicReference<Node<E>?>(null)

    override fun push(element: E) {
        while (true) {
            val curTop = top.get()
            val newTop = Node(element, curTop)
            if (top.compareAndSet(curTop, newTop)) {
                return
            }
        }
    }

    override fun pop(): E? {
        while (true) {
            val curTop = top.get() ?: return null
            if (top.compareAndSet(curTop, curTop.next)) {
                return curTop.element
            }
        }
    }

    private class Node<E>(
        val element: E,
        val next: Node<E>?
    )
}
