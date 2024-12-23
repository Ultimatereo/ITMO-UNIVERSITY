/**
 * @author Sultanov Mirzomansurkhon
 */

import java.util.concurrent.atomic.AtomicReference


class Solution(val env: Environment) : Lock<Solution.Node> {
    // todo: необходимые поля (val, используем AtomicReference)
    val tail = AtomicReference<Node?>()

    override fun lock(): Node {
        val my = Node() // сделали узел
        my.next.set(null)
        my.locked.set(true)
        val prev = tail.getAndSet(my)
        if (prev != null) {
            prev.next.set(my)
            while (my.locked.get()) env.park()
        }
        return my // вернули узел
    }

    override fun unlock(node: Node) {
        if (node.next.get() == null) {
            if (tail.compareAndSet(node, null)) return
            while (node.next.get() == null) env.park()
        }
        node.next.get()!!.locked.set(false)
        env.unpark(node.next.get().thread)
    }

    class Node {
        val thread: Thread = Thread.currentThread() // запоминаем поток, которые создал узел

        // todo: необходимые поля (val, используем AtomicReference)
        val locked = AtomicReference<Boolean>()
        val next = AtomicReference<Node>()
    }
}