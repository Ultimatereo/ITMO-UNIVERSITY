/**
 * @author : Sultanov Mirzomansurkhon
 */

import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReferenceArray

class FlatCombiningQueue<E> : Queue<E> {
    private val queue = ArrayDeque<E>() // sequential queue
    private val combinerLock = AtomicBoolean(false) // unlocked initially
    private val tasksForCombiner = AtomicReferenceArray<CombinerElement>(TASKS_FOR_COMBINER_SIZE)

    init {
        for (i in 0 until TASKS_FOR_COMBINER_SIZE) {
            tasksForCombiner[i] = CombinerElement.Empty
        }
    }

    private fun randomCellIndex(): Int = ThreadLocalRandom.current().nextInt(tasksForCombiner.length())
    private inline fun tryWithLock(block: () -> Unit): Boolean {
        val v = combinerLock.tryLock()
        if (v) {
            try {
                block()
            } finally {
                combinerLock.unlock()
            }
        }
        return v
    }

    private inline fun Boolean.runIfFalse(block: () -> Unit) {
        if (!this) block()
    }


    private fun combine() {
        for (i in 0 until TASKS_FOR_COMBINER_SIZE) {
            when (val task = tasksForCombiner.get(i)) {
                CombinerElement.Dequeue -> {
                    val t: E? = queue.removeFirstOrNull()
                    tasksForCombiner.set(i, CombinerElement.Result(t))
                }

                is CombinerElement.Enqueue<*> -> {
                    @Suppress("UNCHECKED_CAST") queue.addLast(task.value as E)
                    tasksForCombiner.set(i, CombinerElement.Result(null))
                }

                is CombinerElement.Result<*>, CombinerElement.Empty -> continue
            }
        }
    }

    private fun addTask(task: CombinerElement.CombinerAddElement): CombinerElement.Result<E?> {
        while (true) {
            val i = randomCellIndex()
            if (tasksForCombiner.get(i) == CombinerElement.Empty && tasksForCombiner.compareAndSet(
                    i, CombinerElement.Empty, task
                )
            ) {
                return getResult(i)
            }
        }
    }

    private fun getResult(i: Int): CombinerElement.Result<E?> {
        while (true) {
            val res = tasksForCombiner.get(i)

            if (res is CombinerElement.Result<*>) {
                tasksForCombiner.set(i, CombinerElement.Empty)
                @Suppress("UNCHECKED_CAST") return res as CombinerElement.Result<E?>
            }

            tryWithLock {
                combine()
                val lostRes = tasksForCombiner.get(i) as CombinerElement.Result<*>
                tasksForCombiner.set(i, CombinerElement.Empty)
                @Suppress("UNCHECKED_CAST") return lostRes as CombinerElement.Result<E?>
            }
        }
    }

    override fun enqueue(element: E) {
        tryWithLock {
            combine()
            queue.addLast(element)
        }.runIfFalse {
            addTask(CombinerElement.Enqueue(element))
        }
    }

    override fun dequeue(): E? {
        tryWithLock {
            combine()
            return queue.removeFirstOrNull()
        }.runIfFalse {
            val res = addTask(CombinerElement.Dequeue)
            return res.value
        }

        error("Unreachable code")
    }
}

private fun AtomicBoolean.unlock() {
    this.set(false)
}

private fun AtomicBoolean.tryLock(): Boolean {
    return !this.get() && this.compareAndSet(false, true)
}

private const val TASKS_FOR_COMBINER_SIZE = 3 // Do not change this constant!

private sealed interface CombinerElement {
    sealed interface CombinerAddElement : CombinerElement
    data object Dequeue : CombinerAddElement
    class Enqueue<V>(
        val value: V
    ) : CombinerAddElement

    data object Empty : CombinerElement

    class Result<V>(
        val value: V
    ) : CombinerElement
}