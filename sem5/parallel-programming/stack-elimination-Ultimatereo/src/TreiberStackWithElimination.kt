import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author Sultanov Mirzomansurkhon
 */
open class TreiberStackWithElimination<E> : Stack<E> {
    private val stack = TreiberStack<E>()
    private val eliminationArray = AtomicReferenceArray<Any?>(ELIMINATION_ARRAY_SIZE)

    override fun push(element: E) {
        if (tryPushElimination(element)) return
        stack.push(element)
    }

    protected open fun tryPushElimination(element: E): Boolean {
        val randomCellIndex = randomCellIndex()
        if (eliminationArray.compareAndSet(randomCellIndex, CELL_STATE_EMPTY, element)) {
            while (true) {
                val el = eliminationArray.get(randomCellIndex)
                if (el == CELL_STATE_RETRIEVED) {
                    if (eliminationArray.compareAndSet(randomCellIndex, CELL_STATE_RETRIEVED, CELL_STATE_EMPTY)) {
                        return true
                    }
                } else {
                    if (eliminationArray.compareAndSet(randomCellIndex, el, CELL_STATE_EMPTY)) {
                        break
                    }
                }
            }
        }
        return false

    }

    override fun pop(): E? = tryPopElimination() ?: stack.pop()

    private fun tryPopElimination(): E? {
        val randomCellIndex = randomCellIndex()
        val el = eliminationArray.get(randomCellIndex)
        if (el != CELL_STATE_EMPTY && el != CELL_STATE_RETRIEVED) {
            if (eliminationArray.compareAndSet(randomCellIndex, el, CELL_STATE_RETRIEVED)) {
                return el as E?
            }
        }
        return null
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(eliminationArray.length())

    companion object {
        private const val ELIMINATION_ARRAY_SIZE = 2 // Do not change!
        private const val ELIMINATION_WAIT_CYCLES = 1 // Do not change!

        // Initially, all cells are in EMPTY state.
        private val CELL_STATE_EMPTY = null

        // `tryPopElimination()` moves the cell state
        // to `RETRIEVED` if the cell contains element.
        private val CELL_STATE_RETRIEVED = Any()
    }
}
