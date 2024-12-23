import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author TODO: Sultanov Mirzomansurkhon
 */
class FAABasedQueue<E> : Queue<E> {
    private val enqIdx = AtomicLong(0)
    private val deqIdx = AtomicLong(0)
    private val segmentsQueue = SegmentQueue()

    override fun enqueue(element: E) {
        while (true) {
            val curTail = segmentsQueue.tail.get()
            val i = enqIdx.getAndIncrement()
            val s: Segment = segmentsQueue.findSegment(curTail, i / SEGMENT_SIZE)
            segmentsQueue.moveTailForward(s)
            if (s.cells.compareAndSet((i % SEGMENT_SIZE).toInt(), null, element)) return
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (deqIdx.get() >= enqIdx.get()) return null
            val curHead = segmentsQueue.head.get()
            val i = deqIdx.getAndIncrement()
            val s: Segment = segmentsQueue.findSegment(curHead, i / SEGMENT_SIZE)
            segmentsQueue.moveHeadForward(s)
            if (s.cells.compareAndSet((i % SEGMENT_SIZE).toInt(), null, POISONED)) continue
            val v = s.cells.get((i % SEGMENT_SIZE).toInt()) as E
            if (s.cells.compareAndSet((i % SEGMENT_SIZE).toInt(), v, null)) return v
        }
    }

    private class SegmentQueue {
        val head: AtomicReference<Segment>
        val tail: AtomicReference<Segment>

        init {
            val dummy = Segment(0)
            head = AtomicReference(dummy)
            tail = AtomicReference(dummy)
        }

        fun findSegment(start: Segment, id: Long): Segment {
            var cur = start
            while (cur.id != id) {
                val temp = cur.next.get()
                if (temp == null) {
                    cur.next.compareAndSet(null, Segment(cur.id + 1))
                }
                cur = cur.next.get()!!
            }
            return cur
        }

        fun moveTailForward(s: Segment) {
            val curTail = tail.get()
            if (s.id > curTail.id) {
                tail.compareAndSet(curTail, s)
            }
        }

        fun moveHeadForward(s: Segment) {
            val curHead = head.get()
            if (s.id > curHead.id) {
                head.compareAndSet(curHead, s)
            }
        }

    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2
private val POISONED = Any()