package utils.descriptors

import utils.Outcome
import utils.Ref

class CAS2Descriptor(
    private val a: Ref<Any?>,
    private val expectedA: Any?,
    private val updateA: Any?,
    private val b: Ref<Any?>,
    private val expectedB: Any?,
    private val updateB: Any?
) : Descriptor() {
    private val descriptor = DCSSDescriptor(b, expectedB, updateB, a, this, state)
    override fun complete() {
        if (!descriptor.result()) {
            state.compareAndSet(Outcome.UNDECIDED, Outcome.FAIL)
            a.cas(this, expectedA)
        }
        if (isSuccess) {
            a.cas(this, updateA)
            b.cas(descriptor, updateB)
        } else {
            a.cas(this, expectedA)
            b.cas(descriptor, updateB)
        }
    }

    override fun start() {
        if (!a.cas(expectedA, this)) {
            state.set(Outcome.FAIL)
            return
        }
        complete()
    }
}

fun cas2Impl(
    a: Ref<Any?>, expectedA: Any?, updateA: Any?,
    b: Ref<Any?>, expectedB: Any?, updateB: Any?
): Boolean {
    return CAS2Descriptor(a, expectedA, updateA, b, expectedB, updateB).result()
}

