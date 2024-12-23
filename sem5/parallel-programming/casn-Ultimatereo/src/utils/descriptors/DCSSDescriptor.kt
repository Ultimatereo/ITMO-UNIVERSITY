package utils.descriptors

import utils.Outcome
import utils.Ref
import java.util.concurrent.atomic.AtomicReference


class DCSSDescriptor(
    private val a: Ref<Any?>,
    private val expectedA: Any?,
    private val updateA: Any?,
    private val b: Ref<Any?>,
    private val expectedB: Any?,
    override val state: AtomicReference<Outcome> = AtomicReference(Outcome.UNDECIDED)
) : Descriptor(state) {

    override fun complete() {
        completeButNotSet()

        if (isSuccess) {
            a.v.compareAndSet(this, updateA)
        } else {
            a.v.compareAndSet(this, expectedA)
        }
    }

    private fun completeButNotSet() {
        val bValue = if (expectedB is Descriptor) b.v.get() else b.value

        // Taken descriptor of one and switched to another thread
        if (bValue == expectedB) {
            state.compareAndSet(Outcome.UNDECIDED, Outcome.SUCCESS)
        } else {
            state.compareAndSet(Outcome.UNDECIDED, Outcome.FAIL)
        }
    }

    override fun start() {
        // TODO: How to be able to create it through different threads?
        if (state.get() == Outcome.UNDECIDED) {
            if (a.v.get() == this) {
                complete()
                return
            }
            if (!a.cas(expectedA, this)) {
                state.compareAndSet(Outcome.UNDECIDED, Outcome.FAIL)
                return
            }
            complete()
        }
    }
}