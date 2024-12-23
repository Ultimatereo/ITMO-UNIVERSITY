package utils.descriptors

import utils.Outcome
import java.util.concurrent.atomic.AtomicReference

abstract class Descriptor(
    open val state: AtomicReference<Outcome> = AtomicReference(Outcome.UNDECIDED)
) {
    abstract fun complete()
    abstract fun start()
    open fun result(): Boolean {
        start()
        return isSuccess
    }

    val isFail: Boolean get() = state.get() == Outcome.FAIL
    val isSuccess get() = state.get() == Outcome.SUCCESS
}