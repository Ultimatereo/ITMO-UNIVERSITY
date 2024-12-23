package utils

import utils.descriptors.Descriptor
import java.util.concurrent.atomic.AtomicReference


class Ref<E>(initial: E) {
    val v = AtomicReference<Any?>(initial)

    var value: E
        get() {
            while (true) {
                @Suppress("UNCHECKED_CAST")
                when (val cur = v.get()) {
                    is Descriptor -> cur.complete()
                    else -> return cur as E
                }
            }
        }
        set(upd) {
            while (true) {
                when (val cur = v.get()) {
                    is Descriptor -> cur.complete()
                    else -> if (v.compareAndSet(cur, upd)) return
                }
            }
        }

    fun cas(expected: Any?, update: Any?): Boolean {
        // We have to make cas on value and not on descriptor!
        // We cannot rely on expectedValue nor updateValue from descriptor cuz it could change or not change later on
        if (expected is Descriptor) {
            return v.get() == expected && v.compareAndSet(expected, update)
        }
        return value == expected && v.compareAndSet(expected, update)
    }
}
