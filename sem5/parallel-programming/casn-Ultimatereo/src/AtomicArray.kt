import kotlinx.atomicfu.atomicArrayOfNulls
import utils.Ref
import utils.descriptors.cas2Impl

class AtomicArray<E>(size: Int, initialValue: E) {
    private val a = atomicArrayOfNulls<Ref<Any?>>(size)

    init {
        for (i in 0 until size) a[i].value = Ref(initialValue)
    }

    fun get(index: Int) =
        a[index].value!!.value

    fun set(index: Int, value: E) {
        a[index].value!!.value = value
    }

    fun cas(index: Int, expected: E, update: E) =
        a[index].value!!.cas(expected, update)

    fun cas2(
        index1: Int, expected1: E, update1: E,
        index2: Int, expected2: E, update2: E
    ): Boolean {
        if (index2 < index1) {
            return cas2Impl(
                a[index2].value!!, expected2, update2,
                a[index1].value!!, expected1, update1
            )
        }
        return cas2Impl(
            a[index1].value!!, expected1, update1,
            a[index2].value!!, expected2, update2
        )
    }
}
