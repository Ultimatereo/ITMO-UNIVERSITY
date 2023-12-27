
interface DimentionAware {
    val ndim: Int
    fun dim(i: Int): Int
}

interface SizeAware {
    val size: Int
}