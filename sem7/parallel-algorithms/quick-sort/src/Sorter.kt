sealed class Sorter {
    abstract fun sort(values: IntArray)

    protected fun partition(values: IntArray, low: Int, high: Int): Int {
        val pivot = values[high]
        var i = low - 1
        for (j in low..<high) {
            if (values[j] <= pivot) {
                i++
                values[i] = values[j].also { values[j] = values[i] }
            }
        }
        values[i + 1] = values[high].also { values[high] = values[i + 1] }
        return i + 1
    }
}