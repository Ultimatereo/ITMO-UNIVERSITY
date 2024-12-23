data object SequentialSorter : Sorter() {
    override fun sort(values: IntArray) {
        sort(values, 0, values.size - 1)
    }

    fun sort(values: IntArray, low: Int, high: Int) {
        if (low < high) {
            val pivotIndex = partition(values, low, high)
            sort(values, low, pivotIndex - 1)
            sort(values, pivotIndex + 1, high)
        }
    }
}