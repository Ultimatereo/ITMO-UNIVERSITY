import kotlinx.coroutines.*

data object ParallelSorter : Sorter() {
    @OptIn(ExperimentalCoroutinesApi::class)
    private val dispatcher = Dispatchers.IO.limitedParallelism(4)

    override fun sort(values: IntArray) {
        runBlocking { sort(values, 0, values.size - 1) }
    }

    private suspend fun sort(array: IntArray, low: Int, high: Int, threshold: Int = 10_000) {
        if (low < high) {
            val pivotIndex = partition(array, low, high)
            if (high - low < threshold) {
                SequentialSorter.sort(array, low, pivotIndex - 1)
                SequentialSorter.sort(array, pivotIndex + 1, high)
            } else {
                coroutineScope {
                    val leftSort = async(dispatcher) { sort(array, low, pivotIndex - 1, threshold) }
                    sort(array, pivotIndex + 1, high, threshold)
                    leftSort.await()
                }
            }
        }
    }
}