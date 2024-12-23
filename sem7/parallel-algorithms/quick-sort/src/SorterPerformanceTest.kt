import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertTrue
import kotlin.random.Random
import kotlin.system.measureTimeMillis

class SorterPerformanceTest {

    @Test
    fun compareSequentialAndParallelSort() {
        val arraySize = 100_000_000
        val testRuns = 5
        val randomArray = IntArray(arraySize) { Random.nextInt() }

        println("Starting sequential sorting...")

        val sequentialTime = measureAverageTime(testRuns) {
            val arrayCopy = randomArray.copyOf()
            SequentialSorter.sort(arrayCopy)
        }

        println("Average sequential sort time: $sequentialTime ms")

        println("Starting parallel sorting...")

        val parallelTime = measureAverageTime(testRuns) {
            val arrayCopy = randomArray.copyOf()
            ParallelSorter.sort(arrayCopy)
        }

        println("Average parallel sort time: $parallelTime ms")
        println("Parallel sort is ${sequentialTime / parallelTime} times faster.")
        // Assert that parallel sort is at least 3 times faster
        assertTrue(
                sequentialTime / parallelTime >= 3.0,
                "Parallel sort should be at least 3 times faster."
        )
    }

    private fun measureAverageTime(testRuns: Int, block: () -> Unit): Double {
        println()
        var totalTime = 0L
        repeat(testRuns) {
            val time = measureTimeMillis { block() }
            totalTime += time
            println("Time for ${it + 1} iteration: $time ms")
        }
        println()
        return totalTime.toDouble() / testRuns
    }
}
