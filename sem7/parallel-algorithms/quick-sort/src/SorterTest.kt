import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertTrue
import kotlin.random.Random

class SorterTest {
    @Test
    fun stressTestSequentialSorter() {
        run1000StressTest(SequentialSorter)
    }

    @Test
    fun stressTestParallelSorter() {
        run1000StressTest(ParallelSorter)
    }

    private fun run1000StressTest(sorter: Sorter) {
        val numberOfTests = 1000
        val maxArraySize = 1_000_000

        val allTestsPassed = runStressTest(
            sorter = sorter,
            numberOfTests = numberOfTests,
            maxArraySize = maxArraySize
        )

        assertTrue(allTestsPassed, "Some tests failed!")
    }

    private fun runStressTest(
        sorter: Sorter,
        numberOfTests: Int,
        maxArraySize: Int
    ): Boolean {
        for (test in 1..numberOfTests) {
            val arraySize = Random.nextInt(1, maxArraySize + 1)
            val randomArray = IntArray(arraySize) { Random.nextInt() }
            val expected = randomArray.copyOf()
            val actual = randomArray.copyOf()

            expected.sort()

            sorter.sort(actual)

            if (!expected.contentEquals(actual)) {
                println("Test $test failed!")
                println("Array size: $arraySize")
                println("Expected result: ${expected.take(10)}...")
                println("Actual result: ${actual.take(10)}...")
                return false
            }

            if (test % 100 == 0) {
                println("$test tests passed...")
            }
        }

        println("All $numberOfTests tests passed successfully!")
        return true
    }
}
