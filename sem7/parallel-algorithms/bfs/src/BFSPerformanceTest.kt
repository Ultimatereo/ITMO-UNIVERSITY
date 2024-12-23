import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import kotlin.system.measureTimeMillis

object BFSPerformanceTest {

    private const val SIDE_LENGTH = 300

    private fun index(x: Int, y: Int, z: Int) = x * SIDE_LENGTH * SIDE_LENGTH + y * SIDE_LENGTH + z

    private val getNeighbors: (Int) -> Set<Int> = { v ->
        val x = v / (SIDE_LENGTH * SIDE_LENGTH)
        val y = (v / SIDE_LENGTH) % SIDE_LENGTH
        val z = v % SIDE_LENGTH
        if (x >= SIDE_LENGTH) {
            setOf()
        } else {
            val edges = mutableSetOf<Int>()

            if (x > 0) edges.add(index(x - 1, y, z))
            if (x < SIDE_LENGTH - 1) edges.add(index(x + 1, y, z))
            if (y > 0) edges.add(index(x, y - 1, z))
            if (y < SIDE_LENGTH - 1) edges.add(index(x, y + 1, z))
            if (z > 0) edges.add(index(x, y, z - 1))
            if (z < SIDE_LENGTH - 1) edges.add(index(x, y, z + 1))
            edges
        }
    }

    @Test
    fun performanceTest() {
        runPerformanceTest(SequentialBFS, ParallelBFS)
    }

    private fun runPerformanceTest(seq: BFSImplementation, par: BFSImplementation) {
        val src = 0
        val dst = SIDE_LENGTH * SIDE_LENGTH * SIDE_LENGTH- 1
        val size = SIDE_LENGTH * SIDE_LENGTH * SIDE_LENGTH
        val testRuns = 5

        println("Starting parallel bfs...")

        val parallelTime = measureAverageTime(testRuns) {
            par.bfs(null, src, dst, customGetNeighbours = getNeighbors, customSize = size)
        }

        println("Average parallel bfs time: $parallelTime ms")

        println("Starting sequential bfs...")

        val sequentialTime = measureAverageTime(testRuns) {
            seq.bfs(null, src, dst, customGetNeighbours = getNeighbors, customSize = size)
        }

        println("Average sequential bfs time: $sequentialTime ms")

        println("Parallel bfs is ${sequentialTime / parallelTime} times faster.")
        // Assert that parallel bfs is at least 3 times faster
        assertTrue(
            sequentialTime / parallelTime >= 3.0,
            "Parallel bfs should be at least 3 times faster."
        )

    }

    private fun measureAverageTime(testRuns: Int, block: () -> Any): Double {
        println()
        var totalTime = 0L
        repeat(testRuns) {
            var res: Any
            val time = measureTimeMillis { res = block() }
            totalTime += time
            println("Time for ${it + 1} iteration: $time ms, result : $res")
        }
        println()
        return totalTime.toDouble() / testRuns
    }
}
