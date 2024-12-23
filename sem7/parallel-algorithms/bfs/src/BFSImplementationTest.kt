import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.Test

class BFSImplementationTest {
    @Nested
    @DisplayName("Sequential version BFS Test")
    inner class SequentialBFSTest {
        private val bfsImplementation = SequentialBFS
        @Test
        fun testBFSWithValidPath() {
            testBFSWithValidPath(bfsImplementation)
        }

        @Test
        fun testBFSWithNoPath() {
            testBFSWithNoPath(bfsImplementation)
        }

        @Test
        fun testBFSWithSingleNode() {
            testBFSWithSingleNode(bfsImplementation)
        }

        @Test
        fun testBFSWithMultipleNodes() {
            testBFSWithMultipleNodes(bfsImplementation)
        }

        @Test
        fun testBFSWithDirectConnection() {
            testBFSWithDirectConnection(bfsImplementation)
        }
    }

    @Nested
    @DisplayName("Parallel version BFS Test")
    inner class ParallelBFSTest {
        private val bfsImplementation = ParallelBFS
        @Test
        fun testBFSWithValidPath() {
            testBFSWithValidPath(bfsImplementation)
        }

        @Test
        fun testBFSWithNoPath() {
            testBFSWithNoPath(bfsImplementation)
        }

        @Test
        fun testBFSWithSingleNode() {
            testBFSWithSingleNode(bfsImplementation)
        }

        @Test
        fun testBFSWithMultipleNodes() {
            testBFSWithMultipleNodes(bfsImplementation)
        }

        @Test
        fun testBFSWithDirectConnection() {
            testBFSWithDirectConnection(bfsImplementation)
        }
    }

    @Nested
    @DisplayName("Stress Tests")
    inner class StressTest {
        @Test
        @DisplayName("Small graphs. Maximum number of nodes - 10.")
        fun testSmallGraphs() {
            stressTest(1000, 10, 5)
            stressTest(1000, 10, 10)
            stressTest(1000, 10, 20)
            stressTest(1000, 10, 30)
            stressTest(1000, 10, 40)
            stressTest(1000, 10, 50)
        }

        @Test
        @DisplayName("Big graphs. Maximum number of nodes - 10000.")
        fun testBigGraphs() {
            stressTest(1000, 10000, 50000)
        }
    }

    fun testBFSWithValidPath(bfsImplementation: BFSImplementation) {
        val edges = listOf(
            setOf(1, 2),      // Ребра из вершины 0
            setOf(0, 3),      // Ребра из вершины 1
            setOf(0, 3),      // Ребра из вершины 2
            setOf(1, 2, 4),   // Ребра из вершины 3
            setOf(3)          // Ребра из вершины 4
        )
        val src = 0
        val dst = 4

        val result = bfsImplementation.bfs(edges, src, dst)

        // Ожидаем минимальное расстояние между вершинами 0 и 4
        assertEquals(3, result)
    }


    fun testBFSWithNoPath(bfsImplementation: BFSImplementation) {
        val edges = listOf(
            setOf(1),         // Ребра из вершины 0
            setOf(0),         // Ребра из вершины 1
            setOf(3),         // Ребра из вершины 2
            setOf(2)          // Ребра из вершины 3
        )
        val src = 0
        val dst = 3

        val result = bfsImplementation.bfs(edges, src, dst)

        // Ожидаем -1, так как пути нет
        assertEquals(-1, result)
    }


    fun testBFSWithSingleNode(bfsImplementation: BFSImplementation) {
        val edges = listOf<Set<Int>>(
            emptySet() // Вершина 0, без рёбер
        )
        val src = 0
        val dst = 0

        val result = bfsImplementation.bfs(edges, src, dst)

        // Ожидаем 0, так как src == dst
        assertEquals(0, result)
    }


    fun testBFSWithMultipleNodes(bfsImplementation: BFSImplementation) {
        val edges = listOf(
            setOf(1),     // Ребра из вершины 0
            setOf(0),     // Ребра из вершины 1
            emptySet(),   // Вершина 2 (изолирована)
            emptySet()    // Вершина 3 (изолирована)
        )
        val src = 0
        val dst = 2

        val result = bfsImplementation.bfs(edges, src, dst)

        // Ожидаем -1, так как src и dst находятся в разных компонентах
        assertEquals(-1, result)
    }


    fun testBFSWithDirectConnection(bfsImplementation: BFSImplementation) {
        val edges = listOf(
            setOf(1),     // Ребра из вершины 0
            setOf(0, 2),  // Ребра из вершины 1
            setOf(1)      // Ребра из вершины 2
        )
        val src = 0
        val dst = 2

        val result = bfsImplementation.bfs(edges, src, dst)

        // Ожидаем минимальное расстояние между вершинами 0 и 2
        assertEquals(2, result)
    }

    fun stressTest(testRuns: Int, nodeCount: Int, edgeCount: Int) {
        repeat(testRuns) {
            val random = java.util.Random()

            // Generate a random graph
            val edges = MutableList(nodeCount) { mutableSetOf<Int>() }
            repeat(edgeCount) {
                val u = random.nextInt(nodeCount)
                val v = random.nextInt(nodeCount)
                if (u != v) { // Avoid self-loops
                    edges[u].add(v)
                    edges[v].add(u)
                }
            }

            // Randomly pick source and destination nodes
            val src = random.nextInt(nodeCount)
            val dst = random.nextInt(nodeCount)

            // Perform BFS using both implementations
            val sequentialResult = SequentialBFS.bfs(edges, src, dst)
            val parallelResult = ParallelBFS.bfs(edges, src, dst)

            // Assert the results are the same
            assertEquals(
                sequentialResult,
                parallelResult,
                "Mismatch between SequentialBFS and ParallelBFS results. Edges: $edges, src: $src, dst: $dst"
            )
        }
    }

}
