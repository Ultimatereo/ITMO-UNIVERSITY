import kotlinx.coroutines.*
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicIntegerArray

data object ParallelBFS : BFSImplementation() {
    @OptIn(ExperimentalCoroutinesApi::class)
    private val dispatcher = Dispatchers.IO.limitedParallelism(4)

    override fun bfs(edges: List<Set<Int>>?, src: Int, dst: Int, customGetNeighbours: (Int) -> Set<Int>, customSize: Int): Int {
        if (src == dst) return 0

        val visited = AtomicIntegerArray(getSize(edges, customSize)) // Потокобезопасное множество для посещенных узлов
        var frontier = ConcurrentHashMap.newKeySet<Int>() // Множество текущего фронтиера
        var newFrontier = ConcurrentHashMap.newKeySet<Int>() // Множество создаваемого фронтиера

        var curDepth = 0
        frontier.add(src)

        val result = AtomicInteger(-1)// Хранение результата

        runBlocking {
            while (frontier.isNotEmpty()) {
                frontier.chunked(512).map {
                    async(dispatcher) {
                        for (v in it) {
                            if (v == dst) {
                                result.compareAndSet(-1, curDepth)
                            }
                            if (visited.compareAndSet(v, 0, 1)) {
                                for (u in getNeighbours(v, edges, customGetNeighbours)) {
                                    if (visited.get(u) == 0) {
                                        newFrontier.add(u)
                                    }
                                }
                            }
                        }
                    }
                }.awaitAll()
                curDepth += 1
                frontier = newFrontier
                newFrontier = ConcurrentHashMap.newKeySet()
            }
        }
        return result.get()
    }
}
