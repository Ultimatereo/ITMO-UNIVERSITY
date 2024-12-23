data object SequentialBFS : BFSImplementation() {
    override fun bfs(
        edges: List<Set<Int>>?,
        src: Int,
        dst: Int,
        customGetNeighbours: (Int) -> Set<Int>,
        customSize: Int
    ): Int {
        val visited = mutableSetOf<Int>()
        val queue: ArrayDeque<Pair<Int, Int>> = ArrayDeque()
        queue.add(Pair(src, 0))
        while (queue.isNotEmpty()) {
            val v = queue.removeFirst()
            if (v.first == dst) {
                return v.second
            }
            for (u in getNeighbours(v.first, edges, customGetNeighbours)) {
                if (visited.add(u)) {
                    queue.add(Pair(u, v.second + 1))
                }
            }
        }
        return -1
    }
}