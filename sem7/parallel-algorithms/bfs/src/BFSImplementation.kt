sealed class BFSImplementation {
    abstract fun bfs(edges: List<Set<Int>>?,
                     src: Int, dst: Int,
                     customGetNeighbours : (Int) -> Set<Int> = { _ -> setOf() },
                     customSize: Int = -1
    ) : Int

    fun getNeighbours(v: Int, edges: List<Set<Int>>?, customGetNeighbours: (Int) -> Set<Int>): Set<Int> {
        return edges?.get(v) ?: customGetNeighbours(v)
    }

    fun getSize(edges: List<Set<Int>>?, customSize: Int): Int {
        return edges?.size ?: customSize
    }
}