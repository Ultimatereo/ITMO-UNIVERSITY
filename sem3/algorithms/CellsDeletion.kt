fun main() {
    data class Vertex(val name: String) {
        val neighbors = mutableSetOf<Vertex>()
    }
    class Graph {
        val vertices = mutableMapOf<Pair<Int, Int>, Vertex>()
        private operator fun get(name: Pair<Int, Int>) = vertices[name] ?: throw IllegalArgumentException()
        fun addVertex(name: Pair<Int, Int>) {
            vertices[name] = Vertex(name.toString())
        }
        private fun connect(first: Vertex, second: Vertex) {
            first.neighbors.add(second)
            second.neighbors.add(first)
        }
        fun connect(first: Pair<Int, Int>, second: Pair<Int, Int>) =
            connect(this[first], this[second])
        fun neighbors(name: Pair<Int, Int>) = vertices[name]?.neighbors?.map {it.name} ?: listOf()
    }
    fun dfs(start: Vertex, visited: MutableSet<Vertex> = mutableSetOf()) {
        visited.add(start)
        for (neighbor in start.neighbors) {
            if (!visited.contains(neighbor)) {
                dfs(neighbor, visited)
            }
        }
    }
    fun countComponents(graph: Graph) : Int {
        val visited = mutableSetOf<Vertex>()
        var counter = 0
        for (vertex in graph.vertices) {
            if (!visited.contains(vertex.value)) {
                dfs(vertex.value, visited)
                counter++
            }
        }
        return counter
    }
    val (m, n) = readLine()!!.split(" ").map(String::toInt)
    val matrix = mutableListOf<List<Boolean>>()
    for (i in 0 until m) {
        matrix.add(readLine()!!.map { it == 'O'})
    }
    val graph = Graph()
    for (i in 0 until m) {
        for (j in 0 until n) {
            if (matrix[i][j]) {
                graph.addVertex(Pair(i, j))
                if (i > 0 && matrix[i - 1][j]) {
                    graph.connect(Pair(i, j), Pair(i - 1, j))
                }
                if (j > 0 && matrix[i][j - 1]) {
                    graph.connect(Pair(i, j), Pair(i, j - 1))
                }
            }
        }
    }
//    println(graph.neighbors(Pair(0, 5)))
    println(countComponents(graph))
}