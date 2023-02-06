import java.util.*

fun main() {
    //data class Edge(val vertex: Vertex, val weight: Int) : Comparable<Edge> {
//    override fun compareTo(other: Edge): Int {
//        return this.weight - other.weight
//    }
//}

    data class Vertex(val name: Int) : Comparable<Vertex> {
//        val edgesIn = mutableSetOf<Pair<Vertex, Int>>()
        val edgesOut = mutableSetOf<Vertex>()
        var color = 0
        override fun compareTo(other: Vertex): Int {
            return this.name.compareTo(other.name)
        }

    }

    class Graph {
        val vertices = mutableMapOf<Int, Vertex>()
        //    val edges = mutableSetOf<Triple<Vertex, Vertex, Int>>()
        fun size() = vertices.size
        val topOrdering = ArrayDeque<Vertex>()
        var hasCycle = false
        operator fun get(name: Int) = vertices[name] ?: throw IllegalArgumentException()
        fun addVertex(name: Int) {
            if (!vertices.containsKey(name)) {
                vertices[name] = Vertex(name)
            }
        }

        private fun connect(first: Vertex, second: Vertex, weight: Int) {
            first.edgesOut.add(second)
//            second.edgesIn.add(Pair(first, weight))
//        edges.add(Triple(first, second, weight))
        }

        fun connect(first: Int, second: Int, weight: Int) =
            connect(this[first], this[second], weight)
    }

    fun dfs(graph: Graph, start: Vertex) {
        if (graph.hasCycle) {
            return
        }
        start.color = 1
        for (edge in start.edgesOut) {
            if (edge.color != 2) {
                if (edge.color == 1) {
                    graph.hasCycle = true
                    return
                }
                dfs(graph, edge)
            }
        }
        start.color = 2
        graph.topOrdering.addFirst(start)
    }
    fun topologicalOrder(graph: Graph) {
        for (vertex in graph.vertices) {
            if (vertex.value.color != 2) {
                if (graph.hasCycle) {
                    return
                }
                dfs(graph, vertex.value)
            }
        }
    }
    val (n, a, b) = readLine()!!.split(" ").map(String::toInt)
    val graph = Graph()
    for (i in 1..n) {
        graph.addVertex(i)
        graph.addVertex(-i)
        graph.connect(
            i,
            -i, 0
        )
    }
    for (i in 0 until a + b) {
        val (l, r) = readLine()!!.split(" ").map(String::toInt)
        if (i < a) {
            graph.connect(
                -l,
                r, 0
            )
        } else {
            graph.connect(
                l,
                -r, 0
            )
            graph.connect(
                r,
                -l, 0
            )
        }
    }
    topologicalOrder(graph)
    if (graph.hasCycle) {
        println("NO")
    } else {
        println("YES")
        val starts = IntArray(n)
        val ends = IntArray(n)
        var i = 0
        for (vertex in graph.topOrdering) {
            i += 1
            if (vertex.name > 0) {
                starts[vertex.name - 1] = i
            } else {
                ends[-vertex.name - 1] = i
            }
        }
        for (j in 0 until n) {
            println("${starts[j]} ${ends[j]}")
        }
    }
}