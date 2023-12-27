import java.util.*
import kotlin.math.min

//data class Edge(val vertex: Vertex, val weight: Int) : Comparable<Edge> {
//    override fun compareTo(other: Edge): Int {
//        return this.weight - other.weight
//    }
//}

data class Vertex(val name: Int) : Comparable<Vertex> {
    val edgesIn = mutableSetOf<Pair<Vertex, Int>>()
    val edgesOut = mutableSetOf<Pair<Vertex, Int>>()
    var distance = Int.MAX_VALUE
    override fun compareTo(other: Vertex): Int {
        return this.distance - other.distance
    }

}

class Graph {
    val vertices = mutableMapOf<Int, Vertex>()
    //    val edges = mutableSetOf<Triple<Vertex, Vertex, Int>>()
    fun size() = vertices.size
    val topOrdering = ArrayDeque<Vertex>()
    var visited = mutableSetOf<Vertex>()
    operator fun get(name: Int) = vertices[name] ?: throw IllegalArgumentException()
    fun addVertex(name: Int) {
        vertices[name] = Vertex(name)
    }

    private fun connect(first: Vertex, second: Vertex, weight: Int) {
        first.edgesOut.add(Pair(second, weight))
        second.edgesIn.add(Pair(first, weight))
//        edges.add(Triple(first, second, weight))
    }

    fun connect(first: Int, second: Int, weight: Int) =
        connect(this[first], this[second], weight)
}

//fun dijkstra(graph: Graph, s: Int, t: Int) {
//    graph[s].distance = 0
//    val unsettledVertices = TreeSet<Vertex>()
//
//    unsettledVertices.add(graph[s])
//
//    while (!unsettledVertices.isEmpty()) {
//        val curV = unsettledVertices.pollFirst()!!
//        for (edge in curV.edgesOut) {
//            edge.vertex.distance = min(edge.vertex.distance, curV.distance + edge.weight)
//            unsettledVertices.add(edge.vertex)
//        }
//    }
//}
//fun reverseDijkstra(graph: Graph, s: Int, t: Int) {
//    graph[t].distance = 0
//    val unsettledVertices = TreeSet<Vertex>()
//
//    unsettledVertices.add(graph[t])
//
//    while (!unsettledVertices.isEmpty()) {
//        val curV = unsettledVertices.pollFirst()!!
//        for (edge in curV.edgesIn) {
//            unsettledVertices.remove(edge.first)
//            edge.first.distance = min(edge.first.distance, curV.distance + edge.second)
//            unsettledVertices.add(edge.first)
//        }
//    }
//}

//fun ford(graph: Graph, s: Int, t: Int) {
//    graph[s].distance = 0
//    var ok : Boolean = false
//    while (!ok) {
//        ok = true
//        for (edge in graph.edges) {
//            if (edge.first.distance != Int.MAX_VALUE && edge.first.distance + edge.third < edge.second.distance) {
//                edge.second.distance = edge.first.distance + edge.third
//                ok = false
//            }
//        }
//    }
//}
fun dfs(graph: Graph, start: Vertex) {
    graph.visited.add(start)
    for (edge in start.edgesOut) {
        if (!graph.visited.contains(edge.first)) {
            dfs(graph, edge.first)
        }
    }
    graph.topOrdering.addFirst(start)
}
fun topologicalOrder(graph: Graph) {
    graph.visited = mutableSetOf()
    for (vertex in graph.vertices) {
        if (!graph.visited.contains(vertex.value)) {
            dfs(graph, vertex.value)
        }
    }
}
fun main() {
    val (n, m, s, t) = readLine()!!.split(" ").map(String::toInt)
    val graph = Graph()
    for (i in 1..n) {
        graph.addVertex(i)
    }
    for (i in 0 until m) {
        val (b, e, w) = readLine()!!.split(" ").map(String::toInt)
        graph.connect(b, e, w)
    }
//    dijkstra(graph, s, t)
//    val answer = graph[t].distance
//    reverseDijkstra(graph, s, t)
//    Do fucking dfs to get topological order and then count minimum path to every node in that order!!!
//    ford(graph, s, t)
    topologicalOrder(graph)
    graph[s].distance = 0
    for (vertex in graph.topOrdering) {
        for (edge in vertex.edgesIn) {
            if (edge.first.distance != Int.MAX_VALUE) {
                vertex.distance = min(vertex.distance, edge.first.distance + edge.second)
            }
        }
    }
    val answer = graph[t].distance
    if (answer == Int.MAX_VALUE) {
        println("Unreachable")
    } else {
        println(answer)
    }
}