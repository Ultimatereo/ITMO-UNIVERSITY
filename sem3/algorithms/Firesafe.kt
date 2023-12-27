import java.util.*

fun main() {
    data class Vertex(val name: Int) : Comparable<Vertex> {
        val edgesOut : MutableSet<Vertex> = mutableSetOf()
        val edgesIn : MutableSet<Vertex> = mutableSetOf()
        var color : Int = 0 // 0 - white 1 - grey 2 - black
        var time: Int = -1
        var group = 0
        override fun compareTo(other: Vertex): Int {
            if (this.edgesOut.size == other.edgesOut.size) {
                return 1
            }
            return this.edgesOut.size.compareTo(other.edgesOut.size)
        }
    }
    class Graph {
        val vertices = mutableMapOf<Int, Vertex>()
        val orderedVertices = TreeSet<Int>()
        val edges = mutableSetOf<Pair<Vertex, Vertex>>()
        val size = vertices.size
        operator fun get(i: Int) = vertices[i]
        var time = 1
        fun addVertex(name: Int) {
            val vertex = Vertex(name)
            vertices[name] = vertex
            orderedVertices.add(vertex.name)
        }
        private fun addEdge(a: Vertex, b: Vertex) {
            a.edgesOut.add(b)
            b.edgesIn.add(a)
            edges.add(Pair(a, b))
        }
        private fun deleteEdge(a: Vertex, b: Vertex) {
            a.edgesOut.remove(b)
            b.edgesIn.remove(a)
            edges.remove(Pair(a, b))
        }

        fun addEdge(a: Int, b: Int) {
            addEdge(this[a]!!, this[b]!!)
        }
        fun deleteEdge(a: Int, b: Int) {
            deleteEdge(this[a]!!, this[b]!!)
        }
        fun deleteVertex(vertex: Vertex) {
            while (vertex.edgesIn.isNotEmpty()) {
                deleteVertex(vertex.edgesIn.first())
            }
            while (vertex.edgesOut.isNotEmpty()) {
                deleteEdge(vertex, vertex.edgesOut.first())
            }
            vertices.remove(vertex.name)
            orderedVertices.remove(vertex.name)
        }
        fun dfs1(start: Vertex) {
            start.color = 1
            for (edge in start.edgesIn) {
                if (edge.color == 0) {
                    dfs1(edge)
                }
            }
            start.time = time++
        }
        fun dfs2(start: Vertex, group: Int) {
            start.color = 1
            start.group = group
            for (edge in start.edgesOut) {
                if (edge.color == 0) {
                    dfs2(edge, group)
                }
            }
        }
        fun updateForDFS() {
            for (vertex in vertices) {
                vertex.value.color = 0
            }
        }

    }

    val n = readLine()!!.toInt()
    val m = readLine()!!.toInt()
    val graph = Graph()
    for (i in 1..n) {
        graph.addVertex(i)
    }
    for (i in 0 until m) {
        val (a, b) = readLine()!!.split(" ").map(String::toInt)
        graph.addEdge(a, b)
    }
    graph.time = 1
    for (vertex in graph.vertices) {
        if (vertex.value.color == 0) {
            graph.dfs1(vertex.value)
        }
    }
    val order = graph.vertices.values.sortedBy { -it.time }
    graph.updateForDFS()
    var group = 1
    val groupsAndVertices = mutableMapOf<Int, Vertex>()
    for (vertex in order) {
        if (vertex.color == 0) {
            groupsAndVertices[group] = vertex
            graph.dfs2(vertex, group)
            group += 1
        }
    }
    val condensationGraph = Graph()
    for (i in 1 until group) {
        condensationGraph.addVertex(i)
    }
    for (edge in graph.edges) {
        if (edge.first.group != edge.second.group) {
            condensationGraph.addEdge(edge.first.group, edge.second.group)
        }
    }

    val answer = IntArray(group)
    var ind = 0
    while (condensationGraph.vertices.isNotEmpty()) {
        val vertexName = condensationGraph.orderedVertices.first()
        val vertex = condensationGraph[vertexName]!!
        answer[ind++] = groupsAndVertices[vertexName]!!.name
        while (vertex.edgesIn.isNotEmpty()) {
            condensationGraph.deleteVertex(vertex.edgesIn.first())
        }
        condensationGraph.deleteVertex(vertex)
    }

    println(ind)
    for (i in 0 until ind) {
        print("${answer[i]} ")
    }
}