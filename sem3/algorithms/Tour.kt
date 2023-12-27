fun main() {
//    data class Vertex(val name: Int) {
////        val edgesIn = mutableSetOf<Vertex>()
////        val edgesOut = mutableSetOf<Vertex>()
//        var time = 0
//        var color = 0
//        var group = 0
//        var value = false
//    }

    class Graph(val maxSize: Int) {
        //        val edges = mutableSetOf<Pair<Vertex, Vertex>>()

        //        var size = 0
        val edges = Array<MutableList<Int>>(maxSize) { mutableListOf() }
        val invertedEdges = Array<MutableList<Int>>(maxSize) { mutableListOf() }

        //        val nameVertices = Array(maxSize) {it - maxSize/2}
        val timeVertices = Array(maxSize) { 0 }
        val colorVertices = Array(maxSize) { 0 }
        val groupVertices = Array(maxSize) { -1 }

        //        val valueVertices = Array(maxSize) {false}
        val groupsAndVertices = mutableMapOf<Int, MutableSet<Int>>()
        val order = ArrayDeque<Int>()
        var time = 1

//        fun setN(n : Int) {
//            size = n
//        }

        fun addEdge(a: Int, b: Int) {
            edges[a].add(b)
            invertedEdges[b].add(a)
        }

        fun addEdge2SAT(a: Int, b: Int) {
            addEdge(getNegative(a), b)
            addEdge(getNegative(b), a)
        }

        fun getNegative(a: Int): Int {
            return maxSize - a
        }
//        fun updateDFS() {
//            time = 1
//            for (i in 0 until maxSize) {
//                timeVertices[i] = 0
//                colorVertices[i] = 0
//            }
//        }

        fun dfsForFindingComponents1(start: Int) {
            colorVertices[start] = 1
            for (edge in edges[start]) {
                if (colorVertices[edge] == 0) {
                    dfsForFindingComponents1(edge)
                }
            }
            order.addFirst(start)
            timeVertices[start] = time++
        }

        fun dfsForFindingComponents2(start: Int, group: Int) {
            groupVertices[start] = group
            groupsAndVertices[group]!!.add(start)
            for (edge in invertedEdges[start]) {
                if (groupVertices[edge] == -1) {
                    dfsForFindingComponents2(edge, group)
                }
            }
        }

        fun updateGroupsAndVertices(n : Int) {
//            updateDFS()
            for (i in 1..n) {
                if (colorVertices[i] == 0) {
                    dfsForFindingComponents1(i)
                }
                if (colorVertices[getNegative(i)] == 0) {
                    dfsForFindingComponents1(getNegative(i))
                }
            }
//            updateDFS()
            var group = 1
//            groupsAndVertices.clear()
            for (vertex in order) {
                if (groupVertices[vertex] == -1) {
                    groupsAndVertices[group] = mutableSetOf(vertex)
                    dfsForFindingComponents2(vertex, group++)
                }
            }
        }

        fun checkFor2Sat(n: Int): Boolean {
            var hasAnswer = true
            for (i in 1..n) {
                if (groupVertices[i] == groupVertices[getNegative(i)]) {
                    hasAnswer = false
                }
            }
            return hasAnswer
        }

        fun findSolutionFor2Sat(n: Int, offset: Int): MutableSet<Int>? {
            updateGroupsAndVertices(offset)
            val hasAnswer = checkFor2Sat(n)
            if (hasAnswer) {
                val answer = mutableSetOf<Int>()
                for (i in 1..n) {
                    if (groupVertices[i] > groupVertices[getNegative(i)]) {
                        answer.add(i)
                    }
                }
                return answer
            }
            return null
        }
    }

    fun getVertex(el1: Int, graph: Graph): Int {
        return if (el1 > 0) el1 else graph.getNegative(-el1)
    }

    val (n, m, k) = readLine()!!.split(" ").map(String::toInt)
    val requests = mutableMapOf<Int, MutableList<Int>>()
    val graph = Graph(2*(m + 2*k) + 100)
    for (i in 0 until k) {
        val (a, b) = readLine()!!.split(" ").map(String::toInt)
        if (requests.containsKey(a)) {
            requests[a]!!.add(getVertex(b, graph))
        } else {
            requests[a] = mutableListOf(getVertex(b, graph))
        }
    }
    var offset = m + 1
    for (request in requests.values) {
        val starts = IntArray(request.size) { 0 }
        val ends = IntArray(request.size) { 0 }
        for (i in 0 until request.size) {
            starts[i] = offset
            ends[i] = graph.getNegative(offset)
//                    graph.addVertex(offset) // S
//                    graph.addVertex(-offset) // P
            graph.addEdge(starts[i], request[i])
            graph.addEdge(ends[i], request[i])
            if (i > 0) {
                graph.addEdge(ends[i], ends[i - 1])
                graph.addEdge(starts[i - 1], starts[i])
            }
            offset++
        }
        for (i in 0 until request.size) {
            if (i > 0) {
                graph.addEdge(graph.getNegative(request[i]), ends[i - 1])
            }
            if (i < request.size - 1) {
                graph.addEdge(graph.getNegative(request[i]), starts[i + 1])
            }
        }
    }
    val answer = graph.findSolutionFor2Sat(m, offset - 1)
    if (answer == null) {
        println("-1")
    } else {
        println(answer.size)
        for (a in answer) {
            print("$a ")
        }
    }
}


