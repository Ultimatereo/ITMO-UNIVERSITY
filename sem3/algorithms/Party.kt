fun main() {
    data class Vertex(val name: Int) {
        val edgesIn = mutableSetOf<Vertex>()
        val edgesOut = mutableSetOf<Vertex>()
        var time = 0
        var color = 0
        var group = 0
        var value = false
    }

    class Graph {
        val vertices = mutableMapOf<Int, Vertex>()
        val edges = mutableSetOf<Pair<Vertex, Vertex>>()
        val groupsAndVertices = mutableMapOf<Int, MutableSet<Vertex>>()
        var time = 1
        operator fun get(i: Int) = vertices[i]
        fun addVertex(name: Int) {
            val vertex = Vertex(name)
            vertices[name] = vertex
        }
        private fun addEdge(a: Vertex, b:Vertex) {
            a.edgesOut.add(b)
            b.edgesIn.add(a)
            edges.add(Pair(a, b))
        }
        fun addEdge(a: Int, b: Int) {
            addEdge(this[a]!!, this[b]!!)
        }

        fun addEdge2SAT(a: Int, b: Int) {
            addEdge(-a, b)
            addEdge(-b, a)
        }
        fun updateDFS() {
            time = 1
            for (vertex in vertices.values) {
                vertex.color = 0
            }
        }
        fun dfsForFindingComponents1(start: Vertex) {
            start.color = 1
            for (edge in start.edgesIn) {
                if (edge.color == 0) {
                    dfsForFindingComponents1(edge)
                }
            }
            start.time = time++
        }

        fun dfsForFindingComponents2(start: Vertex, group: Int) {
            start.color = 1
            start.group = group
            groupsAndVertices[group]!!.add(start)
            for (edge in start.edgesOut) {
                if (edge.color == 0) {
                    dfsForFindingComponents2(edge, group)
                }
            }
        }

        fun updateGroupsAndVertices() : MutableMap<Int, MutableSet<Vertex>> {
            updateDFS()
            for (vertex in vertices.values) {
                if (vertex.color == 0) {
                    dfsForFindingComponents1(vertex)
                }
            }
            updateDFS()
            val order = vertices.values.sortedBy { -it.time }
            var group = 1
            groupsAndVertices.clear()
            for (vertex in order) {
                if (vertex.color == 0) {
                    groupsAndVertices[group] = mutableSetOf(vertex)
                    dfsForFindingComponents2(vertex, group++)
                }
            }
            return groupsAndVertices
        }

        fun getCondensedGraph() : Graph {
            val condensedGraph = Graph()
            for (i in 1..groupsAndVertices.size) {
                condensedGraph.addVertex(i)
            }
            for (edge in edges) {
                if (edge.first.group != edge.second.group) {
                    condensedGraph.addEdge(edge.first.group, edge.second.group)
                }
            }
            return condensedGraph
        }

        private fun clear() {
            vertices.clear()
            edges.clear()
            groupsAndVertices.clear()
        }

        fun updateIn(start: Vertex) {
            start.color = 1
            start.value = false
            for (edge in start.edgesIn) {
                if (edge.color == 0) {
                    updateIn(edge)
                }
            }
        }

        fun updateOut(start: Vertex) {
            start.color = 1
            start.value = true
            for (edge in start.edgesOut) {
                if (edge.color == 0) {
                    updateOut(edge)
                }
            }
        }

        fun checkFor2Sat(): Boolean {
            var hasAnswer = true
            for (i in 1..(vertices.size / 2)) {
                if (this[i]!!.group == this[-i]!!.group) {
                    hasAnswer = false
                }
            }
            return hasAnswer
        }

        fun findSolutionFor2Sat() : MutableSet<Int>? {
            updateGroupsAndVertices()
            val hasAnswer = checkFor2Sat()
            if (hasAnswer) {
                val condensedGraph = getCondensedGraph()
                for (vertex in condensedGraph.vertices.values) {
                    if (vertex.color == 0) {
                        dfsForFindingComponents1(vertex)
                    }
                }
                val answer = mutableSetOf<Int>()
                for (i in 1..(vertices.size/2)) {
                    val group1 = condensedGraph[this[i]!!.group]!!
                    val group2 = condensedGraph[this[-i]!!.group]!!
                    if (group1.time > group2.time) {
                        answer.add(i)
                    }
                }
                return answer
            }
            return(null)
        }
    }
    fun getNumberFromName(name: String, map: MutableMap<String, Int>): Int {
        val name1 = name.substring(1)
        if (name[0] == '-') {
            return -map[name1]!!
        }
        return map[name1]!!
    }
    val (n, m) = readLine()!!.split(" ").map(String::toInt)
    val numberToNames = mutableMapOf<Int, String>()
    val namesToNumber = mutableMapOf<String, Int>()
    val graph = Graph()
    for (i in 1..n) {
        val name = readLine()!!
        numberToNames[i] = name
        namesToNumber[name] = i
        graph.addVertex(i)
        graph.addVertex(-i)
    }
    for (i in 0 until m) {
        val (from, _, to) = readLine()!!.split(" ")
        val a = getNumberFromName(from, namesToNumber)
        val b = getNumberFromName(to, namesToNumber)
        graph.addEdge(a, b)
        graph.addEdge(-b, -a)

    }
    val answer = graph.findSolutionFor2Sat()
    if (answer == null) {
        println(-1)
    } else {
        println(answer.size)
        for (number in answer) {
            println(numberToNames[number])
        }
    }
}

