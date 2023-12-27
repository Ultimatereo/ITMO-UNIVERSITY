package sem5

import java.util.*

var mark : BooleanArray = BooleanArray(0)
var g: List<MutableList<IntArray>> = List(0) { mutableListOf() }
var edges: Array<IntArray> = Array(0) { IntArray(0) }
var end: Int = 0
var start: Int = 0

fun main() {
    val scanner = Scanner(System.`in`)
    val n = scanner.nextInt()
    val m = scanner.nextInt()
    val mas = Array(n) { CharArray(m) }
    for (i in 0 until n) {
        val word = scanner.next()
        for (j in 0 until m) {
            mas[i][j] = word[j]
        }
    }

    val coordToNumber = mutableMapOf<Pair<Int, Int>, Pair<Int, Int>>()
    val numberToCoord = mutableListOf<Pair<Int, Int>>()
    var startCoord = Pair(0, 0)
    var endCoord = Pair(n - 1, m - 1)
    val inf = 3_000_000
    val rawEdges = mutableListOf<IntArray>()
    var cur = 1

    for (i in 0 until n) {
        for (j in 0 until m) {
            if (mas[i][j] == '.') {
                coordToNumber[Pair(i , j)] = Pair(cur, cur + 1)
                rawEdges.add(intArrayOf(cur, cur + 1, 1, 0))
                numberToCoord.add(Pair(i, j))
                numberToCoord.add(Pair(i, j))
                if (i > 0 && mas[i - 1][j] != '#') {
                    rawEdges.add(intArrayOf(coordToNumber[Pair(i - 1, j)]!!.second, cur, inf, 0))
                    rawEdges.add(intArrayOf(cur + 1, coordToNumber[Pair(i - 1, j)]!!.first, inf, 0))
                }
                if (j > 0 && mas[i][j - 1] != '#') {
                    rawEdges.add(intArrayOf(coordToNumber[Pair(i, j - 1)]!!.second, cur, inf, 0))
                    rawEdges.add(intArrayOf(cur + 1, coordToNumber[Pair(i, j - 1)]!!.first, inf, 0))
                }
                cur += 2
            } else if (mas[i][j] == '-' || mas[i][j] == 'A' || mas[i][j] == 'B') {
                coordToNumber[Pair(i, j)] = Pair(cur, cur + 1)
                rawEdges.add(intArrayOf(cur, cur + 1, inf, 0))
                numberToCoord.add(Pair(i, j))
                numberToCoord.add(Pair(i, j))
                if (i > 0 && mas[i - 1][j] != '#') {
                    rawEdges.add(intArrayOf(coordToNumber[Pair(i - 1, j)]!!.second, cur, inf, 0))
                    rawEdges.add(intArrayOf(cur + 1, coordToNumber[Pair(i - 1, j)]!!.first, inf, 0))
                }
                if (j > 0 && mas[i][j - 1] != '#') {
                    rawEdges.add(intArrayOf(coordToNumber[Pair(i, j - 1)]!!.second, cur, inf, 0))
                    rawEdges.add(intArrayOf(cur + 1, coordToNumber[Pair(i, j - 1)]!!.first, inf, 0))
                }
                cur += 2
                if (mas[i][j] == 'A') {
                    startCoord = Pair(i, j)
                }
                if (mas[i][j] == 'B') {
                    endCoord = Pair(i, j)
                }
            }
        }
    }

    var answer = -1
    if (rawEdges.isNotEmpty()) {
        val numberOfVertices = cur - 1
        start = coordToNumber[startCoord]!!.second
        end = coordToNumber[endCoord]!!.first
        val result = getMinCut(numberOfVertices, rawEdges)
        val size = result.size - 1
        val answerMinCost = result[size]

        if (answerMinCost < inf) {
            for (i in 0 until size) {
                val edge = result[i]
                val coord = numberToCoord[rawEdges[edge - 1][0]]
                mas[coord.first][coord.second] = '+'
            }
            answer = size
        }
    }

    println(answer)
    if (answer != -1) {
        for (line in mas) {
            println(String(line))
        }
    }
}

private fun dfs(v: Int, dmin: Int): Int {
    if (v == end || mark[v]) {
        return dmin
    }
    mark[v] = true
    for (i in g[v].indices) {
        val edgeData = g[v][i]
        val u = edgeData[0]
        val cost = edgeData[1]
        val f = edgeData[2]
        val j = edgeData[3]
        if (!mark[u] && cost - f > 0) {
            val d = dfs(u, minOf(dmin, cost - f))
            if (d > 0) {
                g[v][i][2] += d
                g[u][j][2] -= d
                return d
            }
        }
    }
    return 0
}

private fun getMinCut(numberOfVertices: Int, rawEdges: List<IntArray>): List<Int> {
    start -= 1
    end -= 1
    val numberOfEdges = rawEdges.size

    val BIG = 100_000_000

    edges = Array(numberOfEdges) { IntArray(2) }
    g = MutableList(numberOfVertices) { mutableListOf<IntArray>() }
    mark = BooleanArray(numberOfVertices)
    for (i in 0 until numberOfEdges) {
        val edge = rawEdges[i]
        val aa = edge[0]
        val bb = edge[1]
        val ccFront = edge[2]
        val ccBack = edge[3]

        g[aa - 1].add(intArrayOf(bb - 1, ccFront, 0, g[bb - 1].size))
        edges[i][0] = aa - 1
        edges[i][1] = g[aa - 1].size - 1
        g[bb - 1].add(intArrayOf(aa - 1, ccBack, 0, g[aa - 1].size - 1))
    }

    var answer = 0
    var delta = 0
    while (true) {
        mark.fill(false)
        delta = dfs(start, BIG)
        if (delta != 1) {
            break
        }
        answer += delta
    }
    if (delta > 1) {
        return listOf(BIG)
    }
    mark.fill(false)
    dfs(start, BIG)
    val answerList = mutableListOf<Int>()
    for (i in 0 until numberOfEdges) {
        val edge = edges[i]
        if (mark[edge[0]] && !mark[g[edge[0]][edge[1]][0]]) {
            answerList.add(i + 1)
        }
    }
    answerList.add(answer)
    return answerList
}
