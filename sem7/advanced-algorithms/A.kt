import java.io.File
import java.util.*

fun main() {
    data class Job(val p: Int, val r: Int, val d: Int)
    data class Interval(val start: Double, val end: Double) {
        val length: Double = end - start
    }

    fun getIntervalWorkerIndex(n: Int, intervalIndex: Int, m: Int, worker: Int) = n + 1 + intervalIndex * m + worker

    fun getIntervalIndex(n: Int, t: Int, m: Int, intervalIndex: Int) = n + t * m + 1 + intervalIndex

    fun findMaxFlow(capacity: Array<MutableMap<Int, Double>>, s: Int, t: Int): Double {
        val n = capacity.size
        val height = IntArray(n)
        height[s] = n
        val flow = Array(n) { mutableMapOf<Int, Double>() }
        val excess = DoubleArray(n)
        excess[s] = Double.MAX_VALUE
        val edges = Array(n) { mutableListOf<Int>() }
        for (u in 0 until n) {
            for (edge in capacity[u]) {
                edges[u].add(edge.key)
                edges[edge.key].add(u)
            }
        }

        fun push(u: Int, v: Int) {
            val d = minOf(excess[u], capacity[u].getOrDefault(v, 0.0) - flow[u].getOrDefault(v, 0.0))
            flow[u][v] = flow[u].getOrDefault(v, 0.0) + d
            flow[v][u] = flow[v].getOrDefault(u, 0.0) - d
            excess[u] -= d
            excess[v] += d
        }

        fun relabel(u: Int) {
            var d = Int.MAX_VALUE
            for (i in edges[u]) {
                if (capacity[u].getOrDefault(i, 0.0) - flow[u].getOrDefault(i, 0.0) > 0) d = minOf(d, height[i])
            }
            if (d < Int.MAX_VALUE) height[u] = d + 1
        }

        fun findMaxHeightVertices(s: Int, t: Int): MutableList<Int> {
            val maxHeight = mutableListOf<Int>()
            for (i in 0 until n) {
                if (i != s && i != t && excess[i] > 0) {
                    if (maxHeight.isNotEmpty() && height[i] > height[maxHeight[0]]) maxHeight.clear()
                    if (maxHeight.isEmpty() || height[i] == height[maxHeight[0]]) maxHeight.add(i)
                }
            }
            return maxHeight
        }

        for (i in edges[s]) {
            push(s, i)
        }

        var current = findMaxHeightVertices(s, t)
        while (current.isNotEmpty()) {
            for (i in current) {
                var pushed = false
                for (j in edges[i]) {
                    if (excess[i] == 0.0) {
                        break
                    }
                    if (capacity[i].getOrDefault(j, 0.0) - flow[i].getOrDefault(
                            j,
                            0.0
                        ) > 0 && height[i] == height[j] + 1
                    ) {
                        push(i, j)
                        pushed = true
                    }
                }
                if (!pushed) {
                    relabel(i)
                    break
                }
            }
            current = findMaxHeightVertices(s, t)
        }
        return excess[t]
    }


    fun check(jobs: MutableList<Job>, productivity: MutableList<Int>, late: Double, n: Int, m: Int): Boolean {
        if (late < 0) {
            return false
        }

        val times = sortedSetOf<Double>().apply {
            jobs.forEach {
                add(it.r.toDouble())
                add(it.d + late)
            }
        }.toMutableList()

        val intervals = mutableListOf<Interval>()
        for (i in 0 until times.size - 1) {
            intervals.add(Interval(times[i], times[i + 1]))
        }

        val t = intervals.size
        val graphSize = 1 + n + t * m + t + 1
        val graph = Array(graphSize) { mutableMapOf<Int, Double>() }
        val start = 0
        val finish = n + t * m + t + 1

        for (i in jobs.indices) {
            val job = jobs[i]
            val jobIndex = i + 1
            graph[start][jobIndex] = job.p.toDouble()

            for (intervalIndex in intervals.indices) {
                val interval = intervals[intervalIndex]
                if (job.r <= interval.start && interval.end <= job.d + late) {
                    for (worker in 0 until m) {
                        graph[jobIndex][getIntervalWorkerIndex(n, intervalIndex, m, worker)] =
                            (productivity[worker] - productivity[worker + 1]) * interval.length
                    }
                }
            }
        }

        val pSum = productivity.sum()

        for (intervalIndex in intervals.indices) {
            val interval = intervals[intervalIndex]
            for (worker in 0 until m) {
                graph[getIntervalWorkerIndex(n, intervalIndex, m, worker)][getIntervalIndex(n, t, m, intervalIndex)] =
                    (worker + 1) * (productivity[worker] - productivity[worker + 1]) * interval.length
            }
            graph[getIntervalIndex(n, t, m, intervalIndex)][finish] = pSum * interval.length
        }

        val maxFlow = findMaxFlow(graph, start, finish)
        return maxFlow == jobs.sumOf { it.p }.toDouble()
    }
    // Создаём Scanner для чтения из файла
    val scanner = Scanner(File("cheese.in"))

    // Читаем n и m
    val n = scanner.nextInt()
    val m = scanner.nextInt()

    val jobs = mutableListOf<Job>()
    var pMax = -1

    // Читаем задания
    repeat(n) {
        val p = scanner.nextInt()
        val r = scanner.nextInt()
        val d = scanner.nextInt()
        jobs.add(Job(p, r, d))
        pMax = maxOf(pMax, p)
    }

    // Читаем производительности
    val productivity = mutableListOf<Int>()
    repeat(m) {
        productivity.add(scanner.nextInt())
    }

    productivity.sortDescending()
    productivity.add(0)

    var left: Double = (-1).toDouble()
    var right: Double = (pMax * n).toDouble()
    while (right - left > 10e-5) {
        val mid = (left + right) / 2
        if (check(jobs, productivity, mid, n, m)) {
            right = mid
        } else {
            left = mid
        }
    }
    File("cheese.out").writeText(right.toString())
}