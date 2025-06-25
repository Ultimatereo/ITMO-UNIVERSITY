import java.io.*
import java.util.*

fun main() {
    val inputFile = "p1p1sumu.in"
    val outputFile = "p1p1sumu.out"

    val scanner = Scanner(File(inputFile).bufferedReader(Charsets.UTF_8))
    val n = scanner.nextInt()
    var d1 = scanner.nextInt()
    var d2 = scanner.nextInt()
    val a = scanner.nextLong()
    val b = scanner.nextLong()
    val c = scanner.nextLong()
    val d = scanner.nextLong()

    val deadlines = IntArray(n + 2)
    var time = 0

    time += processInitialDeadlines(d1, n, deadlines)
    time += processInitialDeadlines(d2, n, deadlines)

    repeat(n - 2) {
        val d3 = calculateNextDeadline(a, b, c, d1, d2, d)
        d1 = d2
        d2 = d3

        time += processInitialDeadlines(d3, n, deadlines)
    }

    time += processJobs(n, deadlines)

    File(outputFile).printWriter().use { writer ->
        writer.println(time)
    }
}

fun processInitialDeadlines(d: Int, n: Int, deadlines: IntArray): Int {
    var time = 0
    if (d - 1 >= n) {
        time++
    } else if (d > 0) {
        deadlines[d]++
    }
    return time
}

fun calculateNextDeadline(a: Long, b: Long, c: Long, d1: Int, d2: Int, d: Long): Int {
    return ((a * d1 + b * d2 + c) % d).toInt()
}

fun processJobs(n: Int, deadlines: IntArray): Int {
    var time = 0
    var completedJobs = 0
    repeat(n) {
        val current = minOf(it + 1 - completedJobs, deadlines[it + 1])
        completedJobs += current
        time += current
    }
    return time
}
