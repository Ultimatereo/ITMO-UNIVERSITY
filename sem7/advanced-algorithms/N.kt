import java.io.File
import java.util.*

fun main() {
    fun scheduling(p1: List<Long>, p2: List<Long>): Pair<Long, List<Int>> {
        val todo = PriorityQueue(compareBy<Triple<Long, Int, Boolean>> { it.first }.reversed())

        for (i in p1.indices) {
            todo.add(Triple(-minOf(p1[i], p2[i]), i, p1[i] < p2[i]))
        }

        val l = LinkedList<Int>()
        val r = LinkedList<Int>()

        while (todo.isNotEmpty()) {
            val (_, index, firstFlag) = todo.poll()
            if (firstFlag) {
                l.addLast(index)
            } else {
                r.addFirst(index)
            }
        }

        l.addAll(r)

        var firstCurrent = 0L
        var secondCurrent = 0L

        for (i in l) {
            firstCurrent += p1[i]
            secondCurrent = maxOf(firstCurrent, secondCurrent) + p2[i]
        }

        return Pair(secondCurrent, l)
    }
    val scanner = Scanner(File("f2cmax.in").bufferedReader(Charsets.UTF_8))
    val n = scanner.nextInt()
    val p1 = List(n) { scanner.nextLong() }
    val p2 = List(n) { scanner.nextLong() }

    val (size, schedule) = scheduling(p1, p2)

    File("f2cmax.out").bufferedWriter().use { writer ->
        writer.write("$size\n")

        repeat(2) {
            schedule.forEach { writer.write("${it + 1} ") }
            writer.write("\n")
        }
    }
}
