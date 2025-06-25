import java.io.File
import java.util.*

fun main() {
    data class Job(val time: Int, val deadline: Int)

    fun scheduling(jobs: List<Job>): Pair<Int, List<Int>> {
        val indexedJobs = jobs.mapIndexed { index, job -> index to job }
            .sortedBy { it.second.deadline }

        val selected = mutableSetOf<Int>()
        val heap = PriorityQueue<Pair<Int, Int>>(compareByDescending { it.first })
        var time = 0

        indexedJobs.forEach { (index, job) ->
            selected.add(index)
            heap.add(job.time to index)
            time += job.time

            if (time > job.deadline) {
                val (jobTime, jobIndex) = heap.poll()
                selected.remove(jobIndex)
                time -= jobTime
            }
        }

        val result = MutableList(jobs.size) { -1 }
        time = 0

        indexedJobs.filter { selected.contains(it.first) }
            .forEach { (index, job) ->
                result[index] = time
                time += job.time
            }

        return selected.size to result
    }

    val inputFile = File("p1sumu.in").bufferedReader(Charsets.UTF_8)
    val scanner = Scanner(inputFile)

    val n = scanner.nextInt()
    val jobs = List(n) {
        Job(
            time = scanner.nextInt(),
            deadline = scanner.nextInt()
        )
    }

    val (size, schedule) = scheduling(jobs)

    val outputFile = File("p1sumu.out")
    outputFile.printWriter().use { writer ->
        writer.println(size)
        schedule.forEach { writer.print("$it ") }
    }
}