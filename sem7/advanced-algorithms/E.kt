import java.io.File;
import java.util.*;

fun main() {
    data class Job(val deadline: Long, val weight: Long)

    fun scheduleP1SumWu(jobs: List<Job>): Pair<Long, List<Long>> {
        val indexedJobs = jobs.mapIndexed { index, job -> index to job }
        val sortedJobs = indexedJobs.sortedBy { it.second.deadline }

        val selected = mutableSetOf<Int>()
        val heap = PriorityQueue<Pair<Long, Int>>(compareByDescending { it.first })
        var time = 1L

        sortedJobs.forEachIndexed { i, (_, job) ->
            selected.add(i)
            heap.add(-job.weight to i)

            if (job.deadline >= time) {
                time++
            } else {
                selected.remove(heap.poll().second)
            }
        }

        val result = MutableList(jobs.size) { -1L }
        var sumWu = 0L
        time = 0L

        sortedJobs.forEachIndexed { i, (originalIndex, job) ->
            if (selected.contains(i)) {
                result[originalIndex] = time
                time++
            } else {
                result[originalIndex] = (selected.size + originalIndex).toLong()
                sumWu += job.weight
            }
        }

        return sumWu to result
    }

    val input = Scanner(File("p1sumwu.in").bufferedReader(Charsets.UTF_8))
    val n = input.nextInt()

    val jobs = List(n) { Job(input.nextLong(), input.nextLong()) }
    val (size, schedule) = scheduleP1SumWu(jobs)

    File("p1sumwu.out").printWriter().use { writer ->
        writer.println(size)
        writer.println(schedule.joinToString(" "))
    }
}
