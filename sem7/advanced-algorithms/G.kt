import java.io.File
import java.util.*

fun main() {
    fun getCmax(n: Int, p1: IntArray, p2: IntArray): Int {
        val maxTime = p1.sum()
        var prevDp = IntArray(maxTime + 1) { Int.MAX_VALUE }
        var currDp = IntArray(maxTime + 1) { Int.MAX_VALUE }

        prevDp[0] = 0

        for (i in 0 until n) {
            for (j in 0..maxTime) {
                if (j >= p1[i]) {
                    currDp[j] = minOf(currDp[j], prevDp[j - p1[i]])
                }
                if (prevDp[j] != Int.MAX_VALUE) {
                    currDp[j] = minOf(currDp[j], prevDp[j] + p2[i])
                }
            }
            // Swap references to save memory
            val temp = prevDp
            prevDp = currDp
            currDp = temp
            currDp.fill(Int.MAX_VALUE)
        }

        var answer = Int.MAX_VALUE
        for (j in 0..maxTime) {
            if (prevDp[j] != Int.MAX_VALUE) {
                answer = minOf(answer, maxOf(j, prevDp[j]))
            }
        }

        return answer
    }

    val input = Scanner(File("r2cmax.in").bufferedReader(Charsets.UTF_8))
    val n = input.nextInt()
    val p1 = IntArray(n) { input.nextInt() }
    val p2 = IntArray(n) { input.nextInt() }
    val cMax = getCmax(n, p1, p2)

    File("r2cmax.out").printWriter().use { writer ->
        writer.println(cMax)
    }
}