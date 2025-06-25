import java.io.File
import java.util.*


fun main() {
    fun scheduling(a: LongArray, b: LongArray): Triple<LongArray, LongArray, Long> {
        val n = a.size
        val cMax = maxOf(a.sum(), b.sum(), a.zip(b) {el1, el2 -> el1 + el2}.max())
        val I = mutableListOf<Int>()
        val J = mutableListOf<Int>()

        for (i in 0 until n) {
            if (a[i] <= b[i]) {
                I.add(i)
            } else {
                J.add(i)
            }
        }
        val x = I.map { Pair(a[it], it) }.maxByOrNull { it.first }?.second
        val y = J.map { Pair(b[it], it) }.maxByOrNull { it.first }?.second
        if (y == null || (x != null && a[x] >= b[y])) {
            val sA = LongArray(n)
            val sB = LongArray(n)

            var time = 0L
            for (i in I) {
                if (i != x) {
                    sA[i] = time
                    time += a[i]
                }
            }
            time = cMax - a[x!!]
            sA[x] = time
            for (j in J) {
                time -= a[j]
                sA[j] = time
            }

            sB[x] = 0L
            time = b[x]
            for (i in I) {
                if (i != x) {
                    sB[i] = time
                    time += b[i]
                }
            }
            time = cMax
            for (j in J) {
                time -= b[j]
                sB[j] = time
            }

            return Triple(sA, sB, cMax)
        } else {
            val (sB, sA) = scheduling(b, a)
            return Triple(sA, sB, cMax)
        }
    }

    fun solve() {
        val scanner = Scanner(File("o2cmax.in").bufferedReader(Charsets.UTF_8))

        val n = scanner.nextInt()
        val a = LongArray(n) {scanner.nextLong()}
        val b = LongArray(n) {scanner.nextLong()}

        val (sA, sB, cMax) = scheduling(a, b)

        File("o2cmax.out").printWriter().use { output ->
            output.println(cMax)
            output.println(sA.joinToString(" "))
            output.println(sB.joinToString(" "))
        }
    }

    fun intervalsIntersect(interval1: Pair<Long, Long>, interval2: Pair<Long, Long>): Boolean {
        val (start1, end1) = interval1
        val (start2, end2) = interval2
        return start1 < end2 && start2 < end1
    }

    fun check(sA: LongArray, sB: LongArray, a: LongArray, b: LongArray) {
        var correct = true
        for (i in a.indices) {
            val aInterval = Pair(sA[i], sA[i] + a[i])
            val bInterval = Pair(sB[i], sB[i] + b[i])
            if (intervalsIntersect(aInterval, bInterval)) {
                correct = false
                break
            }
        }

        if (!correct) {
            println("a: " + a.joinToString(" "))
            println("b: " + b.joinToString(" "))
            println("sA: " + sA.joinToString(" "))
            println("sB: " + sB.joinToString(" "))
            println("----------")
        }

    }


    fun test() {
        val n = Random().nextInt(1, 10)
        val a = LongArray(n)
        val b = LongArray(n)
        for (i in 0 until n) {
            a[i] = Random().nextLong(0, 100)
            b[i] = Random().nextLong(0, 100)
        }
        try {
            val (sA, sB, cMax) = scheduling(a, b)
            check(sA, sB, a, b)
        } catch (e: StackOverflowError) {
            println(a.joinToString(" "))
            println(b.joinToString(" "))
        }
    }

    fun test100() {
        repeat(100) {
            test()
        }
    }
//    test100()
    solve()
}
