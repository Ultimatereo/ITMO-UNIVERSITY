class RunnableAndThreadAdv {
    class SumCalculator(private val numbers: IntArray, private val start: Int, private val end: Int) : Runnable {
        private var result: Int = 0

        override fun run() {
            for (i in start until end) {
                result += numbers[i]
            }
        }

        fun getResult(): Int {
            return result
        }
    }
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            val numbers = intArrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
            val numThreads = 3
            val segmentSize = numbers.size / numThreads

            val threads = mutableListOf<Thread>()
            val calculators = mutableListOf<SumCalculator>()

            for (i in 0 until numThreads) {
                val start = i * segmentSize
                val end = if (i == numThreads - 1) numbers.size else (i + 1) * segmentSize
                val calculator = SumCalculator(numbers, start, end)
                calculators.add(calculator)
                val thread = Thread(calculator)
                thread.start()
                threads.add(thread)
            }

            for (thread in threads) {
                thread.join()
            }

            var totalSum = 0
            for (calculator in calculators) {
                totalSum += calculator.getResult()
            }

            println("Total sum: $totalSum")
        }
    }
}