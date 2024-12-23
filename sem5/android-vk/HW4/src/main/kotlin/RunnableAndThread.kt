import java.util.concurrent.Executors

class RunnableAndThread {
    class SumCalculator(private val numbers: IntArray) : Runnable {
        override fun run() {
            val sum = numbers.sum()
            System.err.println("Sum of elements in array: $sum")
        }
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            val numbers = intArrayOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

            val sumCalculator = SumCalculator(numbers)

            val executor = Executors.newSingleThreadExecutor()
            executor.execute(sumCalculator)

            executor.shutdown()
        }
    }
}