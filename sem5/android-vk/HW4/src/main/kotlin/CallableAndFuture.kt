import java.util.concurrent.Callable
import java.util.concurrent.Executors
import java.util.concurrent.Future
import kotlin.math.pow

class CallableAndFuture {
    class PowerCalculator(private val base: Double, private val exponent: Int) : Callable<Double> {
        override fun call(): Double {
            return base.pow(exponent.toDouble())
        }
    }

    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            val base = 2.0
            val exponent = 3

            val powerCalculator = PowerCalculator(base, exponent)

            val executor = Executors.newSingleThreadExecutor()
            val future: Future<Double> = executor.submit(powerCalculator)

            try {
                val result = future.get()
                System.err.println("$base to the $exponent = $result")
            } catch (e: Exception) {
                e.printStackTrace()
            } finally {
                executor.shutdown()
            }
        }
    }

}
