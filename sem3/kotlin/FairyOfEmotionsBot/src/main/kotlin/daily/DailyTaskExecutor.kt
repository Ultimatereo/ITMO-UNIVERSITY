package daily

import java.time.Duration
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit


class DailyTaskExecutor(
    private val dailyTask: DailyTask,
    val targetHour: Int,
    val targetMin: Int,
    private val chatId: Long,
    private val targetSec: Int = 0
) : Comparable<DailyTaskExecutor> {
    private val executorService: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
    fun startExecution() {
        val taskWrapper = Runnable {
            dailyTask.execute(chatId)
            startExecution()
        }
        val delay = computeNextDelay(targetHour, targetMin, targetSec)
        executorService.schedule(taskWrapper, delay, TimeUnit.SECONDS)
    }

    private fun computeNextDelay(targetHour: Int, targetMin: Int, targetSec: Int): Long {
        val localNow = LocalDateTime.now()
        val currentZone = ZoneId.systemDefault()
        val zonedNow = ZonedDateTime.of(localNow, currentZone)
        var zonedNextTarget = zonedNow.withHour(targetHour).withMinute(targetMin).withSecond(targetSec)
        if (zonedNow >= zonedNextTarget) zonedNextTarget = zonedNextTarget.plusDays(1)
        val duration: Duration = Duration.between(zonedNow, zonedNextTarget)
        return duration.seconds
    }

    fun stop() {
        executorService.shutdownNow()
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as DailyTaskExecutor

        if (targetHour != other.targetHour) return false
        if (targetMin != other.targetMin) return false
        if (chatId != other.chatId) return false
        if (targetSec != other.targetSec) return false

        return true
    }

    override fun hashCode(): Int {
        var result = targetHour
        result = 31 * result + targetMin
        result = 31 * result + chatId.hashCode()
        result = 31 * result + targetSec
        return result
    }

    override fun compareTo(other: DailyTaskExecutor): Int {
        return (60 * targetHour + targetMin).compareTo(60 * other.targetHour + targetMin)
    }

}