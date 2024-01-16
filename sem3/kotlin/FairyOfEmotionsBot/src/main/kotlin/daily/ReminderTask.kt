package daily

class ReminderTask(private val callback: Callback) : DailyTask {
    interface Callback {
        fun onTimeForDailyTask(chatId: Long)
    }

    override fun execute(chatId: Long) {
        callback.onTimeForDailyTask(chatId)
    }
}