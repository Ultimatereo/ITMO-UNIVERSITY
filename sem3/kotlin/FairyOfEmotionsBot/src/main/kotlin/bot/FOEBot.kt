package bot

import constants.FOEBotMessages
import daily.ReminderTask
import org.telegram.telegrambots.bots.TelegramLongPollingBot
import org.telegram.telegrambots.meta.api.objects.Update
import properties.ProjectProperties

object FOEBot : TelegramLongPollingBot(), ReminderTask.Callback {
    init {
        ResponseHandler
    }

    private var token = ""
    override fun getBotToken(): String {
        if (token.isEmpty()) {
            token = ProjectProperties.mainProperties.getProperty("BOT_TOKEN")
            if (token.isEmpty()) {
                System.err.println("Bot token wasn't found in main.properties!")
            }
        }
        return token
    }

    override fun getBotUsername(): String {
        return "FairyOfEmotionsBot"
    }

    override fun onUpdateReceived(update: Update?) {
        if (update == null || update.message == null || update.message.chatId == null) {
            return
        }
        val chatId = update.message.chatId

        if (!update.hasMessage()) return

        if (update.message.text == null) {
            update.message.text = ""
        }

        System.err.println(update.message.text)

        when (update.message.text) {
            "/help" -> ResponseHandler.helpCommand(chatId)
            "/start" -> ResponseHandler.startCommand(chatId)
            "/tables" -> ResponseHandler.tablesCommand(chatId)
            "/cancel" -> ResponseHandler.cancelCommand(chatId)
            "/add_emotion" -> ResponseHandler.addEmotionCommand(chatId)
            "/add_rate" -> ResponseHandler.addRateCommand(chatId)
            "/get_emotions" -> ResponseHandler.getEmotionsCommand(chatId)
            "/get_time" -> ResponseHandler.getTimeCommand(chatId)
            "/link_email" -> ResponseHandler.linkEmailCommand(chatId)
            "/add_time" -> ResponseHandler.addTimeCommand(chatId)
            "/remove_time" -> ResponseHandler.removeTimeCommand(chatId)
            "/cancel_reminder" -> ResponseHandler.cancelReminderCommand(chatId)
            "/reset" -> ResponseHandler.resetCommand(chatId)
            "/support" -> ResponseHandler.supportCommand(chatId)
            else -> ResponseHandler.notCommand(chatId, update.message.text)
        }
    }

    override fun onTimeForDailyTask(chatId: Long) {
        ResponseHandler.createMessage(chatId, FOEBotMessages.DAILY_REMINDER)
    }
}