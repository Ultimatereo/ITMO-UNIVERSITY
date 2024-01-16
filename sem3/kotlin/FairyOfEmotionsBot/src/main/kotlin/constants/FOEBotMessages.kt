package constants

import daily.DailyTaskExecutor
import sheets.SheetsManager

class FOEBotMessages {
    companion object {
        const val REMOVE_TIME_ERROR: String = "Не удалось удалить напоминалки :("
        const val REMOVE_TIME_FAIL: String =
            "Не получилось удалить напоминания. Возможно, что вы некорректно ввели данные!"
        const val REMOVE_TIME_EMPTY: String = "У вас не стоит напоминаний, поэтому и удалять вам нечего :)"
        const val RESET_STOP: String = "Отлично! Старая таблица удалена! Приступаю к созданию новой..."
        const val RESET_START: String = "Начинаем ресет..."
        private const val WRITE_TO_SUPPORT =
            "Если же ничего не помогает, то напиши /support, чтобы обратиться в поддержку."
        const val SUPPORT = "По всем вопросам, связанным с ботом, можно обратиться ко мне в лс: @Ultimatereo"
        const val RESET_ERROR = "Не получилось сделать ресет, к сожалению\n$WRITE_TO_SUPPORT"
        const val NOT_COMMAND_ERROR: String = "Что-то пошло не так при обработке обычного текста.\n" +
                "Убедись, что ты написал всё, как надо\n" +
                WRITE_TO_SUPPORT
        const val CANCEL_REMINDER_ERROR: String = "Не получилось отключить напоминалки.\n$WRITE_TO_SUPPORT"
        const val ADD_TIME_ERROR: String = "Не получилось поставить или изменить время напоминания.\n$WRITE_TO_SUPPORT"
        const val LINK_EMAIL_ERROR: String = "Не получилось привязать почту к таблице.\n$WRITE_TO_SUPPORT"
        const val GET_TIME_ERROR: String = "Не получилось вывести время напоминания.\n$WRITE_TO_SUPPORT"
        const val RATE_ERROR: String = "Не получилось добавить запись.\n$WRITE_TO_SUPPORT"
        const val EMOTION_ADD_ERROR: String = "Не получилось добавить эмоцию.\n$WRITE_TO_SUPPORT"
        const val CANCELLATION_ERROR: String = "Не получилось отменить операцию.\n$WRITE_TO_SUPPORT"
        const val TABLES_ERROR: String = "У меня не получилось вывести ссылку на таблицу\n" +
                "Убедитесь, что таблица создана\n" +
                WRITE_TO_SUPPORT
        const val HELP_MESSAGE_ERROR: String = "У меня не получилось вывести все команды :(\n$WRITE_TO_SUPPORT"
        const val CREATE_MESSAGE_ERROR: String =
            "Что-то пошло не так и, у меня не получилось тебе отправить сообщение :(\n" +
                    "Убедись, что всё в твоей таблице корректно\n" +
                    WRITE_TO_SUPPORT
        const val GET_EMOTIONS_ERROR: String = "Не получилось вывести все эмоции"
        const val SHEETS_CREATION_ERROR: String = "Не получилось создать для вас таблицу :(\n" +
                "Попробуйте ещё раз создать таблицу с помощью /start"
        const val DAILY_REMINDER: String = "Это твоё ежедневное напоминание!\n" +
                "Скорее пиши /add_rate и записывай, как ты себя чувствуешь <3"
        const val SET_TIME_FAIL: String = "Данные были введены неверно. Попробуйте ещё раз."
        const val CANCEL_REMINDER_SUCCESS: String = "Напоминалки успешно удалены"
        const val ADD_TIME: String =
            "Введи, пожалуйста, время по МСК в формате HH:mm\n" +
                    "Если вы хотите несколько напоминаний, то введите их через запятую\n" +
                    "Например, 10:00, 19:00"
        const val LINK_EMAIL: String =
            "Введите, пожалуйста, почту gmail или любую другую, привязанную к гуглу.\n" +
                    "Если вы введёте почту, никак не связанную с гуглом, то вы не получите доступ к таблице"
        private const val RATE_RETRY = "Введите оценку ещё раз. Оценка должна быть от 1 до 10!\n" +
                "Если вы хотите отменить запись, то просто напишите /cancel"
        const val RATE_RETRY_NOT_INT: String = "Вы ввели не число!\n $RATE_RETRY"
        const val RATE_RETRY_WRONG_RANGE: String = "Вы ввели число не от 1 до 10!\n $RATE_RETRY"
        const val RATE_END = "Поздравляю, чекап завершён!"
        const val EMOTION_ADD_SUCCESS = "Всё успешно добавлено!"
        const val EMOTION_ADD_FAIL = "Что-то пошло не так. И не получилось добавить эмоцию :("
        const val EMAIL_WRONG =
            "Что-то не так с вашей почтой. Введите её ещё раз.\n" +
                    "Убедитесь, что вы вписали корректную почту."
        const val RATE_IN_PROCCESS = "Запись оценки по эмоциям в процессе! " +
                "Ответьте, пожалуйста, на последний вопрос!"
        const val CANCELLATION_SUCCESS = "Команда успешно отменена!"
        const val RATE_EMOTIONS: String = "Начнём же оценку каждой из эмоций, дорогой друг."
        const val WRITE_EMOTION =
            "Введите через запятую все эмоции, за которыми вы хотите следить. \nДля отмены операции введите /cancel."
        const val HELP_MESSAGE =
            "Доступны следующие команды:\n\n" +
                    "/help - Вывод всех доступных команд\n" +
                    "/start - Начало работы с ботом. Создание таблицы\n" +
                    "/tables - Вывод таблицы\n" +
                    "/cancel - Отмена последней операции\n" +
                    "/add_emotion - Добавить эмоцию в список трекаемых эмоций\n" +
                    "/add_rate - Добавить запись, оценку эмоций\n" +
                    "/get_emotions - Выводит список всех трекуемых эмоций через запятую\n" +
                    "/link_email - Привязать почту к таблице\n" +
                    "/add_time - Добавить ещё напоминалок\n" +
                    "/remove_time - Удалить некоторые напоминалки\n" +
                    "/cancel_reminder - Удалить все напоминалки\n" +
                    "/get_time - Узнать время напоминания\n" +
                    "/support - Поддержка и помощь по вопросам, связанных с ботом"
        const val START_MESSAGE =
            "Отправь, пожалуйста, свою почту на @gmail.com или привязанную почту к Google-аккаунту.\n" +
                    "Пожалуйста, указывайте реальную почту, иначе вы не получите доступа к таблице с данными\n" +
                    "Например vasya@gmail.com\n" +
                    "Если ты хочешь отменить создание таблицы, то просто напиши /cancel"
        const val UNKNOWN_MESSAGE = "Я не понимаю, чего вы добиваетесь этим сообщением."

        fun rateEmotion(emotion: String): String {
            return "Оцени, как сильно ты сегодня проявлял(а) эмоцию '$emotion' от 1 до 10"
        }

        fun accessIsGivenTo(email: String): String {
            return "Доступ к таблице успешно выдан на почту $email"
        }

        fun writeAllEmotions(sheetsId: String) =
            """
            Вот список всех эмоций, которые трекуются:
            ${SheetsManager.getAllEmotions(sheetsId).joinToString(", ")}
            """.trimIndent()

        fun createTablesMessage(sheetsId: String): String =
            """
                Вот твоя таблица со всеми эмоциями и записями:
                https://docs.google.com/spreadsheets/d/$sheetsId/edit#gid=0
            """.trimIndent()

        fun writeTime(dailyTaskExecutors: MutableList<DailyTaskExecutor>): String {
            if (dailyTaskExecutors.isEmpty()) {
                return "Время напоминания не установлено!"
            }
            var answer = "Установленные времена напоминания:"
            for (i in 1..dailyTaskExecutors.size) {
                val element = dailyTaskExecutors[i - 1]
                answer = "$answer\n$i. ${element.targetHour}:${element.targetMin}"
            }
            return answer
        }

        fun setTimeSuccess(hours: Int, minutes: Int): String =
            "Напоминалка $hours:$minutes успешно поставлена!"

        fun removeTime(dailyTaskExecutors: MutableList<DailyTaskExecutor>): String {
            val text = "Выведите через запятую номера напоминалок, которые вы хотите удалить.\n" +
                    "Например, '1, 3' удалит первую и третью напоминалку\n"
            return "$text\n${writeTime(dailyTaskExecutors)}"

        }

        fun setTimeAlreadyExists(hours: Int, minutes: Int): String =
            "Напоминалка $hours:$minutes уже была поставлена до этого!"

        fun removeTimeWrong(num: Int): String = "Напоминалки под номером $num не существует!"
        fun removeTimeSuccess(task: DailyTaskExecutor): String =
            "Напоминалка ${task.targetHour}:${task.targetMin} успешно отключена!"

    }
}