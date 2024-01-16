package bot

import constants.FOEBotMessages
import daily.DailyTaskExecutor
import daily.ReminderTask
import org.telegram.telegrambots.meta.api.methods.send.SendMessage
import sheets.SheetsManager

object ResponseHandler : ResponseHandlerInterface {
    private val EMAIL_REGEX =
        ("^(?:[a-z0-9!#\$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#\$%&'*+/=?^_`{|}~-]+)" +
                "*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]" +
                "|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]" +
                "*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(2(5[0-5]|[0-4][0-9])" +
                "|1[0-9][0-9]|[1-9]?[0-9])\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])" +
                "|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\" +
                "[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)])").toRegex()

    private fun isEmailValid(email: String): Boolean {
        return EMAIL_REGEX.matches(email)
    }

    data class ClientData(
        var sheetsId: String?,
        var dialogMode: DialogMode,
        var currentIndex: Int,
        var emotions: List<String>?,
        var dailyTaskExecutors: MutableList<DailyTaskExecutor>
    )

    private val mapClient = mutableMapOf<Long, ClientData>()

    init {
        try {
            SheetsManager.updateMaps(mapClient, FOEBot)
        } catch (e: Exception) {
            System.err.println("Updating chat ids and sheets ids failed!")
            e.printStackTrace()
        }
    }

    override fun helpCommand(chatId: Long) {
        try {
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            createMessage(chatId, FOEBotMessages.HELP_MESSAGE)
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.HELP_MESSAGE_ERROR)
            System.err.println("Something wrong happened when helping")
            e.printStackTrace()
        }
    }

    override fun startCommand(chatId: Long) {
        try {
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            if (mapClient.containsKey(chatId)) {
                createMessage(chatId, FOEBotMessages.createTablesMessage(getSheetsId(chatId)))
            } else {
                createSpreadSheetCommand(chatId)
            }
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.SHEETS_CREATION_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something wrong happened when adding client to data base")
            e.printStackTrace()
        }
    }

    private fun createSpreadSheetCommand(chatId: Long) {
        val sheetsId = SheetsManager.createSpreadsheetFairy()
        SheetsManager.addClient(sheetsId, chatId.toString())
        createMessage(chatId, FOEBotMessages.START_MESSAGE)
        mapClient[chatId] = ClientData(sheetsId, DialogMode.EMAIL, 0, mutableListOf(), mutableListOf())
    }

    override fun tablesCommand(chatId: Long) {
        try {
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            createMessage(chatId, FOEBotMessages.createTablesMessage(getSheetsId(chatId)))
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.TABLES_ERROR)
            System.err.println("Something wrong happened when tables")
            e.printStackTrace()
        }
    }

    override fun cancelCommand(chatId: Long) {
        cancelCommand(chatId, true)
    }

    private fun cancelCommand(chatId: Long, log: Boolean) {
        try {
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            if (log) {
                createMessage(chatId, FOEBotMessages.CANCELLATION_SUCCESS)
            }
        } catch (e: Exception) {
            if (log) {
                createMessage(chatId, FOEBotMessages.CANCELLATION_ERROR)
            }
            System.err.println("Something wrong happened when cancelling to data base")
            e.printStackTrace()
        }
    }

    override fun addEmotionCommand(chatId: Long) {
        try {
            createMessage(chatId, FOEBotMessages.WRITE_EMOTION)
            mapClient[chatId]!!.dialogMode = DialogMode.ADD_EMOTION
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.EMOTION_ADD_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something wrong happened when adding emotion")
            e.printStackTrace()
        }
    }

    override fun addRateCommand(chatId: Long) {
        try {
            if (mapClient[chatId]!!.dialogMode == DialogMode.ADD_RATE) {
                createMessage(chatId, FOEBotMessages.RATE_IN_PROCCESS)
            } else {
                try {
                    mapClient[chatId]!!.emotions = SheetsManager.getAllEmotions(getSheetsId(chatId))
                } catch (e: Exception) {
                    System.err.println("Something wrong happened when getting all emotions")
                    e.printStackTrace()
                }
                mapClient[chatId]!!.dialogMode = DialogMode.ADD_RATE
                mapClient[chatId]!!.currentIndex = 0
                createMessage(chatId, FOEBotMessages.RATE_EMOTIONS)
                createMessage(chatId, FOEBotMessages.rateEmotion(getEmotion(chatId)))
            }
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.RATE_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something wrong happened when adding rates")
            e.printStackTrace()
        }
    }

    private fun getEmotion(chatId: Long) =
        mapClient[chatId]!!.emotions!![mapClient[chatId]!!.currentIndex]

    override fun getEmotionsCommand(chatId: Long) {
        try {
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            createMessage(chatId, FOEBotMessages.writeAllEmotions(getSheetsId(chatId)))
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.GET_EMOTIONS_ERROR)
            System.err.println("Something went wrong when getting emotions")
            e.printStackTrace()
        }
    }

    override fun getTimeCommand(chatId: Long) {
        try {
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            createMessage(chatId, FOEBotMessages.writeTime(mapClient[chatId]!!.dailyTaskExecutors))
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.GET_TIME_ERROR)
            System.err.println("Something went wrong when getting time")
            e.printStackTrace()
        }
    }

    override fun linkEmailCommand(chatId: Long) {
        try {
            createMessage(chatId, FOEBotMessages.LINK_EMAIL)
            mapClient[chatId]!!.dialogMode = DialogMode.EMAIL
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.LINK_EMAIL_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something went wrong when getting time")
            e.printStackTrace()
        }
    }

    override fun addTimeCommand(chatId: Long) {
        try {
            createMessage(chatId, FOEBotMessages.ADD_TIME)
            mapClient[chatId]!!.dialogMode = DialogMode.ADD_TIME
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.ADD_TIME_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something went wrong when setting time")
            e.printStackTrace()
        }
    }

    override fun removeTimeCommand(chatId: Long) {
        try {
            if (mapClient[chatId]!!.dailyTaskExecutors.isEmpty()) {
                createMessage(chatId, FOEBotMessages.REMOVE_TIME_EMPTY)
                mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            } else {
                createMessage(chatId, FOEBotMessages.removeTime(mapClient[chatId]!!.dailyTaskExecutors))
                mapClient[chatId]!!.dialogMode = DialogMode.REMOVE_TIME
            }
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.REMOVE_TIME_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something went wrong when cancelling reminder")
            e.printStackTrace()
        }
    }

    override fun cancelReminderCommand(chatId: Long) {
        try {
            for (dailyTaskExecutor in mapClient[chatId]!!.dailyTaskExecutors) {
                dailyTaskExecutor.stop()
            }
            mapClient[chatId]!!.dailyTaskExecutors = mutableListOf()
            SheetsManager.cancelReminder(chatId.toString(), getSheetsId(chatId))
            createMessage(chatId, FOEBotMessages.CANCEL_REMINDER_SUCCESS)
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.CANCEL_REMINDER_ERROR)
            System.err.println("Something went wrong when cancelling reminder")
            e.printStackTrace()
        }
    }

    override fun resetCommand(chatId: Long) {
        try {
            createMessage(chatId, FOEBotMessages.RESET_START)
            SheetsManager.deleteSpreadSheet(getSheetsId(chatId))
            createMessage(chatId, FOEBotMessages.RESET_STOP)
            createSpreadSheetCommand(chatId)
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.RESET_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something went wrong when resetting reminder")
            e.printStackTrace()
        }
    }

    override fun supportCommand(chatId: Long) {
        createMessage(chatId, FOEBotMessages.SUPPORT)
        mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
    }

    override fun notCommand(chatId: Long, text: String) {
        try {
            when (mapClient[chatId]!!.dialogMode) {
                DialogMode.EMAIL -> {
                    if (isEmailValid(text)) {
                        try {
                            SheetsManager.givePermissionToSpreadsheet(
                                getSheetsId(chatId), text
                            )
                        } catch (e: Exception) {
                            createMessage(chatId, FOEBotMessages.EMAIL_WRONG)
                            return
                        }
                        createMessage(chatId, FOEBotMessages.accessIsGivenTo(text))
                        createMessage(chatId, FOEBotMessages.createTablesMessage(getSheetsId(chatId)))
                        mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
                    } else {
                        createMessage(chatId, FOEBotMessages.EMAIL_WRONG)
                    }
                }
                DialogMode.ADD_EMOTION -> {
                    val emotionsForPrinting: List<String> =
                        text.split(",")
                    try {
                        SheetsManager.addEmotions(emotionsForPrinting, getSheetsId(chatId))
                    } catch (e: Exception) {
                        System.err.println(
                            "Something went wrong when adding emotions " +
                                    "for user with chatId $chatId"
                        )
                        e.printStackTrace()
                        createMessage(chatId, FOEBotMessages.EMOTION_ADD_FAIL)
                        return
                    }
                    mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
                    createMessage(chatId, FOEBotMessages.EMOTION_ADD_SUCCESS)
                }
                DialogMode.ADD_RATE -> {
                    try {
                        val currentRate = text.toInt()
                        if ((10 >= currentRate) && (currentRate >= 1)) {
                            SheetsManager.addRate(
                                getEmotion(chatId),
                                currentRate,
                                getSheetsId(chatId)
                            )
                            mapClient[chatId]!!.currentIndex++
                            if (mapClient[chatId]!!.currentIndex == mapClient[chatId]!!.emotions!!.size) {
                                mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
                                createMessage(chatId, FOEBotMessages.RATE_END)
                            } else {
                                createMessage(chatId, FOEBotMessages.rateEmotion(getEmotion(chatId)))
                            }
                        } else {
                            createMessage(chatId, FOEBotMessages.RATE_RETRY_WRONG_RANGE)
                        }
                    } catch (e: Exception) {
                        createMessage(chatId, FOEBotMessages.RATE_RETRY_NOT_INT)
                    }
                }
                DialogMode.ADD_TIME -> {
                    try {
                        val requests = text.split(",")
                        for (request in requests) {
                            try {
                                val hoursAndMinutes = request.split(":")
                                val hours = hoursAndMinutes[0].filterNot { it.isWhitespace() }.toInt()
                                val minutes = hoursAndMinutes[1].filterNot { it.isWhitespace() }.toInt()
                                val dailyTaskExecutorValue = DailyTaskExecutor(
                                    ReminderTask(FOEBot),
                                    hours, minutes, chatId
                                )
                                if (mapClient[chatId]!!.dailyTaskExecutors.contains(dailyTaskExecutorValue)) {
                                    createMessage(chatId, FOEBotMessages.setTimeAlreadyExists(hours, minutes))
                                } else {
                                    dailyTaskExecutorValue.startExecution()
                                    mapClient[chatId]!!.dailyTaskExecutors.add(dailyTaskExecutorValue)
                                    createMessage(chatId, FOEBotMessages.setTimeSuccess(hours, minutes))
                                }
                                updateDailyTaskExecutors(chatId)
                            } catch (e: Exception) {
                                createMessage(chatId, FOEBotMessages.SET_TIME_FAIL)
                            }
                        }
                    } catch (e: Exception) {
                        createMessage(chatId, FOEBotMessages.SET_TIME_FAIL)
                    }
                    mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
                }
                DialogMode.REMOVE_TIME -> {
                    try {
                        val requests = text.split(",")
                        val tasks = mutableListOf<DailyTaskExecutor>()
                        for (request in requests) {
                            val num = request.filterNot { it.isWhitespace() }.toInt()
                            if (num < 1 || num > requests.size) {
                                createMessage(chatId, FOEBotMessages.removeTimeWrong(num))
                            } else {
                                tasks.add(mapClient[chatId]!!.dailyTaskExecutors[num - 1])
                            }
                        }
                        for (task in tasks) {
                            task.stop()
                            createMessage(chatId, FOEBotMessages.removeTimeSuccess(task))
                            mapClient[chatId]!!.dailyTaskExecutors.remove(task)
                        }
                        updateDailyTaskExecutors(chatId)
                    } catch (e: Exception) {
                        createMessage(chatId, FOEBotMessages.REMOVE_TIME_FAIL)
                    }
                    mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
                }
                else -> {
                    createMessage(chatId, FOEBotMessages.UNKNOWN_MESSAGE)
                    mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
                }
            }
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.NOT_COMMAND_ERROR)
            cancelCommand(chatId, false)
            System.err.println("Something went wrong when parsing not command")
            e.printStackTrace()
        }
    }

    private fun updateDailyTaskExecutors(chatId: Long) {
        SheetsManager.setTime(chatId.toString(), getSheetsId(chatId), mapClient[chatId]!!.dailyTaskExecutors)
    }

    private fun getSheetsId(chatId: Long) = mapClient[chatId]!!.sheetsId!!

    fun createMessage(chatId: Long, text: String) {
        try {
            val sendMessage = SendMessage()
            sendMessage.chatId = chatId.toString()
            sendMessage.text = text
            FOEBot.execute(sendMessage)
        } catch (e: Exception) {
            createMessage(chatId, FOEBotMessages.CREATE_MESSAGE_ERROR)
            mapClient[chatId]!!.dialogMode = DialogMode.DEFAULT
            System.err.println("Something wrong happened when trying to create a message")
            e.printStackTrace()
        }
    }
}