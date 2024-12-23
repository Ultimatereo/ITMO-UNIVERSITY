package company.vk.polis.task1

object MessageController {
    fun getValidData(): List<Entity> {
        val answer = mutableListOf<Entity>()
        val info = Repository.getInfo()
        for (entity in info) {
            when (entity) {
                is ChatInterface -> {
                    if (entity.messageIds != null && entity.userIds != null && entity.id != null) {
                        answer.add(entity)
                    }
                }

                is Message -> {
                    if (entity.id != null && entity.senderId != null && entity.text != null && entity.state != null && entity.timestamp != null) {
                        answer.add(entity)
                    }
                }

                is User -> {
                    if (entity.id != null && entity.name != null) {
                        answer.add(entity)
                    }
                }

                else -> throw AssertionError("Unknown class of Entity: ${entity.javaClass.toGenericString()}")
            }
        }
        return answer
    }


    fun getChatItemForId(chatId: Int, state: State? = null): ChatItem {
        val info = Repository.getInfo()
        val chat = info.filterIsInstance<ChatInterface>().first { it.id == chatId }
        val messagesIds = chat.messageIds ?: throw AssertionError("Message ids of chat $chatId is null!")
        val messages = info.filterIsInstance<Message>().filter { it.id in messagesIds }
        var message = messages.sortedBy { it.timestamp }.last()
        val curState = state ?: message.state
        if (curState is State.Deleted) {
            val user = info.filterIsInstance<User>().first { it.id == curState.userId }
            message = Message(
                message.id,
                "Сообщение было удалено ${user.name}",
                message.senderId,
                message.timestamp,
                message.state
            )
        }
        val avatarUrl: String? = when (chat) {
            is GroupChat -> chat.avatarUrl
            is Chat -> info.filterIsInstance<User>().first { it.id == chat.userIds!![1] }.avatarUrl
            else -> throw AssertionError("Unknown class inherited from ChatInterface ${chat.javaClass.toGenericString()}")
        }
        return ChatItem(message, avatarUrl, curState)
    }

    fun countMessagesFromChatAndUser(chatId: Int, userId: Int?): Int {
        val info = Repository.getInfo()
        val chat = info.filterIsInstance<ChatInterface>().first { it.id == chatId }
        val messageIds = (chat.messageIds ?: throw AssertionError("Message ids of chat $chat are unknown"))
        if (userId == null) {
            return messageIds.size
        }
        return info.filterIsInstance<Message>().filter { it.id in messageIds }.count { it.senderId == userId }
    }
}