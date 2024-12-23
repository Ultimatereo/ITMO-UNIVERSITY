package company.vk.polis.task1

fun main() {
    val list = MessageController.getValidData()
    for (entity in list) {
        println(entity)
    }
    for (i in 1 until 10) {
        println(MessageController.getChatItemForId(i))
    }
    for (i in 1 until 10) {
        println(MessageController.countMessagesFromChatAndUser(i, i))
    }
}