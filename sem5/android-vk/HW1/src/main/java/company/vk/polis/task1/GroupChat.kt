package company.vk.polis.task1

data class GroupChat(
    val groupChatId: Int?,
    override val userIds: List<Int?>?,
    override val messageIds: List<Int?>?,
    val avatarUrl: String?,
) : ChatInterface {
    override fun getId(): Int? {
        return groupChatId
    }
}
