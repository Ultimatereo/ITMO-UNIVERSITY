package company.vk.polis.task1

sealed class State {
    data object Read : State()
    data object Unread : State()
    data class Deleted(val userId: Int?) : State()
}

