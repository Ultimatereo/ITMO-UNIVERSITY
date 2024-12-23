package mutex

import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Mirzomansurkhon Sultanov
 */
class ProcessImpl(private val env: Environment) : Process {
    private enum class State {
        THINKING, HUNGRY, EATING
    }

    private enum class MsgType {
        FORK, REQUEST
    }


    // fork[j] => We hold the fork that is shared with process j
    private val fork = BooleanArray(env.nProcesses + 1) { it <= env.processId }

    // dirty[j] => The fork that is shared with process j is dirty
    private val dirty = BooleanArray(env.nProcesses + 1) { true }

    // request[j] => We hold the request token for the fork that is shared with process j
    // Basically means that I can ask j questions, and they can't do that yet.
    private val request = BooleanArray(env.nProcesses + 1) { it > env.processId }
    private var state = State.THINKING
    private val lock = ReentrantLock()

    override fun onMessage(srcId: Int, message: Message) {
        lock.withLock {
            message.parse {
                val type = readEnum<MsgType>()
                when (type) {
                    MsgType.REQUEST -> {
                        request[srcId] = true
                        if (state != State.EATING && fork[srcId] && dirty[srcId]) {
                            send(srcId, MsgType.FORK)
                            fork[srcId] = false
                            if (state == State.HUNGRY) {
                                send(srcId, MsgType.REQUEST)
                                request[srcId] = false
                            }
                        }
                    }

                    MsgType.FORK -> {
                        fork[srcId] = true
                        dirty[srcId] = false
                        if (haveForks()) {
                            state = State.EATING
                            env.locked()
                        }
                    }
                }
            }
        }
    }

    override fun onLockRequest() {
        lock.withLock {
            state = State.HUNGRY
            if (haveForks()) {
                state = State.EATING
                env.locked()
                return
            }
            for (i in 1..env.nProcesses) {
                if (request[i] && !fork[i]) {
                    send(i, MsgType.REQUEST)
                    request[i] = false
                }
            }
        }
    }

    private fun haveForks(): Boolean {
        for (i in 1..env.nProcesses) {
            if (!fork[i]) return false
        }
        return true
    }

    private fun send(destId: Int, type: MsgType) {
        env.send(destId) {
            writeEnum(type)
        }
    }

    override fun onUnlockRequest() {
        lock.withLock {
            state = State.THINKING
            env.unlocked()
            for (i in 1..env.nProcesses) {
                dirty[i] = true
                if (request[i]) {
                    send(i, MsgType.FORK)
                    fork[i] = false
                }
            }
        }
    }
}
