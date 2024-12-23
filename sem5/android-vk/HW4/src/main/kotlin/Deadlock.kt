import java.util.concurrent.locks.ReentrantLock
class Deadlock {
    class DeadlockExample {
        private val lock1 = ReentrantLock()
        private val lock2 = ReentrantLock()

        fun executeThread1() {
            lock1.lock()
            System.err.println("Thread 1: Locks lock1")
            Thread.sleep(1000)

            lock2.lock()
            System.err.println("Thread 1: Tries to lock lock2")
            lock2.unlock()
            lock1.unlock()
        }

        fun executeThread2() {
            lock2.lock()
            System.err.println("Thread 2: Locks lock2")
            Thread.sleep(1000)

            lock1.lock()
            System.err.println("Thread 2: Tries to lock lock1")
            lock1.unlock()
            lock2.unlock()
        }
    }
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            val deadlockExample = DeadlockExample()

            val thread1 = Thread { deadlockExample.executeThread1() }
            val thread2 = Thread { deadlockExample.executeThread2() }

            thread1.start()
            thread2.start()

            thread1.join()
            thread2.join()
        }
    }
}