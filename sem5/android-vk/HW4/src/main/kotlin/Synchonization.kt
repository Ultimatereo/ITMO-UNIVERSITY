class Synchonization {
    class Counter {
        var count = 0

        @Synchronized
        fun increment() {
            count++
        }

        fun unSyncIncrement() {
            count++
        }
    }
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            for (t in 0 until 10) {
                System.err.println("Test ${t + 1}")
                val counter = Counter()
                val thread1 = Thread {
                    for (i in 1..1000) {
                        counter.increment()
                    }
                }
                val thread2 = Thread {
                    for (i in 1..1000) {
                        counter.increment()
                    }
                }
                val thread3 = Thread {
                    for (i in 1..1000) {
                        counter.increment()
                    }
                }

                thread1.start()
                thread2.start()
                thread3.start()

                thread1.join()
                thread2.join()
                thread3.join()

                System.err.println("Synchronized counter: ${counter.count}")

                val unsynchronizedCounter = Counter()
                val unsynchronizedThread1 = Thread {
                    for (i in 1..1000) {
                        unsynchronizedCounter.unSyncIncrement()
                    }
                }
                val unsynchronizedThread2 = Thread {
                    for (i in 1..1000) {
                        unsynchronizedCounter.unSyncIncrement()
                    }
                }
                val unsynchronizedThread3 = Thread {
                    for (i in 1..1000) {
                        unsynchronizedCounter.unSyncIncrement()
                    }
                }

                unsynchronizedThread1.start()
                unsynchronizedThread2.start()
                unsynchronizedThread3.start()

                unsynchronizedThread1.join()
                unsynchronizedThread2.join()
                unsynchronizedThread3.join()

                System.err.println("Unsynchronized counter: ${unsynchronizedCounter.count}")
            }
        }
    }
}