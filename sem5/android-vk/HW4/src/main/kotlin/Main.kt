fun main(args: Array<String>) {
    println("Hello threads")

    println("Runnable and Thread")
    RunnableAndThread.main(Array(0) {""})
    println("------------")

    println("Callable and Future")
    CallableAndFuture.main(Array(0) {""})
    println("------------")

    println("Synchronization")
    Synchonization.main(Array(0) {""})
    println("------------")

    println("Runnable, Thread, join()")
    RunnableAndThreadAdv.main(Array(0) {""})
    println("------------")

    println("Deadlock")
    Deadlock.main(Array(0) {""})
    println("------------")
}