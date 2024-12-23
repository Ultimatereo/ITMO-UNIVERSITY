import kotlinx.coroutines.async
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking

fun main() {
    val a = runBlocking {
        launch {printSecond()}
        launch { printFirst() }
        3
    }
    println(a)
}

suspend fun printFirst() {
    for (i in 0..10) {
        delay(5)
        println("printFirst - $i")
    }
}

suspend fun printSecond() {
    for (i in 0..10) {
        delay(5)
        println("printSecond - $i")
    }
}