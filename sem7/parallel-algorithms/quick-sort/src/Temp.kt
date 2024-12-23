open class ParentClass {
    open val age = 10
    protected val name = "Bob"

    protected class Nested {
        val friendName: String = "Alice"
    }
}

class ChildClass : ParentClass() {
    override val age = 25

    fun tryIt() {
        println("$name is $age")
        println("${Nested().friendName} friend for $name")
    }

}

fun main() {
    ChildClass().tryIt()
}