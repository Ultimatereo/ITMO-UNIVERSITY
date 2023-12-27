
interface Shape: DimentionAware, SizeAware

/**
 * Реализация Point по умолчаению
 *
 * Должны работать вызовы DefaultShape(10), DefaultShape(12, 3), DefaultShape(12, 3, 12, 4, 56)
 * с любым количество параметров
 *
 * При попытке создать пустой Shape бросается EmptyShapeException
 *
 * При попытке указать неположительное число по любой размерности бросается NonPositiveDimensionException
 * Свойство index - минимальный индекс с некорректным значением, value - само значение
 *
 * Сама коллекция параметров недоступна, доступ - через методы интерфейса
 */
class DefaultShape(private vararg val dimentions: Int): Shape {
    init {
        if (dimentions.isEmpty()) {
            throw ShapeArgumentException.EmptyShapeException()
        }
        val index = dimentions.indexOfFirst { dim -> dim <= 0 }
        if (index != -1) {
            throw ShapeArgumentException.NonPositiveDimensionException(index, dimentions[index])
        }
    }
    override val ndim: Int
        get() = dimentions.size

    override fun dim(i: Int): Int {
        return dimentions[i]
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as DefaultShape

        if (!dimentions.contentEquals(other.dimentions)) return false

        return true
    }

    override fun hashCode(): Int {
        return dimentions.contentHashCode()
    }

    override val size: Int by lazy {
        dimentions.reduce{acc, i -> acc * i }
    }
}

sealed class ShapeArgumentException (reason: String = "") : IllegalArgumentException(reason) {
    class EmptyShapeException : ShapeArgumentException("Empty Shape was tried to be initialized!")
    class NonPositiveDimensionException(index: Int, value: Int) :
        ShapeArgumentException("${index+1} dimension of the shape equals $value! But it should be a positive number!")
}
