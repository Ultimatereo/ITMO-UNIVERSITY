
interface Point: DimentionAware

/**
 * Реализация Point по умолчаению
 *
 * Должны работать вызовы DefaultPoint(10), DefaultPoint(12, 3), DefaultPoint(12, 3, 12, 4, 56)
 * с любым количество параметров
 *
 * Сама коллекция параметров недоступна, доступ - через методы интерфейса
 */
class DefaultPoint(private vararg val coords: Int): Point {
    override val ndim: Int
        get() = coords.size

    override fun dim(i: Int): Int {
        return coords[i]
    }
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as DefaultPoint

        if (!coords.contentEquals(other.coords)) return false

        return true
    }

    override fun hashCode(): Int {
        return coords.contentHashCode()
    }

}