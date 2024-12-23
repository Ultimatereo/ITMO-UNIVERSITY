/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author : Sultanov Mirzomansurkhon
 */
class Solution : MonotonicClock {
    private var l1 by RegularInt(0)
    private var l2 by RegularInt(0)
    private var r by RegularInt(0)

    override fun write(time: Time) {
        // write right-to-left
        val ll = time.d1
        val rr = time.d2 * 10_000 + time.d3
        l2 = ll
        r = rr
        l1 = ll
    }

    override fun read(): Time {
        // read left-to-right
        val v1 = l1
        val w = r
        val v2 = l2
        if (v1 == v2) {
            return Time(v2, w / 10_000, w % 10_000)
        }
        return Time(v2, 0, 0)
    }
}