package binomial

/*
 * BinomialHeap - реализация биномиальной кучи
 *
 * https://en.wikipedia.org/wiki/Binomial_heap
 *
 * Запрещено использовать
 *
 *  - var
 *  - циклы
 *  - стандартные коллекции
 *
 * Детали внутренней реализации должны быть спрятаны
 * Создание - только через single() и plus()
 *
 * Куча совсем без элементов не предусмотрена
 *
 * Операции
 *
 * plus с кучей
 * plus с элементом
 * top - взятие минимального элемента
 * drop - удаление минимального элемента
 *
 * Инвариант
 * Кучу будем хранить в порядке возрастания степеней. В head находится минимальная степень, а в самом конце максимальная
 *
 */
class BinomialHeap<T : Comparable<T>> private constructor(val trees: FList<BinomialTree<T>>) :
    SelfMergeable<BinomialHeap<T>> {
    companion object {
        fun <T : Comparable<T>> single(value: T): BinomialHeap<T> =
            BinomialHeap(FList.Cons(BinomialTree.single(value), FList.Nil()))
    }

    /*
     * слияние куч
     *
     * Требуемая сложность - O(log(n))
     */

    private tailrec fun plusImplFirstCycle(
        first: FList<BinomialTree<T>>, second: FList<BinomialTree<T>>,
        result: FList<BinomialTree<T>> = FList.nil()
    ): FList<BinomialTree<T>> {
        if (first.isEmpty && second.isEmpty) {
            return result.reverse()
        }
        val first1: FList<BinomialTree<T>>
        val second1: FList<BinomialTree<T>>
        val result1: FList<BinomialTree<T>>
        if (first.isEmpty) {
            first1 = first
            second1 = (second as FList.Cons).tail
            result1 = FList.Cons(second.head, result)
        } else if (second.isEmpty) {
            first1 = (first as FList.Cons).tail
            second1 = second
            result1 = FList.Cons(first.head, result)
        } else if ((first as FList.Cons).head.order <
            (second as FList.Cons).head.order
        ) {
            first1 = first.tail
            second1 = second
            result1 = FList.Cons(first.head, result)
        } else {
            first1 = first
            second1 = second.tail
            result1 = FList.Cons(second.head, result)
        }
        return plusImplFirstCycle(
            first1,
            second1,
            result1
        )
    }

    private tailrec fun plusImplSecondCycle(
        heap: FList<BinomialTree<T>>,
        result: FList<BinomialTree<T>> = FList.nil(),
        resultOrder: Int = -1
    ): FList<BinomialTree<T>> {
        if (heap is FList.Nil) {
            return result
        }
        val heap1: FList<BinomialTree<T>>
        val result1: FList<BinomialTree<T>>
        val resultOrder1: Int
        heap as FList.Cons
        if (heap.tail is FList.Cons && heap.head.order == heap.tail.head.order) {
            heap1 = heap.tail.tail
            result1 = FList.Cons(heap.head + heap.tail.head, result)
            resultOrder1 = heap.head.order + 1
        } else if (result is FList.Cons && heap.head.order == resultOrder) {
            heap1 = heap.tail
            result1 = FList.Cons(heap.head + result.head, result.tail)
            resultOrder1 = resultOrder + 1
        } else if (heap.tail is FList.Cons && result is FList.Cons && heap.tail.head.order == resultOrder) {
            heap1 = FList.Cons(heap.head, heap.tail.tail)
            result1 = FList.Cons(heap.tail.head + result.head, result.tail)
            resultOrder1 = resultOrder + 1
        } else {
            heap1 = heap.tail
            result1 = FList.Cons(heap.head, result)
            resultOrder1 = heap.head.order
        }
        return plusImplSecondCycle(heap1, result1, resultOrder1)
    }

    override fun plus(other: BinomialHeap<T>): BinomialHeap<T> =
        BinomialHeap(plusImplSecondCycle(plusImplFirstCycle(this.trees, other.trees)).reverse())

    /*
     * добавление элемента
     *
     * Требуемая сложность - O(log(n))
     */
    operator fun plus(elem: T): BinomialHeap<T> =
        this + BinomialHeap(flistOf(BinomialTree.single(elem)))

    /*
     * минимальный элемент
     *
     * Требуемая сложность - O(log(n))
     */
    fun top(): T = getMinTree().value

    /*
     * удаление элемента
     *
     * Требуемая сложность - O(log(n))
     */
    private fun getMinTree(): BinomialTree<T> =
        this.trees.fold((this.trees as FList.Cons<BinomialTree<T>>).head)
        { acc, current ->
            if (acc.value < current.value) {
                acc
            } else {
                current
            }
        }

    private fun getTreeWithoutMinTree(minTree: BinomialTree<T> = getMinTree()): FList<BinomialTree<T>> =
        trees.filter { tree -> tree != minTree }

    fun drop(): BinomialHeap<T> {
        val minTree = getMinTree()
        val treeWithoutMinTree = getTreeWithoutMinTree(minTree)
        return BinomialHeap(treeWithoutMinTree) + BinomialHeap(minTree.children.reverse())
    }
}

