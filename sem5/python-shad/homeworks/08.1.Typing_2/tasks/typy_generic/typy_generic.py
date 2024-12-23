import typing as tp

T = tp.TypeVar('T', int, float)


class Pair(tp.Generic[T]):
    def __init__(self, a: T, b: T) -> None:
        self._a: T = a
        self._b: T = b

    def sum(self) -> T:
        return self._a + self._b

    def first(self) -> T:
        return self._a

    def second(self) -> T:
        return self._b

    def __iadd__(self, pair: 'Pair[T]') -> 'Pair[T]':
        self._a += pair.first()
        self._b += pair.second()
        return self
