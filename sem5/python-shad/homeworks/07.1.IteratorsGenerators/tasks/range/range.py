from collections.abc import Iterable, Iterator, Sized


class RangeIterator(Iterator[int]):
    """The iterator class for Range"""

    def __init__(self, range_: 'Range') -> None:
        self.range_ = range_
        self.current = range_.start

    def __iter__(self) -> 'RangeIterator':
        return self

    def __next__(self) -> int:
        if self.current >= self.range_.stop and self.range_.step > 0 or \
                self.current <= self.range_.stop and self.range_.step < 0:
            raise StopIteration
        else:
            value = self.current
            self.current += self.range_.step
            return value


class Range(Sized, Iterable[int]):
    """The range-like type, which represents an immutable sequence of numbers"""

    def __init__(self, *args: int) -> None:
        """
        :param args: either it's a single `stop` argument
            or sequence of `start, stop[, step]` arguments.
        If the `step` argument is omitted, it defaults to 1.
        If the `start` argument is omitted, it defaults to 0.
        If `step` is zero, ValueError is raised.
        """
        if len(args) == 1:
            self.start = 0
            self.stop = args[0]
            self.step = 1
        elif len(args) == 2:
            self.start = args[0]
            self.stop = args[1]
            self.step = 1
        elif len(args) == 3:
            self.start = args[0]
            self.stop = args[1]
            self.step = args[2]
        else:
            raise ValueError("Range accepts 1 to 3 arguments")

        if self.step == 0:
            raise ValueError("step cannot be zero")

    def __iter__(self) -> 'RangeIterator':
        return RangeIterator(self)

    def __repr__(self) -> str:
        if self.step == 1:
            return f"range({self.start}, {self.stop})"
        return f"range({self.start}, {self.stop}, {self.step})"

    def __str__(self) -> str:
        if self.step == 1:
            return f"range({self.start}, {self.stop})"
        return f"range({self.start}, {self.stop}, {self.step})"

    def __contains__(self, key: int) -> bool:
        if self.step > 0:
            return self.start <= key < self.stop and (key - self.start) % self.step == 0
        elif self.step < 0:
            return self.stop < key <= self.start and (self.start - key) % (-self.step) == 0
        else:
            return False

    def __getitem__(self, key: int) -> int:
        if key < 0:
            key += len(self)
        if key < 0 or key >= len(self):
            raise IndexError("Index out of range")
        return self.start + key * self.step

    def __len__(self) -> int:
        if self.step > 0:
            return max(0, (self.stop - self.start + self.step - 1) // self.step)
        elif self.step < 0:
            return max(0, (self.start - self.stop + (-self.step) - 1) // (-self.step))
        else:
            return 0
