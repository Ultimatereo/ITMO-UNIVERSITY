import typing as tp
from collections import UserList


class ListTwist(UserList[tp.Any]):
    """
    List-like class with additional attributes:
        * reversed, R - return reversed list
        * first, F - insert or retrieve first element;
                     Undefined for empty list
        * last, L -  insert or retrieve last element;
                     Undefined for empty list
        * size, S -  set or retrieve size of list;
                     If size less than list length - truncate to size;
                     If size greater than list length - pad with Nones
    """

    def __setattr__(self, __name: str, __value: tp.Any) -> None:
        if __name == 'first' or __name == 'F':
            self.data[0] = __value
        if __name == 'last' or __name == 'L':
            self.data[-1] = __value
        if __name == 'size' or __name == 'S':
            new_data = [None for _ in range(__value)]
            for i in range(min(__value, len(self.data))):
                new_data[i] = self.data[i]
            self.data = new_data
        super().__setattr__(__name, __value)

    def __getattribute__(self, __name: str) -> tp.Any:
        if __name == 'reversed' or __name == 'R':
            return self.data[::-1]
        if __name == 'first' or __name == 'F':
            return self.data[0]
        if __name == 'last' or __name == 'L':
            return self.data[-1]
        if __name == 'size' or __name == 'S':
            return len(self.data)
        return super().__getattribute__(__name)
