from abc import ABC, abstractmethod
from dataclasses import dataclass, field, InitVar

DISCOUNT_PERCENTS = 15


@dataclass(init=True, order=True, unsafe_hash=True, frozen=True)
class Item:
    item_id: int = field(compare=False)
    title: str
    cost: int

    def __post_init__(self):  # type: ignore
        assert self.title != ''
        assert self.cost > 0


@dataclass
class Position(ABC):
    item: Item

    @property
    @abstractmethod
    def cost(self) -> float:
        pass


@dataclass
class CountedPosition(Position):
    count: int = 1

    @property
    def cost(self) -> float:
        return self.count * self.item.cost


@dataclass
class WeightedPosition(Position):
    weight: float = 1.0

    @property
    def cost(self) -> float:
        return self.weight * self.item.cost


@dataclass
class Order:
    order_id: int
    positions: list[Position] = field(default_factory=list)
    cost: int = field(init=False)
    have_promo: InitVar[bool] = False

    def __post_init__(self, have_promo):  # type: ignore
        total_cost = sum(position.cost for position in self.positions)
        if have_promo:
            discount = total_cost * (DISCOUNT_PERCENTS / 100)
            self.cost = int(total_cost - discount)
        else:
            self.cost = int(total_cost)
