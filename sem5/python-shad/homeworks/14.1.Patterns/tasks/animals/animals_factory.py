import typing as tp

from .animals import Cat, Cow, Dog

class Animal:
    pass


class CatAdapter:
    pass


class DogAdapter:
    pass


class CowAdapter:
    pass


def animals_factory(animal: tp.Any) -> Animal:
    pass