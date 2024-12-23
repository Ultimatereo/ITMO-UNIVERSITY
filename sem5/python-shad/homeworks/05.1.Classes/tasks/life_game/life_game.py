from copy import deepcopy


class LifeGame(object):
    """
    Class for Game life
    """

    def __init__(self, board: list[list[int]]):
        self.board = board
        self.n = len(board)
        self.m = len(board[0])

    def _get_neighbours(self, x: int, y: int) -> dict[int, int]:
        neighbours: dict[int, int] = {0: 0, 1: 0, 2: 0, 3: 0}
        for dx in [-1, 0, 1]:
            for dy in [-1, 0, 1]:
                if dx == 0 and dy == 0:
                    continue
                if x + dx < 0 or x + dx >= self.n or y + dy < 0 or y + dy >= self.m:
                    continue
                neighbours[self.board[x + dx][y + dy]] += 1
        return neighbours

    def get_next_generation(self) -> list[list[int]]:
        next_gen: list[list[int]] = deepcopy(self.board)
        for x in range(self.n):
            for y in range(self.m):
                if self.board[x][y] == 1:
                    continue
                neighbours = self._get_neighbours(x, y)
                if self.board[x][y] == 0:
                    for creature in [2, 3]:
                        if neighbours[creature] == 3:
                            next_gen[x][y] = creature
                            break
                else:
                    for creature in [2, 3]:
                        if self.board[x][y] == creature:
                            if neighbours[creature] > 3 or neighbours[creature] < 2:
                                next_gen[x][y] = 0
                            break
        self.board = next_gen
        return self.board
