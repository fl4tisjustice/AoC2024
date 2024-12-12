from __future__ import annotations
from itertools import combinations
from typing import Generator, Self

class Vector2D:
    def __init__(self : Self, x : int, y : int):
        self.x : int = x;
        self.y : int = y;
    
    def __neg__(self : Self):
        return Vector2D(-self.x, -self.y)

    def __add__(self : Self, other : Vector2D):
        return Vector2D(self.x + other.x, self.y + other.y)
    
    def __sub__(self : Self, other):
        return Vector2D(self.x - other.x, self.y - other.y)

    def __mul__(self : Self, scalar : int):
        return Vector2D(self.x * scalar, self.y * scalar)

    def __eq__(self : Self, other):
        return self.x == other.x and self.y == other.y

    def __hash__(self : Self):
        return hash((self.x, self.y))

    def within(self : Self, width : int, height : int):
        return self.x >= 0 and self.x < width and self.y >= 0 and self.y < height    

def get_input() -> tuple[dict[str, list[Vector2D]], tuple[int, int]]:
    with open("day-8/input.txt", "r") as file:
        contents : list[str] = file.readlines()

    antennas = dict()
    for row, line in enumerate(contents):
        for col, cell in enumerate(line):
            if not cell.isalnum(): continue
            if cell not in antennas.keys():
                antennas[cell] = []
            antennas[cell].append(Vector2D(row, col))
    
    return (antennas, (len(contents[0]) - 1, len(contents)))

def resonance(coordinate : Vector2D, vector : Vector2D, dimensions : tuple[int, int]) -> Generator[Vector2D]:
    while coordinate.within(*dimensions):
        yield coordinate
        coordinate += vector

def count_antinodes(antennas : dict[str, list[Vector2D]], dimensions : tuple[int, int], *, resonance_active : bool = False) -> int:
    antinodes = set()
    for frequency in antennas.keys():
        for first, second in combinations(antennas[frequency], 2):
            vector = second - first
            if resonance_active:
                antinodes |= { *resonance(second, vector, dimensions), *resonance(first, -vector, dimensions) }
            else:
                antinodes |= set(filter(lambda v : v.within(*dimensions), { second + vector, first - vector }))
    return len(antinodes)

def main() -> None:
    antennas, dimensions = get_input()
    print(f"Part One: { count_antinodes(antennas, dimensions) }\nPart Two { count_antinodes(antennas, dimensions, resonance_active=True)}")

if __name__ == "__main__":
    main()