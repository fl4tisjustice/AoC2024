from __future__ import annotations
from typing import Self
from enum import IntEnum
from collections import defaultdict
from math import sqrt
import heapq
import re

class Vector2D:
    def __init__(self: Self, x: int, y: int) -> Vector2D:
        self.x: int = x;
        self.y: int = y;
    
    def __add__(self: Self, other: Vector2D) -> Vector2D:
        return Vector2D(self.x + other.x, self.y + other.y)
    
    def __sub__(self: Self, other: Vector2D) -> Vector2D:
        return Vector2D(self.x - other.x, self.y - other.y)
    
    def __mul__(self: Self, scalar: int) -> Vector2D:
        return Vector2D(self.x * scalar, self.y * scalar)

    def __eq__(self: Self, other: Vector2D) -> bool:
        return self.x == other.x and self.y == other.y
    
    def __hash__(self: Self) -> int:
        return hash((self.x, self.y))

    def __lt__(self: Self, other: Vector2D) -> bool:
        return sqrt(self.x ** 2 + self.y ** 2) < sqrt(other.x ** 2+ other.y ** 2)
    
    def __repr__(self: Self) -> str:
        return f"({self.x}, {self.y})"

class Direction(IntEnum):
    UP = 0,
    LEFT = 1
    DOWN = 2,
    RIGHT = 3

    @staticmethod
    def to_vector2D(direction: Self) -> Vector2D:
        match (direction):
            case Direction.UP: return Vector2D(0, -1)
            case Direction.DOWN: return Vector2D(0, 1)
            case Direction.LEFT: return Vector2D(-1, 0)
            case Direction.RIGHT: return Vector2D(1, 0)

def get_cell(board: list[list[str]], unique: str) -> Vector2D:
    for row, line in enumerate(board):
        for col, cell in enumerate(line):
            if cell == unique: return Vector2D(col, row)


def dijkstra(board: list[list[str]], from_cell: Vector2D) -> defaultdict[Vector2D, int]:
    dist: defaultdict[Vector2D, int] = defaultdict(lambda: float('inf'))

    queue = [(0, from_cell)]
    dist[(from_cell)] = 0
    
    while len(queue):
        _, cell = heapq.heappop(queue)
        
        for next_cell in map(lambda x : x[0] + x[1], zip(map(Direction.to_vector2D, map(Direction, range(4))), [cell] * 4)):
            if next_cell.x < 0 or next_cell.x >= len(board[0]) or next_cell.y < 0 or next_cell.y >= len(board): continue
            if board[next_cell.y][next_cell.x] == '#': continue
            
            cmp: int = dist[cell] + 1

            if cmp < dist[next_cell]:
                dist[next_cell] = cmp
                heapq.heappush(queue, (cmp ,next_cell))

    return dist

def get_input() -> list[list[str]]:
    with open("day-20/input.txt") as file:
        return [list(line) for line in file.read().split('\n')]

def transpose(board: list[list[str]]) -> list[list[str]]:
    transposed: list[list[str]] = []
    for col in range(len(board[0])):
        transposed.append([])
        for row in range(len(board)):
            transposed[col].append(board[row][col])
    return transposed

def part_one(board: list[str]) -> int:
    start: Vector2D = get_cell(board, 'S')

    dist: defaultdict[Vector2D, int] = dijkstra(board, start)

    exploitable: list[tuple[Vector2D, int, bool]] = []
    pattern: re.Pattern[str] = re.compile(r"[.ES]#{1,2}(?=[.ES])")

    for row, line in enumerate(board):
        for match in re.finditer(pattern, ''.join(line)):
            exploitable.append((Vector2D(match.span()[0], row), match.span()[1] - match.span()[0], False))

    for col, line in enumerate(transpose(board)):
        for match in re.finditer(pattern, ''.join(line)):
            exploitable.append((Vector2D(col, match.span()[0]), match.span()[1] - match.span()[0], True))

    timesaves = defaultdict(lambda: 0)

    for pos, delta, transposed in exploitable:
        timesaves[abs(dist[pos + Direction.to_vector2D(Direction.DOWN if transposed else Direction.RIGHT) * delta] - dist[pos]) - delta] += 1

    return sum(map(lambda x : x[1], filter(lambda x : x[0] >= 100, timesaves.items())))


def main() -> None:
    board: list[list[str]] = get_input()
    print(f"Part One: { part_one(board) }")

if __name__ == "__main__":
    main()