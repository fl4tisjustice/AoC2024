from __future__ import annotations
from typing import Self
from enum import IntEnum
from collections import defaultdict, deque
from functools import reduce
from math import sqrt
import heapq

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

def dijkstra(board: list[list[str]], from_cell: Vector2D, from_direction: Direction) -> \
    tuple[defaultdict[(Vector2D, Direction), int], defaultdict[(Vector2D, Direction), set[(Vector2D, Direction)]]]:

    dist: defaultdict[(Vector2D, Direction), int] = defaultdict(lambda: float('inf'))
    prev: defaultdict[(Vector2D, Direction), set[(Vector2D, Direction)]] = defaultdict(set)

    queue = [(0, from_cell, from_direction)]
    dist[(from_cell, from_direction)] = 0
    
    while len(queue):
        _, cell, direction = heapq.heappop(queue)
        board[cell.y][cell.x] = 'X'

        clockwise: Direction = Direction((direction - 1) % 4)
        counter_clockwise: Direction = Direction((direction + 1) % 4)

        cell_ahead: Vector2D = cell + Direction.to_vector2D(direction)
        clockwise_cell: Vector2D = cell + Direction.to_vector2D(clockwise)
        counter_clockwise_cell: Vector2D = cell + Direction.to_vector2D(counter_clockwise)

        for next_cell, next_direction in [(cell_ahead, direction), (clockwise_cell, clockwise), (counter_clockwise_cell, counter_clockwise)]:
            if board[next_cell.y][next_cell.x] == '#': continue
            
            cmp_score = dist[(cell, direction)] + 1 + int(direction != next_direction) * 1000

            if cmp_score <= dist[(next_cell, next_direction)]:
                prev[(next_cell, next_direction)] |= { (cell, direction) }
                if cmp_score == dist[(next_cell, next_direction)]: continue
                dist[(next_cell, next_direction)] = cmp_score
                heapq.heappush(queue, (cmp_score, next_cell, next_direction))

    return dist, prev

def unique_path_cells(prev: defaultdict[(Vector2D, Direction), set[(Vector2D, Direction)]], end: Vector2D, direction: Direction):
    queue: deque[Vector2D] = deque()
    queue.append((end, direction))

    visited: set[(Vector2D, Direction)] = { (end, direction) }
    unique: set[Vector2D] = { end }
    
    while len(queue):
        cell, direction = queue.popleft()
        for prev_cell_dir in prev[(cell, direction)]:
            if prev_cell_dir in visited: continue
            visited.add(prev_cell_dir)
            unique.add(prev_cell_dir[0])
            queue.append(prev_cell_dir)

    return len(unique)

def get_input() -> list[list[str]]:
    with open("day-16/input.txt", "r") as file:
        contents = file.read().split("\n\n")
    return list(map(list, contents[0].split('\n')))

def main():
    board: list[list[str]] = get_input()

    start: Vector2D = get_cell(board, 'S')
    end: Vector2D = get_cell(board, 'E')

    dist, prev = dijkstra(board, start, Direction.RIGHT)
    _, direction = reduce(lambda accum, curr: accum if dist[accum] < dist[curr] else curr, zip((end,) * 4, map(Direction, range(0, 4))))
    
    print(f"Part One: { dist[(end, direction)] }\nPart Two: {unique_path_cells(prev, end, direction)}")

if __name__ == "__main__":
    main()