from __future__ import annotations
from typing import Self
from enum import IntEnum
from collections import defaultdict
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

def get_input() -> list[Vector2D]:
    with open("day-18/input.txt", 'r') as file:
        return [Vector2D(*map(int, coord.split(','))) for coord in file.readlines()]

def part_one(byte_list: list[Vector2D]) -> int:
    board: list[list[str]] = [['.' for _ in range(71)] for _ in range(71)]
    for byte in byte_list[:1024]: board[byte.y][byte.x] = '#'
    dist = dijkstra(board, Vector2D(0, 0))
    return dist[Vector2D(70, 70)]    

def part_two(byte_list: list[Vector2D]) -> Vector2D:
    left: int = 0;
    right: int = len(byte_list)
    mid: int

    while left < right:
        mid = left + (right - left) // 2
        board: list[list[str]] = [['.' for _ in range(71)] for _ in range(71)]
        for byte in byte_list[:mid]: board[byte.y][byte.x] = '#'
        dist = dijkstra(board, Vector2D(0, 0))
        if dist[Vector2D(70, 70)] == float('inf'):
            right = mid
        else:
            left = mid + 1

    return byte_list[left - 1]

def main() -> None:
    byte_list: list[Vector2D] = get_input()
    print(f"Part One: { part_one(byte_list) }\nPart Two: { part_two(byte_list) }");

if __name__ == "__main__":
    main()