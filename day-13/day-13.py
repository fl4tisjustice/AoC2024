from __future__ import annotations
from typing import Self
from functools import cache
from math import gcd
import re, numpy as np

class Vector2D:
    def __init__(self: Self, x: int, y: int) -> Vector2D:
        self.x: int = x;
        self.y: int = y;
    
    def __add__(self: Self, other: Vector2D) -> Vector2D:
        return Vector2D(self.x + other.x, self.y + other.y)
    
    def __sub__(self: Self, other: Vector2D) -> Vector2D:
        return Vector2D(self.x - other.x, self.y - other.y)

    def __eq__(self : Self, other: Vector2D) -> bool:
        return self.x == other.x and self.y == other.y
    
    def __mul__(self : Self, scalar : int):
        return Vector2D(self.x * scalar, self.y * scalar)

    def __div__(self: Self, scalar: int):
        return Vector2D(self.x // scalar, self.y // scalar)

    def __hash__(self : Self) -> int:
        return hash((self.x, self.y))
    
@cache
def min_tokens_to_prize(a: Vector2D, b: Vector2D, prize: Vector2D) -> int:
    if prize.x < 0 or prize.y < 0: return float('inf')
    if prize.x == 0 and prize.y == 0: return 0
    return min(3 + min_tokens_to_prize(a, b, prize - a), 1 + min_tokens_to_prize(a, b, prize - b))


def get_input() -> list[tuple[Vector2D, Vector2D, Vector2D]]:
    with open("day-13/input.txt", "r") as file:
        contents = file.readlines()

    claw_machines = list()
    for i in range(0, len(contents), 4):
        regex = re.compile(r"Button A: X([+-]\d+), Y+([+-]\d+)\nButton B: X([+-]\d+), Y+([+-]\d+)\nPrize: X=(\d+), Y=(\d+)")
        matches = regex.search(''.join(contents[i : i + 4]))
        a = Vector2D(int(matches.group(1)), int(matches.group(2)))
        b = Vector2D(int(matches.group(3)), int(matches.group(4)))
        prize = Vector2D(int(matches.group(5)), int(matches.group(6)))
        claw_machines.append((a, b, prize))

    return claw_machines

def part_one(claw_machines: list[tuple[Vector2D, Vector2D, Vector2D]]) -> int:
    accum = 0
    for machine in claw_machines:
        tokens = min_tokens_to_prize(*machine)
        accum += tokens if tokens != float('inf') else 0
    return accum

def part_two(claw_machines: list[tuple[Vector2D, Vector2D, Vector2D]]):
    accum = 0
    for machine in claw_machines:
        a, b, prize = machine
        prize += Vector2D(10000000000000, 10000000000000)

        A = np.array([[a.x, b.x], [a.y, b.y]])
        B = np.array([prize.x, prize.y])
        X = np.linalg.solve(A, B)
        a_presses, b_presses = X.round().astype(int)

        if Vector2D(a_presses * a.x + b_presses * b.x, a_presses * a.y + b_presses * b.y) != prize: continue
        accum += a_presses * 3 + b_presses
    return accum

def main() -> None:
    claw_machines = get_input()
    print(f"Part One: { part_one(claw_machines) }\nPart Two: { part_two(claw_machines) }")

# Button A: X+94, Y+34
# Button B: X+22, Y+67
# Prize: X=8400, Y=5400



if __name__ == "__main__":
    main()