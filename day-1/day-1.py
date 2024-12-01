from functools import reduce
from math import prod

def get_input() -> tuple[list[int], list[int]]:
    with open(f"day-1/input.txt", 'r') as file:
        lines = file.readlines()
        return reduce(lambda prev, curr : (prev[0] + [curr[0]], prev[1] + [curr[1]]), map(lambda line : tuple(map(int, line.split("   "))), lines), [[],[]])

def part_one() -> int:
    left, right = get_input()
    return sum(map(lambda mins : abs(mins[0] - mins[1]), zip(sorted(left), sorted(right))))

def part_two() -> int:
    left, right = get_input()
    return sum(map(prod, zip(left, [right.count(value) for value in left])))

if __name__ == "__main__":
    print(f"Part One: { part_one() }\nPart Two: { part_two() }")