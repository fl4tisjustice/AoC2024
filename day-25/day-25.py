from functools import partial
from itertools import starmap, product
from operator import add

def transpose(element: list[str]) -> list[str]:
    transposed: list[list[str]] = []
    for col in range(len(element[0])):
        transposed.append([])
        for row in range(len(element)):
            transposed[col].append(element[row][col])
    return list(map(''.join, transposed))

def part_one(locks: list[tuple[int]], keys: list[tuple[int]]) -> int:
    return len(list(filter(lambda match: all(pin < 6 for pin in match), map(partial(starmap, add), starmap(zip, product(locks, keys))))))

def get_input() -> tuple[tuple[int], tuple[int]]:
    with open("day-25/input.txt", 'r') as file:
        locks_and_keys = list(map(partial(str.split, sep='\n'), file.read().split("\n\n")))

    locks: list[tuple[int]] = []
    keys: list[tuple[int]] = []

    for element in locks_and_keys:
        transposed: list[str] = transpose(element)
        if '#' in element[0]: locks.append(tuple(map(lambda pin: pin.rfind('#'), transposed)))
        else: keys.append(tuple(map(lambda pin: len(pin) - pin.find('#') - 1, transposed)))

    return locks, keys

def main() -> None:
    locks, keys = get_input()
    print(f"Part One: { part_one(locks, keys) }")

if __name__ == "__main__":
    main()