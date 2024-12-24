from itertools import combinations, product
from collections import defaultdict
from operator import and_
from functools import reduce

def get_input() -> list[tuple[str, str]]:
    with open("day-23/input.txt", 'r') as file:
        return [tuple(line.split('-')) for line in file.read().split('\n')]

def find_in_threes(connections: list[tuple[str, str]]) -> int:
    connections = list(map(lambda p: tuple(sorted(p)), connections))
    computers: set[str] = set(sum(connections, tuple()))
    interconnected: set[tuple[str, str, str]] = set()

    for computer in computers:
        connected: set[str] = set(sum(filter(lambda p: computer in p, connections), tuple())) - { computer }
        interconnected |= set(
            map(lambda p: tuple(sorted(p + (computer,))),   # resort and insert so no duplicates are created
            filter(lambda p: p in connections,              # only process existing connections (hence the sorting)
            map(lambda p: tuple(sorted(p)),                 # sort element in each 2-tuple of computers connected
            combinations(connected, 2))))                   # n choose 2 for each of the computers connected to computer
        )

    return len(list(filter(lambda t: any(c.startswith('t') for c in t), interconnected)))

def find_largest_interconnected(connections: list[tuple[str, str]]) -> str:
    connections = { *map(lambda p: tuple(sorted(p)), connections) }
    computers: set[tuple[str]] = set(map(lambda c: (c,), sum(connections, tuple())))
    
    table: defaultdict[int, set[tuple[str]]] = defaultdict(set)
    table[1] = computers
    table[2] = connections

    length: int = 2

    while length in table:
        length += 1
        for longest, insert in product(table[length - 1], table[1]):
            if not set(product(longest, insert)).issubset(table[2]): continue
            table[length].add(tuple(sorted(longest + insert)))

    return ','.join(table[length - 1].pop())

def main() -> None:
    connections: list[tuple[str, str]] = get_input()
    print(f"Part One: { find_in_threes(connections) }\nPart Two: { find_largest_interconnected(connections) }")

if __name__ == "__main__":
    main()