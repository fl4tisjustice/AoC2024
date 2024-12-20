from functools import partial, cache

def get_input() -> tuple[tuple[str], list[str]]:
    with open("day-19/input.txt", 'r') as file:
        towels, patterns = file.read().split("\n\n")
    return tuple(towels.split(", ")), patterns.split('\n')

@cache
def buildable_from(target: str, available: list[str]) -> int:
    if len(target) == 0: return 1

    ways: int = 0
    for idx in range(1, len(target) + 1):
        ways += int(target[:idx] in available and buildable_from(target[idx:], available))
    return ways

def main():
    towels, patterns = get_input()
    solution: list[int] = list(map(partial(buildable_from, available=towels), patterns))
    print(f"Part One: { len(list(filter(lambda x : x > 0, solution))) }\nPart Two: { sum(solution) }")

if __name__ == "__main__":
    main()