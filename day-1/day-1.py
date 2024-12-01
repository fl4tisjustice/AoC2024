from functools import reduce

def get_input() -> list[str]:
    with open("day-1/input.txt", 'r') as file:
        return file.readlines()

def solve() -> int:
    data = get_input()
    left, right = reduce(lambda prev, curr : (prev[0] + [curr[0]], prev[1] + [curr[1]]), map(lambda line : tuple(map(int, line.split("   "))), data), [[],[]]) 
    return sum(map(lambda mins : abs(mins[0] - mins[1]), zip(sorted(left), sorted(right))))

if __name__ == "__main__":
    print(solve())