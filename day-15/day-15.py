from __future__ import annotations
from typing import Self
from copy import deepcopy

class Vector2D:
    def __init__(self: Self, x: int, y: int) -> Vector2D:
        self.x: int = x;
        self.y: int = y;
    
    def __add__(self: Self, other: Vector2D) -> Vector2D:
        return Vector2D(self.x + other.x, self.y + other.y)
    
    def __sub__(self: Self, other: Vector2D) -> Vector2D:
        return Vector2D(self.x - other.x, self.y - other.y)
    
    def __mul__(self : Self, scalar: int) -> Vector2D:
        return Vector2D(self.x * scalar, self.y * scalar)

    def __eq__(self : Self, other: Vector2D) -> bool:
        return self.x == other.x and self.y == other.y
    
    def __hash__(self : Self) -> int:
        return hash((self.x, self.y))

def get_robot(board: list[list[str]]) -> Vector2D:
    width: int = len(board[0])
    height: int = len(board)

    for row in range(0, height):
        for col in range(0, width):
            if board[row][col] == '@':
                return Vector2D(col, row)

def get_direction(move: str) -> Vector2D:
    match move:
        case '^': return Vector2D( 0,-1)
        case 'v': return Vector2D( 0, 1)
        case '<': return Vector2D(-1, 0)
        case '>': return Vector2D( 1, 0)

def try_push(board: list[list[str]], check: Vector2D, toward: Vector2D) -> bool:
    next: Vector2D = check + toward
    if board[next.y][next.x] == '#': return False
    if board[next.y][next.x] == '.' or try_push(board, next, toward):
        board[next.y][next.x] = board[check.y][check.x]
        return True

def get_gps_sum(board: list[list[str]]) -> int:
    gps_sum: int = 0

    for row, line in enumerate(board):
        for col, cell in enumerate(line):
            if cell == 'O' or cell == '[':
                gps_sum += 100 * row + col

    return gps_sum

def part_one(board: list[list[str]], moves: str) -> int:
    robot = get_robot(board)

    for move in moves:
        direction: Vector2D = get_direction(move)
        check: Vector2D = robot + direction
        if board[check.y][check.x] == '#': continue
        if board[check.y][check.x] == '.' or try_push(board, check, direction):
            board[robot.y][robot.x] = '.'
            board[check.y][check.x] = '@'
            robot = check

    return get_gps_sum(board)

def transform(board: list[list[str]]) -> list[list[str]]:
    new_board: list[list[str]] = []
    for row, line in enumerate(board):
        new_board.append([])
        for cell in line:
            if cell == '#' or cell == '.': new_board[row] += [cell, cell]
            elif cell == 'O': new_board[row] += ['[', ']']
            else: new_board[row] += ['@', '.']
    return new_board

def try_push_vertical(board: list[list[str]], left: Vector2D, toward: Vector2D, no_move: bool = False) -> bool:
    right: Vector2D = left + get_direction('>')
    check_left: Vector2D = left + toward
    check_right: Vector2D = right + toward
    if board[check_left.y][check_left.x] == '#' or board[check_right.y][check_right.x] == '#': return False
    if board[check_left.y][check_left.x] == '.' and board[check_right.y][check_right.x] == '.':
        if no_move: return True
        board[check_left.y][check_left.x] = '['
        board[check_right.y][check_right.x] = ']'

        board[left.y][left.x] = '.'
        board[right.y][right.x] = '.'
        return True
    else:
        left_free: bool = board[check_left.y][check_left.x] == '.'
        right_free: bool = board[check_right.y][check_right.x] == '.'

        left_delta: Vector2D = Vector2D(0, 0)
        right_delta: Vector2D = Vector2D(0, 0)
        
        if not left_free and board[check_left.y][check_left.x] == ']': left_delta = get_direction('<')
        if not right_free and board[check_right.y][check_right.x] == ']': right_delta = get_direction('<')

        blocked_left: Vector2D = check_left + left_delta
        blocked_right: Vector2D = check_right + right_delta

        if (left_free or try_push_vertical(board, blocked_left, toward, True)) and (right_free or blocked_left == blocked_right or try_push_vertical(board, blocked_right, toward, True)):
            if no_move: return True
            if board[blocked_left.y][blocked_left.x] != '.': try_push_vertical(board, blocked_left, toward)
            if board[blocked_right.y][blocked_right.x] != '.' and blocked_left != blocked_right: try_push_vertical(board, blocked_right, toward)

            return try_push_vertical(board, left, toward)
        
    return False

def part_two(board: list[list[str]], moves: str) -> int:
    new_board: list[list[str]] = transform(board)
    robot = get_robot(new_board)

    for move in moves:
        direction: Vector2D = get_direction(move)
        check: Vector2D = robot + direction
        if new_board[check.y][check.x] == '#': continue
        if new_board[check.y][check.x] == '.' or (direction.x != 0 and try_push(new_board, check, direction)):
            # Horizontal
            new_board[robot.y][robot.x] = '.'
            new_board[check.y][check.x] = '@'
            robot = check
            continue
        if direction.y != 0:
            # Vertical
            # Always check the left one for consistency
            left: Vector2D = check if new_board[check.y][check.x] == '[' else check + get_direction('<')

            if try_push_vertical(new_board, left, direction):
                new_board[robot.y][robot.x] = '.'
                new_board[check.y][check.x] = '@'
                robot = check

    return get_gps_sum(new_board)

def get_input() -> tuple[list[list[str]], str]:
    with open("day-15/input.txt", "r") as file:
        contents = file.read().split("\n\n")
    return list(map(list, contents[0].split('\n'))), contents[1].replace('\n', '')

def main() -> None:
    board, moves = get_input()
    print(f"Part One: { part_one(deepcopy(board), moves) }\nPart Two: { part_two(deepcopy(board), moves) }")

if __name__ == "__main__":
    main()