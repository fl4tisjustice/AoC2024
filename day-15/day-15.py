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

def try_push(board: str, check: Vector2D, toward: Vector2D) -> bool:
    next: Vector2D = check + toward
    if board[next.y][next.x] == '#': return False
    if board[next.y][next.x] == '.' or try_push(board, next, toward):
        board[next.y][next.x] = 'O'
        return True

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

    gps_sum: int = 0

    for row, line in enumerate(board):
        for col, cell in enumerate(line):
            if cell == 'O':
                gps_sum += 100 * row + col

    return gps_sum

def transform(board: list[list[str]]) -> list[list[str]]:
    new_board: list[list[str]] = []
    for row, line in enumerate(board):
        new_board.append([])
        for cell in line:
            if cell == '#' or cell == '.': new_board[row] += [cell, cell]
            elif cell == 'O': new_board[row] += ['[', ']']
            else: new_board[row] += ['@', '.']
    return new_board

# def try_push_expanded(board: str, check: Vector2D, toward: Vector2D, no_check: bool = False) -> bool:
#     next: Vector2D = check + toward
#     if board[next.y][next.x] == '#': return False
#     if toward.x != 0 and (board[next.y][next.x] == '.' or try_push_expanded(board, next, toward)):
#         board[next.y][next.x] = board[check.y][check.x]
#         return True
#     if toward.y != 0 and board[next.y][next.x] == '.':
        
#         other_half: str = '[' if board[check.y][check.x] == ']' else ']'
#         other_half_delta: Vector2D = get_direction('<' if board[check.y][check.x] == ']' else '>')
#         other_half_pos: Vector2D = other_half_delta + next
        
#         if board[other_half_pos.y][other_half_pos.x] == '#': return False
#         if board[other_half_pos.y][other_half_pos.x] == '.':
#             if no_check: return True

#             # Move
#             board[next.y][next.x] = board[check.y][check.x]
#             board[other_half_pos.y][other_half_pos.x] = other_half

#             # Erase previous
#             prev: Vector2D = other_half_delta + check
#             board[check.y][check.x] = '.'
#             board[prev.y][prev.x] = '.'
#             return True
#     if toward.y != 0:
#         if board[next.y][next.x] == board[check.y][check.x] and try_push_expanded(board, next, toward):
#             try_push_expanded(board, check, toward)
#             return True
#         elif board[next.y][next.x] != board[check.y][check.x]:
#             extra: Vector2D = get_direction('<' if board[next.y][next.x] == '[' else '>') + next;
#             if try_push_expanded(board, next, toward, True) and try_push_expanded(board, extra, toward, True):
#                 if board[next.y][next.x] != '.': try_push_expanded(board, next, toward)
#                 if board[extra.y][extra.x] != '.': try_push_expanded(board, extra, toward)
#                 try_push_expanded(board, check, toward)
#                 return True
#     return False

# def part_two(board: list[list[str]], moves: str) -> int:
#     new_board: list[list[str]] = transform(board)
#     robot = get_robot(new_board)

#     for move in moves:
#         print('\n'.join(''.join(line) for line in new_board))
#         direction: Vector2D = get_direction(move)
#         check: Vector2D = robot + direction
#         if new_board[check.y][check.x] == '#': continue
#         if new_board[check.y][check.x] == '.':
#             new_board[robot.y][robot.x] = '.'
#             new_board[check.y][check.x] = '@'
#             robot = check
#             continue
#         if try_push_expanded(new_board, check, direction):
#             new_board[robot.y][robot.x] = '.'
#             # Vertical
#             if direction.x == 0:
#                 confirm_half: Vector2D = check + direction
#                 leftover_box_pos: Vector2D = get_direction('>' if new_board[confirm_half.y][confirm_half.x] == '[' else '<') + check
#                 new_board[leftover_box_pos.y][leftover_box_pos.x] = '.'
#             new_board[check.y][check.x] = '@'
#             robot = check

#     print('\n'.join(''.join(line) for line in new_board))


def get_input() -> tuple[list[list[str]], str]:
    with open("day-15/input.txt", "r") as file:
        contents = file.read().split("\n\n")
    return list(map(list, contents[0].split('\n'))), contents[1].replace('\n', '')

def main() -> None:
    board, moves = get_input()

    print(f"Part One: { part_one(deepcopy(board), moves) }")

if __name__ == "__main__":
    main()