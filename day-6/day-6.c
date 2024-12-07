#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

struct board_position {
    long row;
    long col;
};

struct board_position add_board_positions(const struct board_position pos1, const struct board_position pos2) {
    return (struct board_position){ .row = pos1.row + pos2.row, .col = pos1.col + pos2.col };
}

struct board_position subtract_board_positions(const struct board_position pos1, const struct board_position pos2) {
    return (struct board_position){ .row = pos1.row - pos2.row, .col = pos1.col - pos2.col };
}

bool valid_position(const struct board_position pos, long board_size) {
    return pos.row >= 0 && pos.row < board_size && pos.col >= 0 && pos.col < board_size;
}

bool position_equals(const struct board_position pos1, const struct board_position pos2) {
    return pos1.row == pos2.row && pos1.col == pos2.col;
}

enum direction {
    UP, RIGHT, DOWN, LEFT
};

enum direction rotate(enum direction dir, int times) {
    return (enum direction)((dir + times) % 4);
}

// Same order as enum definition
const char *direction_mapping = "^>v<";

struct board_position delta_from_direction(enum direction dir) {
    struct board_position delta = { 0 };
    switch (dir) {
        case UP:
            delta.row--;
            break;
        case RIGHT:
            delta.col++;
            break;
        case DOWN:
            delta.row++;
            break;
        case LEFT:
            delta.col--;
            break;
    }
    return delta;
}


long get_input(char*** const board) {
    FILE *file = fopen("day-6/input.txt", "r");
    if (!file) { perror("fopen"); return -1; }

    while (fgetc(file) != '\n');
    const long board_size = ftell(file) - 1;
    fseek(file, 0, SEEK_SET);
    *board = (char**)malloc(board_size * sizeof(char*));
    for (long i = 0; i < board_size; i++) {
        (*board)[i] = (char*)calloc(board_size + 1, sizeof(char));
        fgets((*board)[i], board_size + 1, file);
        // Skip the newline character
        (void)fgetc(file);
    }

    fclose(file);
    return board_size;
}

struct board_position get_guard_pos(const char **board, long board_size) {
    struct board_position guard_pos = { -1, -1 };
    for (long row = 0; row < board_size; row++) {
        for (long col = 0; col < board_size; col++) {
            if (strchr(direction_mapping, board[row][col])) {
                guard_pos.row = row; guard_pos.col = col;
                return guard_pos;
            }
        }
    }
    return guard_pos;
}

// Returns true if traversed without loops
bool traverse(char **board, long board_size) {
    struct board_position guard_pos = get_guard_pos((const char**)board, board_size);
    enum direction guard_dir = (enum direction)(strchr(direction_mapping, board[guard_pos.row][guard_pos.col]) - &direction_mapping[0]);

    board[guard_pos.row][guard_pos.col] = '.';

    while (valid_position(guard_pos, board_size)) {
        if (board[guard_pos.row][guard_pos.col] == direction_mapping[guard_dir])
            return false;

        struct board_position delta = delta_from_direction(guard_dir);
        if (valid_position(add_board_positions(guard_pos, delta), board_size) &&
            board[guard_pos.row + delta.row][guard_pos.col + delta.col] == '#') {
            guard_dir = rotate(guard_dir, 1);
            continue;
        }

        board[guard_pos.row][guard_pos.col] = direction_mapping[guard_dir];
        guard_pos = add_board_positions(guard_pos, delta);
    }

    return true;
}


int part_one(const char **traversed, long board_size) {
    int distinct = 0;

    for (long row = 0; row < board_size; row++)
        for (long col = 0; col < board_size; col++)
            if (!strchr("#.", traversed[row][col])) distinct++;

    return distinct;
}

int part_two(const char **traversed, long board_size) {
    char **initial_board;

    get_input(&initial_board);
    const struct board_position guard_pos = get_guard_pos((const char**)initial_board, board_size);

    char **playground = (char **)malloc(board_size * sizeof(char *));

    for (long i = 0; i < board_size; i++) {
        playground[i] = (char *)calloc(board_size + 1, sizeof(char));
        strcpy(playground[i], initial_board[i]);
    }

    int loops = 0;

    for (long row = 0; row < board_size; row++) {
        for (long col = 0; col < board_size; col++) {
            if (!strchr("#.", traversed[row][col]) && !(row == guard_pos.row && col == guard_pos.col)) {
                playground[row][col] = '#';
                loops += (int)!traverse(playground, board_size);
                for (long i = 0; i < board_size; i++) strcpy(playground[i], initial_board[i]);
            }
        }
    }

    for (long i = 0; i < board_size; i++) { free(initial_board[i]); free(playground[i]); }
    free(initial_board); free(playground);

    return loops;
}

int main() {
    char **board;
    const long board_size = get_input(&board);

    (void)traverse(board, board_size);

    printf("Part One: %d\nPart Two: %d\n", part_one((const char **)board, board_size), part_two((const char**)board, board_size));
    
    for (long i = 0; i < board_size; i++) free(board[i]);
    free(board);
    
    return 0;
}