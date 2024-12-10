#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#define INPUT_SIZE 50
#define MOVE_COUNT 4
#define CHECK_VALID_PTR(ptr)                                                                \
    do {                                                                                    \
        if (!ptr) {                                                                         \
            fprintf(stderr, "Null pointer received at %s: %d.\n", __FUNCTION__, __LINE__);  \
            exit(EXIT_FAILURE);                                                             \
        }                                                                                   \
    } while (false);

struct board_position {
    int8_t row;
    int8_t col;
};

const struct board_position UP =    { .row = -1, .col =  0 };
const struct board_position DOWN =  { .row =  1, .col =  0 };
const struct board_position LEFT =  { .row =  0, .col = -1 };
const struct board_position RIGHT = { .row =  0, .col =  1 };
const struct board_position *MOVES[MOVE_COUNT] = { &UP, &DOWN, &LEFT, &RIGHT };

struct board_position add_board_positions(const struct board_position pos1, const struct board_position pos2) {
    return (struct board_position){ .row = pos1.row + pos2.row, .col = pos1.col + pos2.col };
}

bool valid_position(const struct board_position pos, int8_t board_size) {
    return pos.row >= 0 && pos.row < board_size && pos.col >= 0 && pos.col < board_size;
}

struct deque {
    struct board_position * const base;
    struct board_position * const end;
    unsigned short count;
    unsigned short offset;
};

struct deque deque_alloc(unsigned short max_size) {
    struct board_position *base = (struct board_position*)calloc(max_size, sizeof(struct board_position));
    return (struct deque) {
        .base = base,
        .end = base + max_size,
        .count = 0,
        .offset = 0
    };
}

void deque_free(struct deque *deque) {
    free(deque->base);
    memset(deque, 0, sizeof(*deque));
}

void deque_clear(struct deque *deque) {
    memset(deque->base, 0, deque->end - deque->base);
    deque->count = 0; deque->offset = 0;
}

void deque_push(struct deque *deque, struct board_position node) {
    CHECK_VALID_PTR(deque);
    if (deque->base + deque->offset + deque->count == deque->end) {
        memmove(deque->base, deque->base + deque->offset, deque->count * sizeof(struct board_position));
        deque->offset = 0;
    }
    if (deque->base + deque->count >= deque->end) {
        fprintf(stderr, "Max deque size (%u) reached when trying to push (%d, %d).\n", deque->count, node.row, node.col);
        exit(EXIT_FAILURE);
    }
    if (deque->count == (1 << 16) - 1) {
        fprintf(stderr, "Max addressable size reached for unsigned short.\n");
        exit(EXIT_FAILURE);
    }
    deque->base[deque->offset + deque->count++] = node;
}

struct board_position deque_pop(struct deque *deque) {
    CHECK_VALID_PTR(deque);
    if (deque->count == 0) {
        fprintf(stderr, "Tried to pop from empty deque.\n");
        exit(EXIT_FAILURE);
    }
    struct board_position ret = deque->base[deque->offset];
    deque->count--; deque->offset++;
    return ret;
}

int8_t get_input(char*** const board) {
    FILE *file = fopen("/home/fl4t/Documents/AoC2024/day-10/input.txt", "r");
    if (!file) { perror("fopen"); return -1; }

    while (fgetc(file) != '\n');
    const long board_size = ftell(file) - 1;

    assert((board_size <= (1 << 8) - 1) && "Board too big");

    int8_t size = (int8_t)board_size;

    fseek(file, 0, SEEK_SET);
    *board = (char **)malloc(size * sizeof(char *));
    for (int8_t i = 0; i < size; i++) {
        (*board)[i] = (char *)calloc(size + 1, sizeof(char));
        fgets((*board)[i], size + 1, file);
        // Skip the newline character
        (void)fgetc(file);
    }

    fclose(file);
    return size;
}


int trailhead_bfs(struct deque *deque, char **board, int8_t size, struct board_position start, bool rating) {
    char **scratch = (char **)malloc(size * sizeof(char *));
    for (int8_t i = 0; i < size; i++) {
        scratch[i] = (char *)calloc(size + 1, sizeof(char));
        strcpy(scratch[i], board[i]);
    }

    deque_push(deque, start);
    scratch[start.row][start.col] = 0;

    int score = 0;

    while (deque->count) {
        struct board_position curr = deque_pop(deque);
        if (board[curr.row][curr.col] == '9') { score++; continue; } 
        for (int i = 0; i < MOVE_COUNT; i++) {
            struct board_position move = add_board_positions(curr, *MOVES[i]);
            if (!valid_position(move, size) ||
                !scratch[move.row][move.col] ||
                board[curr.row][curr.col] + 1 != board[move.row][move.col])
                continue;
            deque_push(deque, move);
            if (!rating) scratch[move.row][move.col] = 0;
        }
        if (rating) scratch[curr.row][curr.col] = 0;

    }

    for (int8_t i = 0; i < size; i++) free(scratch[i]);
    free(scratch);

    return score;
}

int measure(char **board, int8_t size, bool rating) {
    unsigned short length = (unsigned short)(INPUT_SIZE * INPUT_SIZE); // idk

    int total_score = 0;

    struct deque deque = deque_alloc(length);

    for (int8_t row = 0; row < size; row++) {
        for (int8_t col = 0; col < size; col++) {
            if (board[row][col] == '0') {
                struct board_position start = { .row = row, .col = col };
                total_score += trailhead_bfs(&deque, board, size, start, rating);
                deque_clear(&deque);
            }
        }
    }

    deque_free(&deque);
    return total_score;
}

int main() {
    char **board;
    int8_t size = get_input(&board);

    printf("Part One: %d\nPart Two: %d\n", measure(board, size, false), measure(board, size, true));

    for (int8_t i = 0; i < size; i++) free(board[i]);
    free(board);

    return EXIT_SUCCESS;
}