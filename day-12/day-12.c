#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <stdint.h>

struct plot {
    struct plot *up;
    struct plot *down;
    struct plot *left;
    struct plot *right;
    char plant;
    bool visited;
};

struct garden {
    struct plot * const vertices;
    size_t cell_count;
};

typedef uint8_t fenced_flags;
enum direction {
    NONE,   // = 0
    UP      = 1 << 0,
    DOWN    = 1 << 1,
    LEFT    = 1 << 2,
    RIGHT   = 1 << 3
};

static inline fenced_flags get_fenced_flags(struct plot *plot) {
    fenced_flags flags = NONE;
    flags |= plot->up == NULL    || plot->up->plant != plot->plant    ? UP    : NONE;
    flags |= plot->down == NULL  || plot->down->plant != plot->plant  ? DOWN  : NONE;
    flags |= plot->left == NULL  || plot->left->plant != plot->plant  ? LEFT  : NONE;
    flags |= plot->right == NULL || plot->right->plant != plot->plant ? RIGHT : NONE;
    return flags;
}

bool check_outer_corner(struct plot *plot, enum direction direction) {
    char cmp1, cmp2;
    struct plot *diagonal;
    switch ((fenced_flags)direction) {
        case UP | LEFT: {
            if (plot->up == NULL || plot->left == NULL) return false;
            diagonal = plot->up->left;
            cmp1 = plot->up->plant; cmp2 = plot->left->plant;
            return cmp1 == cmp2 && plot->plant == cmp1 && diagonal->plant != plot->plant;
        }
        case LEFT | DOWN: {
            if (plot->left == NULL || plot->down == NULL) return false;
            diagonal = plot->left->down;
            cmp1 = plot->left->plant; cmp2 = plot->down->plant;
            return cmp1 == cmp2 && plot->plant == cmp1 && diagonal->plant != plot->plant;
        }
        case DOWN | RIGHT: {
            if (plot->down == NULL || plot->right == NULL) return false;
            diagonal = plot->down->right;
            cmp1 = plot->down->plant; cmp2 = plot->right->plant;
            return cmp1 == cmp2 && plot->plant == cmp1 && diagonal->plant != plot->plant;
        }
        case RIGHT | UP: {
            if (plot->right == NULL || plot->up == NULL) return false;
            diagonal = plot->right->up;
            cmp1 = plot->right->plant; cmp2 = plot->up->plant;
            return cmp1 == cmp2 && plot->plant == cmp1 && diagonal->plant != plot->plant;
        }
        default:
            return false;
    }
}

static inline struct plot *get_valid_plot(struct plot * const vertices, ssize_t size, ssize_t row, ssize_t col) {
    if (row < 0 || row >= size || col < 0 || col >= size) return NULL;
    return &vertices[row * size + col];
}

struct garden build_garden(char **board, size_t size) {
    struct plot * const vertices = (struct plot *)calloc(size * size, sizeof(struct plot));

    // Initialize vertices
    for (size_t row = 0; row < size; row++) {
        for (size_t col = 0; col < size; col++) {
            struct plot plot = {
                .plant = board[row][col],
                .visited = false
            };
            vertices[row * size + col] = plot;
        }
    }

    // Initialize edges
    for (size_t row = 0; row < size; row++) {
        for (size_t col = 0; col < size; col++) {
            struct plot *plot = &vertices[row * size + col];
            plot->up    = get_valid_plot(vertices, size, row - 1, col);
            plot->down  = get_valid_plot(vertices, size, row + 1, col);
            plot->left  = get_valid_plot(vertices, size, row, col - 1);
            plot->right = get_valid_plot(vertices, size, row, col + 1);
        }
    }

    return (struct garden) {
        .vertices = vertices,
        .cell_count = size * size
    };
}


void dfs(struct plot *curr, char plant, unsigned int *perimeter, unsigned int *area, bool discount) {
    if (curr == NULL || curr->plant != plant) { if (!discount) *perimeter += 1; return; }
    if (curr->visited) return;

    curr->visited = true;
    *area += 1;

    dfs(curr->up, plant, perimeter, area, discount);
    dfs(curr->down, plant, perimeter, area, discount);
    dfs(curr->left, plant, perimeter, area, discount);
    dfs(curr->right, plant, perimeter, area, discount);

    if (!discount) return;

    // Check if inner corner
    fenced_flags flags = get_fenced_flags(curr);
    if ((flags & (UP | LEFT)) == (UP | LEFT)) *perimeter += 1;
    if ((flags & (LEFT | DOWN)) == (LEFT | DOWN)) *perimeter += 1;
    if ((flags & (DOWN | RIGHT)) == (DOWN | RIGHT)) *perimeter += 1;
    if ((flags & (RIGHT | UP)) == (RIGHT | UP)) *perimeter += 1;

    // Check if outer corner
    if (check_outer_corner(curr, UP | LEFT)) *perimeter += 1;
    if (check_outer_corner(curr, LEFT | DOWN)) *perimeter += 1;
    if (check_outer_corner(curr, DOWN | RIGHT)) *perimeter += 1;
    if (check_outer_corner(curr, RIGHT | UP)) *perimeter += 1;
}

unsigned int get_fence_pricing(struct garden *garden, bool discount) {
    unsigned int total = 0;

    for (size_t i = 0; i < garden->cell_count; i++) {
        if (garden->vertices[i].visited) continue;
        unsigned int perimeter = 0;
        unsigned int area = 0;

        struct plot *start = &garden->vertices[i];
        dfs(start, start->plant, &perimeter, &area, discount);
        total += perimeter * area;
    }

    for (size_t i = 0; i < garden->cell_count; i++) garden->vertices[i].visited = false;
    return total;
}

size_t get_input(char ***board) {
    FILE *file = fopen("day-12/input.txt", "r");

    while (fgetc(file) != '\n');

    const size_t size = ftell(file) - 1;
    fseek(file, 0, SEEK_SET);

    *board = (char**)malloc(size * sizeof(char*));
    for (size_t i = 0; i < size; i++) {
        (*board)[i] = (char*)calloc(size + 1, sizeof(char));
        fgets((*board)[i], size + 1, file);
        // Skip the newline character
        (void)fgetc(file);
    }

    fclose(file);
    return size;
}

int main() {
    char **board;
    size_t size = get_input(&board);
    struct garden garden = build_garden(board, size);

    printf("Part One: %u\nPart Two: %u\n", get_fence_pricing(&garden, false), get_fence_pricing(&garden, true));

    free(garden.vertices);
    for (size_t i = 0; i < size; i++) free(board[i]);
    free(board);
    return 0;
}