#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

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


void dfs(struct plot *curr, char plant, unsigned int *perimeter, unsigned int *area) {
    if (curr == NULL || curr->plant != plant) { *perimeter += 1; return; }
    if (curr->visited) return;

    curr->visited = true;
    *area += 1;

    dfs(curr->up, plant, perimeter, area);
    dfs(curr->down, plant, perimeter, area);
    dfs(curr->left, plant, perimeter, area);
    dfs(curr->right, plant, perimeter, area);
}

unsigned int get_fence_pricing(struct garden *garden) {
    unsigned int total = 0;

    for (size_t i = 0; i < garden->cell_count; i++) {
        if (garden->vertices[i].visited) continue;
        unsigned int perimeter = 0;
        unsigned int area = 0;

        struct plot *start = &garden->vertices[i];
        dfs(start, start->plant, &perimeter, &area);
        total += perimeter * area;
    }

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

    printf("Part One: %u\n", get_fence_pricing(&garden));

    free(garden.vertices);
    for (size_t i = 0; i < size; i++) free(board[i]);
    free(board);
    return 0;
}