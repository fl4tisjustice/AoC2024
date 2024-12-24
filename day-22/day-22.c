#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define GENERATIONS 2000

#define PRUNE(n) ((1 << n) - 1)
#define MAX_SECRET_LENGTH 24

size_t get_input(uint32_t **initial) {
    FILE *file = fopen("day-22/input.txt", "r");

    size_t secrets = 1;

    char c;
    while ((c = fgetc(file)) != EOF) if (c == '\n') secrets++;

    fseek(file, 0, SEEK_SET);

    *initial = (uint32_t*)calloc(secrets, sizeof(uint32_t));

    // All secrets are calculated mod 24, so they'll never exceed this number
    char buffer[MAX_SECRET_LENGTH + 1] = { 0 };
    for (size_t idx = 0; idx < secrets; idx++) {
        fgets(buffer, MAX_SECRET_LENGTH + 1, file);
        (*initial)[idx] = (uint32_t)strtoul(buffer, NULL, 10);
    }

    fclose(file);
    return secrets;
}

uint64_t simulate(uint32_t *initial, size_t secrets) {
    for (size_t idx = 0; idx < secrets; idx++) {
        uint32_t seed = initial[idx];
        for (size_t counter = 0; counter < GENERATIONS; counter++) {
            seed = ((seed << 6) ^ seed) & PRUNE(MAX_SECRET_LENGTH);
            seed = ((seed >> 5) ^ seed) & PRUNE(MAX_SECRET_LENGTH);
            seed = ((seed << 11) ^ seed) & PRUNE(MAX_SECRET_LENGTH);
        }
        initial[idx] = seed;
    }

    uint64_t sum = 0;
    for (size_t idx = 0; idx < secrets; idx++) sum += initial[idx];
    return sum;
}

int main() {
    uint32_t *initial = NULL;
    size_t secrets = get_input(&initial);
    printf("Part One: %lu\n", simulate(initial, secrets));

    free(initial);
    return 0;
}