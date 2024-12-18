#include <stdio.h>
#include <stdint.h>
#include <regex.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#define REGISTERS 3
#define REG_A 0
#define REG_B 1
#define REG_C 2

#define OPERAND(ip)     *(ip + 1)
#define COMBO(op, regs) (op < 4 ? op : regs[op % 4])

// Make each instruction fit in a single byte
enum __attribute__((__packed__)) opcode {
    ADV, // REG(A) = REG(A) / (2 ** COMBO(*(IP+1)));   IP+=2
    BXL, // REG(B) = REG(B) ^ *(IP+1);                 IP+=2
    BST, // REG(B) = COMBO(*(IP+1)) % 8;               IP+=2
    JNZ, // REG(A) == 0 ? (nop; IP+=2) : IP = *(IP+1)  
    BXC, // REG(B) = REG(B) ^ REG(C);                  IP+=2
    OUT, // COMBO(*(IP+1)) % 8 >> stdout;              IP+=2
    BDV, // REG(B) = REG(A) / (2 ** COMBO(*(IP+1)));   IP+=2
    CDV  // REG(C) = REG(A) / (2 ** COMBO(*(IP+1)));   IP+=2
};

void run(enum opcode *program, unsigned int instruction_count, uint64_t *registers, uint8_t *buf)  {
    enum opcode *ip = program;
    bool output = false;
    size_t out_count = 0;

    while (ip != program + instruction_count) {
        switch (*ip) {
            case ADV: case BDV: case CDV:
                registers[*ip == ADV ? REG_A : (*ip == BDV ? REG_B : REG_C)] =
                    registers[REG_A] >> COMBO(OPERAND(ip), registers);
                break;
            case BXL:
                registers[REG_B] = registers[REG_B] ^ OPERAND(ip);
                break;
            case BST:
                registers[REG_B] = COMBO(OPERAND(ip), registers) & 0b111;
                break;
            case JNZ:
                if (registers[REG_A] == 0) break;
                ip = program + OPERAND(ip);
                continue; // Instruction pointer not increased
            case BXC:
                registers[REG_B] = registers[REG_B] ^ registers[REG_C];
                break;
            case OUT: {
                uint64_t result = COMBO(OPERAND(ip), registers) & 0b111;
                if (buf != NULL) buf[out_count] = result;
                else printf("%s%lu", output ? "," : "", result);
                output = true;
                out_count++;
                break;
            }
        }
        ip += 2;
    }

    if (output && buf == NULL) printf("\n");
}


uint64_t find_quine(enum opcode *program, unsigned int instruction_count, uint64_t *registers,
                    uint8_t *buf, unsigned int cmp_idx, uint64_t prev_cond) {
    if (cmp_idx == instruction_count) return prev_cond;
    
    // Allocate output buffer in topmost call
    uint8_t *output;
    if (!buf) output =(uint8_t*)calloc(instruction_count, sizeof(uint8_t));
    else output = buf;

    uint64_t quine_cond = 0;

    for (unsigned int i = 0; i < 8; i++) {
        quine_cond = prev_cond << 3 | i;

        registers[REG_A] = quine_cond;
        run(program, instruction_count, registers, output);

        if (memcmp(&program[instruction_count - cmp_idx - 1], output, cmp_idx + 1) != 0) continue;
        quine_cond = find_quine(program, instruction_count, registers, output, cmp_idx + 1, quine_cond);

        if (quine_cond == 0) continue;
        return quine_cond;
    }

    // Only free in the topmost call, where it was allocated
    // (Onlt the caller changes the value of buf)
    if (!buf) free(buf);
    return 0;
}

unsigned int get_input(enum opcode **program, uint64_t *registers) {
    FILE *file = fopen("day-17/input.txt", "r");

    fseek(file, 0, SEEK_END);
    long filesize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *contents = (char*)malloc(filesize + 1);
    fread(contents, filesize + 1, 1, file);
    fclose(file);

    regex_t register_regex;
    regcomp(&register_regex, "([0-9]+)\n", REG_EXTENDED);

    regmatch_t register_match;
    regoff_t offset = 0;
    for (size_t i = 0; i < REGISTERS; i++) {
        regexec(&register_regex, contents + offset, 1, &register_match, 0);
        registers[i] = strtoull(&contents[offset + register_match.rm_so], NULL, 10);
        offset += register_match.rm_eo;
    }
    regfree(&register_regex);

    unsigned int instruction_count = 1;
    for (long i = 0; i <= filesize; i++)
        if (contents[i] == ',')
            instruction_count++;

    *program = (enum opcode*)malloc(instruction_count * sizeof(enum opcode));

    regex_t program_regex;
    regcomp(&program_regex, "([0-9]+,)", REG_EXTENDED);

    regmatch_t instruction_match;
    offset = 0;
    for (unsigned int i = 0; i < instruction_count; i++) {
        regexec(&program_regex, contents + offset, 1, &instruction_match, 0);
        (*program)[i] = (enum opcode)strtol(&contents[offset + instruction_match.rm_so], NULL, 10);
        offset += instruction_match.rm_eo;
    }
    regfree(&program_regex);
    
    free(contents);
    return instruction_count;
}

int main() {
    enum opcode *program = NULL;
    uint64_t registers[REGISTERS] = { 0 };

    unsigned int instruction_count = get_input(&program, &registers[0]);

    printf("Part One: "); run(program, instruction_count, &registers[0], NULL);
    printf("Part Two: %lu\n", find_quine(program, instruction_count, &registers[0], NULL, 0, 0));

    free(program);
    return 0;
}