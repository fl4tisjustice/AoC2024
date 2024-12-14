#include <stdlib.h>
#include "regez.h"

regex_t *regex_alloc(void) { return (regex_t*)malloc(sizeof(regex_t)); }
void regex_free(regex_t *ptr) { return free(ptr); }