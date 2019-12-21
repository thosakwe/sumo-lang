#include <stdint.h>
#include <stdio.h>

void sumo_glue_putc(int64_t ch) { putc(ch, stdout); }