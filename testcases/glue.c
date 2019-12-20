#include <stdint.h>
#include <stdio.h>

void tulip_glue_putc(int64_t ch) { putc(ch, stdout); }