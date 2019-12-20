## Calling C functions
This is still in flux, because there is no real design for it yet.
At this point, your best bet for calling C code from Sumo is to simply
write your glue code in C.

`external` functions can optionally have an `#c_name`, which is the name they will
be called as by the emitted code.

Sumo code:
```sumo
external #sumo_glue_putc putc(int ch) : void

fn main() : void {
  putc(0x48)
}
```

C glue:
```c
#include <stdint.h>
#include <stdio.h>

void sumo_glue_putc(void *sumo_context, int64_t ch) {
  putc(ch);
}
```