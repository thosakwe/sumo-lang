all: libsumo.a

clean:
	rm -f *.a *.o

libsumo.a: stdio.o stdio_glue.o
	$(AR) rcs $@ $^

include ../config.mak