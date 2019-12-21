# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
SUMOFLAGS?=-O0

all: call_c return_int return_var #return_var.ll

call_c: call_c.o

clean:
	rm -f call_c return_int return_var *.o *.ll *.s

include ../config.mak