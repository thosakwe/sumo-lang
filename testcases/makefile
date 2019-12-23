# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
SUMOFLAGS?=-O0

all: call_c return_double_cast return int_cast \
return_int return_var var_assign_and_return

%.exe: %.o
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $(LDLIBS)

clean:
	rm -f call_c return_int return_var *.o *.ll *.s

include ../config.mak