# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
SUMOFLAGS?=-O0

all: call_c.exe return_double_cast.exe return int_cast.exe \
return_int.exe return_var.exe var_assign_and_return.exe \
call_user_function.exe

%.exe: %.o
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $(LDLIBS)

clean:
	rm -f *.o *.ll *.s *.exe

include ../config.mak