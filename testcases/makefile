# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
# export SUMOC := dune exec $(shell pwd)/../bin/sumoc.exe --
SUMOFLAGS?=-O0

all: call_c.exe return_double_cast.exe return_int_cast.exe \
return_int.exe return_var.exe var_assign_and_return.exe \
call_user_function.exe var_assign_cast.exe \
return_int_arithmetic.exe return_arith_assign.exe return_shift.exe \
return_bitwise.exe return_bool.exe simple_max.exe while.exe for_loop.exe \
return_optional_none.exe return_optional_some.exe struct.exe \
optional_struct.exe struct_cast.exe import.exe

%.exe: %.o
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $^ $(LDLIBS)

import.exe: import_this.o import.o

clean:
	rm -f *.o *.ll *.s *.exe

include ../config.mak