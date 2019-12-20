SUMOFLAGS=-O0

all: call_c return_int return_var #return_var.ll

call_c: call_c.o glue.o

clean:
	rm -f return_int return_var *.o *.ll *.s

%.o: %.sumo
	dune exec ../bin/sumoc.exe -- $(SUMOFLAGS) -o $@ $<

%.ll: %.sumo
	dune exec ../bin/sumoc.exe -- $(SUMOFLAGS) -emit-llvm -o $@ $<