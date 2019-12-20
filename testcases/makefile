SUMOFLAGS=-O0

all: return_int return_var return_var.ll

clean:
	rm -f return_int return_var *.o *.ll

%.o: %.sumo
	dune exec ../bin/sumoc.exe -- $(SUMOFLAGS) -o $@ $<

%.ll: %.sumo
	dune exec ../bin/sumoc.exe -- $(SUMOFLAGS) -emit-llvm -o $@ $<