SUMOFLAGS=-O0

all: call_c return_int return_var #return_var.ll

call_c: call_c.o glue.o

clean:
	rm -f call_c return_int return_var *.o *.ll *.s

%.o: %.sumo
	$(SUMOC) $(SUMOFLAGS) -o $@ $<

%.ll: %.sumo
	$(SUMOC) $(SUMOFLAGS) -emit-llvm -o $@ $<