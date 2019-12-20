all: return_int

clean:
	rm -f return_int *.o

%.o: %.sumo
	dune exec ../bin/sumoc.exe -- -o $@ $<