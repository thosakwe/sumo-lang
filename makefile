export SUMOC?=dune exec $(shell pwd)/bin/sumoc.exe --
.PHONY: clean testcases lib

all: testcases lib

lib:
	$(MAKE) -C lib

testcases:
	$(MAKE) -C testcases

clean:
	$(MAKE) -C testcases clean
	$(MAKE) -C lib clean