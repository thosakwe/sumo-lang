export LDLIBS+=-L$(shell pwd)/lib -lsumo
export SUMOC?=sumoc
# export SUMOC ?= dune exec $(shell pwd)/bin/sumoc.exe --

.PHONY: clean testcases lib

all: testcases lib

lib:
	# $(MAKE) -C lib

testcases: lib
	$(MAKE) -C testcases

clean:
	$(MAKE) -C testcases clean
	$(MAKE) -C lib clean