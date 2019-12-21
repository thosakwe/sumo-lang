%.o: %.sumo
	$(SUMOC) $(SUMOFLAGS) -o $@ $<

%.ll: %.sumo
	$(SUMOC) $(SUMOFLAGS) -emit-llvm -o $@ $<