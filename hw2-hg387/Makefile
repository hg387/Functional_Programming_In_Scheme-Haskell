HW = more-scheme

.PHONY : all
all : $(HW)

.PHONY : clean
clean :
	rm -rf $(HW) run-tests compiled

$(HW) : $(HW).rkt
	raco exe -o $@ $<

run-tests : run-tests.rkt tests.rkt $(HW).rkt
	raco exe -o $@ $<

.PHONY : test
test : run-tests
	./run-tests
