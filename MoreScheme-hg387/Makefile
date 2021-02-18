.PHONY : all
all : hw1

.PHONY : clean
clean :
	rm -rf scheme run-tests compiled

run-tests : run-tests.rkt tests.rkt scheme.rkt
	raco exe -o $@ $<

.PHONY : test
test : run-tests
	./run-tests
