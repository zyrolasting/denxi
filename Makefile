.PHONY: all test clean doc

all: exe doc

test: compile
	raco test -j 8 *.rkt

racket-deps:
	raco pkg install web-server-lib rackunit-lib

compile:
	raco make -j 8 -v *.rkt

exe: compile test
	raco exe -o zcpkg \
	++lang racket/base \
	++lib racket \
	++lib rackunit \
	++lib compiler/cm \
	cli.rkt

doc:
	raco make *.scrbl
	raco scribble --dest doc +m *.scrbl

clean:
	git clean -fdX
