.PHONY: all test clean

all: exe docs

test:
	raco test .

racket-deps:
	raco pkg install web-server-lib rackunit-lib

compile:
	raco make *.rkt

exe: compile test
	raco exe -o zcpkg cli.rkt

doc:
	raco make *.scrbl
	raco scribble --dest doc +m *.scrbl

clean:
	git clean -fdX
