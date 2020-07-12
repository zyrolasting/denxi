all: exe docs
.PHONY: all

test:
	raco test .

racket-deps:
	raco pkg install web-server-lib rackunit-lib

compile:
	raco make service/*.rkt *.rkt

exe: compile test
	raco exe -o zcpkg cli.rkt

doc:
	raco make *.scrbl
	raco scribble --dest doc +m manual.scrbl

clean:
	git clean -fdX
