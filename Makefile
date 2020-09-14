export SHELL=/bin/bash

.PHONY: all test clean doc

all: racket-deps exe doc

test:
	raco test -j 8 *.rkt

racket-deps:
	raco pkg install -i --skip-installed rackunit-lib db-lib base32

compile:
	raco make -j 8 *.rkt

exe: compile test
	raco exe -o xiden -l --exf -U cli.rkt

doc:
	raco make docs/guide/*.scrbl docs/reference/*.scrbl
	raco scribble --htmls --dest html +m docs/guide/guide.scrbl
	raco scribble --htmls --dest html +m docs/reference/reference.scrbl

clean:
	git clean -fdX
