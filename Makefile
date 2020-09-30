export SHELL=/bin/bash

.PHONY: all test clean build

all: build exe

test:
	raco test -j 8 -c xiden

build:
	raco pkg install -i --skip-installed
	raco setup --doc-index --fail-fast -j 8 xiden

exe: build test
	raco exe -o xiden -l --exf -U cli.rkt

clean:
	git clean -fdX
