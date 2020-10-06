export SHELL=/bin/bash

.PHONY: test clean

install:
	raco pkg install -i --skip-installed

setup: install
	raco setup --doc-index --fail-fast -j 8 xiden

test: setup
	raco test -j 8 -c xiden

exe: build test
	raco exe -o xiden -l --exf -U cli.rkt

clean:
	git clean -fdX
