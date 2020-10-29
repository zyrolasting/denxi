export SHELL=/bin/bash

.PHONY: default test clean

default: setup

install:
	raco pkg install -i --skip-installed

setup: install
	raco setup --doc-index --fail-fast -j 8 xiden

setup-doc:
	raco scribble +m --markdown --dest-name INSTALL docs/guide/setup.scrbl

test: setup
	raco test -j 8 -c xiden

exe: build test
	raco exe -o xiden -l --exf -U cli.rkt

clean:
	git clean -fdX
