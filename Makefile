export SHELL=/bin/bash

.PHONY: default test clean

default: setup

install:
	raco pkg install -i --skip-installed

setup: install
	raco setup --check-pkg-deps --doc-index --fail-fast -j 8 denxi

setup-doc:
	raco scribble +m --markdown --dest-name INSTALL docs/guide/setup.scrbl

test: setup
	raco test -j 8 -c denxi

exe: build test
	raco exe -o denxi -l --exf -U cli.rkt

clean:
	git clean -fdX
