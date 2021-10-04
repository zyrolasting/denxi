#! /usr/bin/env sh

url='https://mirror.racket-lang.org/installers/8.1/racket-8.1-x86_64-linux-cs.sh'
source="(http-source \"$url\")"
>racket-8.1-x86_64-linux-cs.sh denxi fetch -m 300 "$source"
