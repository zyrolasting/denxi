export SHELL=/bin/bash

.PHONY: clean

cwd := $(shell pwd)

openssl:
	git clone \
		--recursive \
		--jobs 4 \
		--branch master \
		git://git.openssl.org/openssl.git \
		openssl

dist: openssl
	cd openssl && \
	git fetch && \
	git checkout 3f987381929ee725daf4746591144dde18f313e1 && \
	perl Configure \
		--prefix=$(cwd)/dist \
		--openssldir=$(cwd)/dist \
		-DPEDANTIC \
		-DOPENSSL_USE_IPV6=0 \
		no-comp \
		no-dtls \
		no-nextprotoneg \
		no-psk \
		no-srp \
		no-ssl2 \
		no-ssl3 \
		no-weak-ssl-ciphers \
		shared && \
   make && make test && make install

crypto.so:
	gcc -shared -o crypto.so -fPIC -Idist/include crypto.c \
		-Ldist/lib -lpthread -lc -l:libcrypto.a -l:libssl.a -ldl 

clean:
	rm -rf openssl
