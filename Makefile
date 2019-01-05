BIN_DIR = ${HOME}/.bin

.PHONY: deps test clean

all: deps test

deps:
	idris --build deps.ipkg

test:
	idris --build test.ipkg
	make -C tests

clean:
	make -C src clean
	make -C tests clean
	rm -f deps
	rm -f runtests

install:
	install deps ${BIN_DIR}
