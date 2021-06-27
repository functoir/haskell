#
# Makefile for Hello World in Haskell.
# Author: siavava <amittaijoel@outlook.com>

CC = ghci

PROG = hello

all: $(PROG)

hello: hello.hs
	$(CC) $^ -o $@

test:
	make $(PROG)
	./hello