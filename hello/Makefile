#
# Makefile for Hello World in Haskell.
# Author: siavava <amittaijoel@outlook.com>

CC = ghc

PROG = hello

all: $(PROG)

hello: hello.hs
	$(CC) -o $@ $^
	rm -f hello.hi
	rm -f hello.o

assembly: hello.hs
	$(CC) -S -o hello.asm $^

test:
	make $(PROG)
	./hello

clean:
	rm -f $(PROG)
	rm -f hello.hi
	rm -f hello.o