#
# Makefile for Hello World in Haskell.
# Author: siavava <amittaijoel@outlook.com>

CC = ghc

CFLAGS = -O2 --make 
LDFLAGS = -threaded -rtscript

PROG = parallels

all: $(PROG)

parallels: parallels.hs
	$(CC) $(CLAGS) $^ $(LDLAGS) -o $@

test:
	make $(PROG)
	./parallels

clean:
	rm -f $(PROG)
	rm -f *.hi
	rm -f *.o