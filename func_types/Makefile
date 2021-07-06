# Makefile for func_types.hs
# Author: siavava < amittaijoel@outlook.com>

SHELL := bash
CC = ghc
PROG = func_types

all: $(PROG)

$(PROG): func_types.hs
	$(CC) $(CFLAGS) $^ -o $@

test:
	make all
	./func_types
	make clean

clean: 
	rm -f $(PROG)
	rm -f *.hi
	rm -f *.o