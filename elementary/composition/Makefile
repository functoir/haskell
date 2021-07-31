# Makefile for composition.hs
# Author: siavava < amittaijoel@outlook.com>

SHELL := bash
CC = ghc
PROG = composition
SOURCES = $(PROG).hs

all: $(PROG)

$(PROG): $(SOURCES)
	$(CC) $(CFLAGS) $^ -o $@

test:
	make all
	./$(PROG)
	make clean

clean: 
	rm -f $(PROG)
	rm -f *.hi
	rm -f *.o