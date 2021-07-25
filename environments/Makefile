# Makefile for environments.hs
# Author: siavava < amittaijoel@outlook.com>

SHELL := bash
CC = ghc
PROG = environments
SOURCES = $(PROG).hs

all: $(PROG)

$(PROG): $(SOURCES)
	$(CC) $(CFLAGS) $^ -o $@

test:
	make all
	./$(PROG) test1 test2 test3
	make clean

clean: 
	rm -f $(PROG)
	rm -f *.hi
	rm -f *.o