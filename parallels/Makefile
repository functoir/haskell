#
# Makefile for Hello World in Haskell.
# Author: siavava <amittaijoel@outlook.com>

SHELL := bash
TIMER = time
QUIET = @

CC = ghc

CFLAGS = -O2 --make 
LDFLAGS = -threaded -rtsopts

PROG = parallels

all: $(PROG)

parallels: parallels.hs
	$(CC) $(CFLAGS) $^ $(LDFLAGS)

test:
	$(QUIET) make $(PROG)
	CORE_COUNTS="1 2 3 4 5 6 7 8"
	for num_cores in ${CORE_COUNTS}; do 						\
		printf "Running with $$num_cores cores...\n"; 	\
		$(TIMER) ./parallels +RTS -N$$num_cores; 			\
	done

clean:
	rm -f $(PROG)
	rm -f *.hi
	rm -f *.o