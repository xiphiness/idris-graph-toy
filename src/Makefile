CFLAGS = -Wall `idris --include` `pkg-config --cflags SDL2_gfx`

.PHONY: all
all: init.o

init.o: init.c init.h

.PHONY: clean
clean:
	rm -f init.o
