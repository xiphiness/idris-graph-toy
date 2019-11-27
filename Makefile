.PHONY: install
install:
	idris --install graph-toy.ipkg

.PHONY: clean
clean:
	idris --clean graph-toy.ipkg
