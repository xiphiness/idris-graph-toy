.PHONY: install
install:
	idris --install matrix.ipkg

.PHONY: clean
clean:
	idris --clean matrix.ipkg
