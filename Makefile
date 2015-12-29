.PHONY: test

default: test

test: Main
	./Main table.csv inflate
	./Main table.t

Main: Main.hs
	ghc $@
