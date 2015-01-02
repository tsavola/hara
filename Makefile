all:: hara lint

hara: hara.hs
	ghc --make hara

lint::
	- hlint hara.hs

clean::
	rm -f hara hara.hi hara.o
