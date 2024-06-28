sources = app/Main.hs $(wildcard src/*.hs) 

.PHONY : clean

icfpc : $(sources)
	cabal build
	cp `cabal list-bin icfpc-exe` icfpc

clean :
	$(RM) icfpc
	cabal clean

