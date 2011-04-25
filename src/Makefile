all: jlc

%: %.hs *.hs
	ghc -main-is main -threaded --make $<
clean:
	rm *.{hi,o} jlc