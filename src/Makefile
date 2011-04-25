all: jlc

%: %.hs *.hs
	ghc -main-is main -threaded --make $<
	mv jlc ../
clean:
	rm *.{hi,o} jlc