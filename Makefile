all: music

music:
	ghc --make Main.hs -o music

clean: 
	rm -f *.o music
	rm -f *.hi	
	
