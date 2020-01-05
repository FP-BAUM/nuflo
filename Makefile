
.PHONY : build test clean

BINARY=lambda-unif

build :
	ghc -isrc/ -itest/ --make app/Main.hs -o ${BINARY}

test : build
	./${BINARY} -t

clean :
	rm -f app/*.o app/*.hi src/*.o src/*.hi test/*.o test/*.hi
