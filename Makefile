
.PHONY : build test clean

BINARY=lambda-unif

build :
	ghc -isrc/ -itest/ --make app/Main.hs -o ${BINARY}

test : build
	./${BINARY} -T

clean :
	rm -f app/*.o app/*.hi \
          src/*.o src/*.hi \
          src/Lexer/*.o src/Lexer/*.hi \
          src/Parser/*.o src/Parser/*.hi \
          test/*.o test/*.hi

