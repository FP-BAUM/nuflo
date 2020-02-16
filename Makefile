
.PHONY : build test clean

BINARY=lambda-unif

GHCOPTS=-fwarn-incomplete-patterns

build :
	ghc -isrc/ -itest/ $(GHCOPTS) --make app/Main.hs -o ${BINARY}

test : build
	./${BINARY} -T

clean :
	rm -f app/*.o app/*.hi \
          src/*.o src/*.hi \
          src/Lexer/*.o src/Lexer/*.hi \
          src/Parser/*.o src/Parser/*.hi \
          test/*.o test/*.hi

