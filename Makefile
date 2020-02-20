
.PHONY : build test clean

BINARY=lambda-unif

GHCOPTS=-fwarn-incomplete-patterns

build :
	ghc -isrc/ -itest/ $(GHCOPTS) --make app/Main.hs -o ${BINARY}

test : build
	./${BINARY} -T

clean :
	rm -f app/*.{o,hi} \
          src/*.{o,hi} \
          src/Syntax/*.{o,hi} \
          src/Lexer/*.{o,hi} \
          src/Parser/*.{o,hi} \
          src/Parser/ModuleSystem/*.{o,hi} \
          test/*.{o,hi}

