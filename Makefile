
.PHONY : build test clean

BINARY=nuflo

GHCOPTS=-fwarn-incomplete-patterns

build :
	ghc -isrc/ -itest/ $(GHCOPTS) --make app/Main.hs -o ${BINARY}

test : build
	./${BINARY} -T

clean :
	rm -f app/*.{o,hi} \
          src/*.{o,hi} \
          src/Calculus/*.{o,hi} \
          src/Desugaring/*.{o,hi} \
          src/Eval/*.{o,hi} \
          src/Infer/*.{o,hi} \
          src/Lexer/*.{o,hi} \
          src/ModuleSystem/*.{o,hi} \
          src/Parser/*.{o,hi} \
          src/Syntax/*.{o,hi} \
          test/*.{o,hi}

