.PHONY = all

all:
	mkdir -p bin obj
	ghc -Werror -fwarn-incomplete-patterns -hidir obj -odir obj -isrc -o bin/voogie src/Voogie/Main.hs

hlint:
	hlint src/

clean:
	rm -rf bin obj
