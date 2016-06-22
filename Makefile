.PHONY = all

all:
	mkdir -p bin obj
	ghc -Werror -hidir obj -odir obj -isrc -o bin/kyckling src/Kyckling/Main.hs

hlint:
	hlint src/

clean:
	rm -rf bin obj