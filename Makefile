.PHONY = all

all:
	mkdir -p bin
	ghc -Werror -isrc -o bin/kyckling src/Kyckling/Main.hs

hlint:
	hlint src/

clean:
	rm -rf bin **/*.hi **/**/*.hi **/**/**/*.hi
