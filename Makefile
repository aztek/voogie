.PHONY = all

all:
	mkdir -p bin
	ghc -Werror -o bin/kyckling Kyckling/Main.hs

hlint:
	hlint Kyckling/

clean:
	rm -rf bin **/*.hi **/**/*.hi
