.PHONY = all

all:
	mkdir -p bin
	ghc -Werror -o bin/kyckling Kyckling/Main.hs

clean:
	rm -rf bin **/*.hi **/**/*.hi