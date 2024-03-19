.PHONY: build
build: syntax
		bash ./build.sh
		
.PHONY: syntax
syntax: ./Fun.cf
	bnfc --haskell -d -o src/ ./Fun.cf

.PHONY: clean
clean:
	cabal clean
	rm ./nbe-lambda
	rm -rf src/Fun/*

