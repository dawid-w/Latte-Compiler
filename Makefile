
all: 
	cd src && bnfc -d -m latte.cf --functor
	make -C src
	cd src && ghc ./main.hs -o ../latc
clean:
	make clean -C src
	rm -f ./latc
	make clean -C src
	rm -f  src/*.hi
	rm -f  src/*.o
	rm -f  src/Makefile
	rm -rf src/Latte
