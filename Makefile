
all: 
	cd src && ghc ./main.hs -o ../latc
clean:
	rm -f ./latc
	rm -f  src/*.hi
	rm -f  src/*.o
