
all: 
	cd src && ghc ./main.hs -o ../latc
clean:
	rm -f ./latc
	rm -f  src/*.hi
	rm -f  src/*.o
	rm -f good/*.bc
	rm -f good/*.ll
	rm -f *.bc
	rm -f *.ll
