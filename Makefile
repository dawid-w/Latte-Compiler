
all: 
	cd src && ghc ./main.hs -o ../latc_llvm
	cd src && ghc ./main.hs -o ../latc
clean:
	rm -f ./latc_llvm
	rm -f ./latc
	rm -f  src/*.hi
	rm -f  src/*.o
	rm -f good/*.bc
	rm -f good/*.ll
	rm -f *.bc
	rm -f *.ll
