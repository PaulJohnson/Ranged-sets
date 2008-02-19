# Tests for Ranged Sets.

all:
	ghc --make -fhpc -i.. -odir . -hidir . -Wall Main.hs -o test-rset
	rm -f test-rset.tix
	./test-rset
	hpc markup --destdir=Report test-rset
	hpc report test-rset

clean:
	rm -fR Data
	rm -f test-rset.tix
	rm -f test-rset
	rm -f *.o *.hi
	rm -fR Report

