obj: r6rs-compat.o1 mk.o1 readtable.o1

r6rs-compat.o1: r6rs-compat.scm
	rm -f r6rs-compat.o1
	gambitc r6rs-compat
	strip r6rs-compat.o1

mk.o1: mk.scm mk\#.scm
	rm -f mk.o1
	gambitc mk
	strip mk.o1

readtable.o1: readtable.scm
	rm -f readtable.o1
	gambitc readtable
	strip readtable.o1

clean:
	rm -f *.o?
	rm -f *~
	rm -f *.c
