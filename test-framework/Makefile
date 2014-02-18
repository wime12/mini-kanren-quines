obj: test-framework.o1

test-framework.o1: test-framework.scm
	rm -f test-framework.o1
	gambitc test-framework
	strip test-framework.o1

clean:
	rm -f *.o?
	rm -f *~
