obj: tests.o1

tests.o1: tests.scm
	rm -f tests.o1
	gambitc tests
	strip tests.o1

exe: tests.scm
	gambitc -exe ../mk ../test-framework/test-framework run-tests
	strip run-tests

clean:
	rm -f run-tests
	rm -f *.o?
	rm -f *~
	rm -f *.c
