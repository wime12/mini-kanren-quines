obj: lambda-calculus.o1 tests.o1

lambda-calculus.o1: lambda-calculus.scm
	rm -f lambda-calculus.o1
	gambitc lambda-calculus
	strip lambda-calculus.o1

tests.o1: tests.scm
	rm -f tests.o1
	gambitc tests
	strip tests.o1

exe: tests.scm
	gambitc -exe ../r6rs-compat ../mk ../test-framework/test-framework \
		lambda-calculus tests run-tests
	strip run-tests

clean:
	rm -f run-tests
	rm -f *.o?
	rm -f *~
	rm -f *.c
