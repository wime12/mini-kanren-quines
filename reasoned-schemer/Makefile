obj: reasoned-schemer-tests.o1 chapter-1.o1 chapter-2.o1

reasoned-schemer-tests.o1: reasoned-schemer-tests.scm
	rm -f reasoned-schemer-tests.o1
	gambitc reasoned-schemer-tests
	strip reasoned-schemer-tests.o1

chapter-1.o1: chapter-1.scm
	rm -f chapter-1.o1
	gambitc chapter-1
	strip chapter-1.o1

chapter-2.o1: chapter-2.scm
	rm -f chapter-2.o1
	gambitc chapter-2
	strip chapter-2.o1

run-tests: chapter-1.scm chapter-2.scm run-tests.scm
	gambitc -exe ../r6rs-compat ../mk ../test-framework/test-framework \
		reasoned-schemer-tests chapter-1 chapter-2 run-tests 
	strip run-tests

clean:
	rm -f run-tests
	rm -f *.o?
	rm -f *.c
	rm -f *~
