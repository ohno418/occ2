occ: main.o
	$(CC) -o occ main.o

test: occ
	./test.sh

clean:
	rm -f occ *.o tmp*

.PHONY: test clean
