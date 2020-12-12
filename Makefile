SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

occ: $(OBJS)
	$(CC) -o $@ $^

$(OBJS): occ.h

test: occ
	./test.sh

clean:
	rm -f occ *.o tmp*

.PHONY: test clean
