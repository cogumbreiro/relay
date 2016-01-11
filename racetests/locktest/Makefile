
OBJS = lock.o fplock.o 
CFLAGS = -Wall -W -pedantic -DDEBUG=y -g
LDFLAGS = -pthread
CC = gcc

all: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS)

clean: 
	rm -f a.out *.o core.* *~
	rm -rf ciltrees
