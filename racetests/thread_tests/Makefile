
OBJS = test_dir/threads2.o test_dir/threads.o test_dir/lock.o
CFLAGS = -Wall -W -pedantic -DDEBUG=y -g
LDFLAGS = -pthread
CC = gcc

all: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS)

clean: 
	rm -f a.out *.o core.* *~
	rm -f test_dir/*.o test_dir/*~
	rm -rf ciltrees
