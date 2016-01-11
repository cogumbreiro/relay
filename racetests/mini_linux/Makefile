
OBJS = poly_l2.o sleep.o block_dev.o reassembly.o ieee80211_rx.o cpqphp_pci.o \
	dnotify.o oid_mgt.o pci_i386.o aio.o main.o
CFLAGS = -Wall -W -pedantic -DDEBUG=y -g
LDFLAGS = -pthread
CC = gcc

all: $(OBJS)
	$(CC) $(OBJS) $(LDFLAGS)

clean: 
	rm -f a.out *.o core.* *~
	rm -rf ciltrees
