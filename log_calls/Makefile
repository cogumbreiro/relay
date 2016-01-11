TARGETS := libcalllog.a libcalllog_pt.a libcalllog.so.1.0.1 libcalllog_pt.so.1.0.1

all: $(TARGETS)

#static
libcalllog.a: logger_single_thread.o
	$(AR) rcs libcalllog.a logger_single_thread.o

libcalllog_pt.a: logger_pthread.o
	$(AR) rcs libcalllog_pt.a logger_pthread.o

#shared
libcalllog.so.1.0.1: logger_single_thread.o
	$(CC) -shared -Wl,-soname,libcalllog.so.1 -o libcalllog.so.1.0.1 logger_single_thread.o

libcalllog_pt.so.1.0.1: logger_pthread.o
	$(CC) -shared -Wl,-soname,libcalllog_pt.so.1 -o libcalllog_pt.so.1.0.1 logger_pthread.o


#Make object files
.c.o:
	$(CC) -c -fPIC $< -o `dirname $<`/`basename $< .c`.o

.PHONY: clean
clean:
	rm -f $(TARGETS)
	rm -f *.o
	rm -f *.*~
	rm -rf ciltrees
