#ifndef _BUG___FOO_BAR_
#define _BUG___FOO_BAR_

#include "compiler.h"

/*

#define BUG() do { \
	printk("kernel BUG at %s:%d!\n", __FILE__, __LINE__); \
	panic("BUG!"); \
} while (0)

*/


 #define BUG()                           \
   __asm__ __volatile__(  "ud2\n"         \
                          "\t.word %c0\n" \
                          "\t.long %c1\n" \
                           : : "i" (__LINE__), "i" (__FILE__))



#define BUG_ON(condition) do { if (unlikely((condition)!=0)) BUG(); } while(0)

#endif
