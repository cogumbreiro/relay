#ifndef __LINUX_COMPILER_H
#define __LINUX_COMPILER_H



# define __user         __attribute__((noderef, address_space(1)))
# define __kernel       /* default address space */

#define likely(x)	__builtin_expect(!!(x), 1)
#define unlikely(x)	__builtin_expect(!!(x), 0)


#endif
