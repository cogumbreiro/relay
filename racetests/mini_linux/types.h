
#ifndef _TYPES___FOO_BAR
#define _TYPES___FOO_BAR


#include <stddef.h>
#include <errno.h>


///

#define container_of(ptr, type, member) ({ const typeof( ((type *)0)->member ) *__mptr = (ptr); (type *)( (char *)__mptr - offsetof(type,member) );})


///

typedef unsigned char u8;
typedef signed   char s8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;


typedef unsigned char __u8;
typedef unsigned short __u16;
typedef unsigned int __u32;
typedef unsigned long long __u64;

typedef signed char __s8;
typedef signed short __s16;
typedef signed int __s32;
typedef signed long long __s64;



typedef __u16  __le16;
typedef __u16  __be16;
typedef __u32  __le32;
typedef __u32  __be32;


typedef unsigned int	__kernel_size_t;
typedef __kernel_size_t		size_t;

typedef int             __kernel_ssize_t;
typedef __kernel_ssize_t        ssize_t;

typedef __u32 __kernel_dev_t;
typedef __kernel_dev_t		dev_t;


typedef unsigned int	__kernel_uid32_t;
typedef unsigned int	__kernel_gid32_t;

typedef __kernel_uid32_t	uid_t;
typedef __kernel_gid32_t	gid_t;


typedef long long       __kernel_loff_t;
typedef __kernel_loff_t         loff_t;



typedef unsigned short umode_t;

#define __bitwise__ __attribute__((bitwise))

typedef unsigned __bitwise__ gfp_t;



#endif
