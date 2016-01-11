# 1 "drivers/ide/ide-disk.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "drivers/ide/ide-disk.c"
# 50 "drivers/ide/ide-disk.c"
# 1 "include/linux/config.h" 1



# 1 "include/linux/autoconf.h" 1
# 5 "include/linux/config.h" 2
# 51 "drivers/ide/ide-disk.c" 2
# 1 "include/linux/module.h" 1
# 10 "include/linux/module.h"
# 1 "include/linux/sched.h" 1



# 1 "include/asm/param.h" 1
# 5 "include/linux/sched.h" 2


# 1 "include/linux/capability.h" 1
# 16 "include/linux/capability.h"
# 1 "include/linux/types.h" 1
# 13 "include/linux/types.h"
# 1 "include/linux/posix_types.h" 1



# 1 "include/linux/stddef.h" 1



# 1 "include/linux/compiler.h" 1
# 44 "include/linux/compiler.h"
# 1 "include/linux/compiler-gcc3.h" 1



# 1 "include/linux/compiler-gcc.h" 1
# 5 "include/linux/compiler-gcc3.h" 2
# 45 "include/linux/compiler.h" 2
# 5 "include/linux/stddef.h" 2
# 5 "include/linux/posix_types.h" 2
# 36 "include/linux/posix_types.h"
typedef struct {
        unsigned long fds_bits [(1024/(8 * sizeof(unsigned long)))];
} __kernel_fd_set;


typedef void (*__kernel_sighandler_t)(int);


typedef int __kernel_key_t;
typedef int __kernel_mqd_t;

# 1 "include/asm/posix_types.h" 1
# 10 "include/asm/posix_types.h"
typedef unsigned long __kernel_ino_t;
typedef unsigned short __kernel_mode_t;
typedef unsigned short __kernel_nlink_t;
typedef long __kernel_off_t;
typedef int __kernel_pid_t;
typedef unsigned short __kernel_ipc_pid_t;
typedef unsigned short __kernel_uid_t;
typedef unsigned short __kernel_gid_t;
typedef unsigned int __kernel_size_t;
typedef int __kernel_ssize_t;
typedef int __kernel_ptrdiff_t;
typedef long __kernel_time_t;
typedef long __kernel_suseconds_t;
typedef long __kernel_clock_t;
typedef int __kernel_timer_t;
typedef int __kernel_clockid_t;
typedef int __kernel_daddr_t;
typedef char * __kernel_caddr_t;
typedef unsigned short __kernel_uid16_t;
typedef unsigned short __kernel_gid16_t;
typedef unsigned int __kernel_uid32_t;
typedef unsigned int __kernel_gid32_t;

typedef unsigned short __kernel_old_uid_t;
typedef unsigned short __kernel_old_gid_t;
typedef unsigned short __kernel_old_dev_t;


typedef long long __kernel_loff_t;


typedef struct {

        int val[2];



} __kernel_fsid_t;
# 48 "include/linux/posix_types.h" 2
# 14 "include/linux/types.h" 2
# 1 "include/asm/types.h" 1





typedef unsigned short umode_t;






typedef __signed__ char __s8;
typedef unsigned char __u8;

typedef __signed__ short __s16;
typedef unsigned short __u16;

typedef __signed__ int __s32;
typedef unsigned int __u32;


typedef __signed__ long long __s64;
typedef unsigned long long __u64;
# 40 "include/asm/types.h"
typedef signed char s8;
typedef unsigned char u8;

typedef signed short s16;
typedef unsigned short u16;

typedef signed int s32;
typedef unsigned int u32;

typedef signed long long s64;
typedef unsigned long long u64;




typedef u64 dma_addr_t;



typedef u64 dma64_addr_t;
# 15 "include/linux/types.h" 2



typedef __u32 __kernel_dev_t;

typedef __kernel_fd_set fd_set;
typedef __kernel_dev_t dev_t;
typedef __kernel_ino_t ino_t;
typedef __kernel_mode_t mode_t;
typedef __kernel_nlink_t nlink_t;
typedef __kernel_off_t off_t;
typedef __kernel_pid_t pid_t;
typedef __kernel_daddr_t daddr_t;
typedef __kernel_key_t key_t;
typedef __kernel_suseconds_t suseconds_t;
typedef __kernel_timer_t timer_t;
typedef __kernel_clockid_t clockid_t;
typedef __kernel_mqd_t mqd_t;


typedef __kernel_uid32_t uid_t;
typedef __kernel_gid32_t gid_t;
typedef __kernel_uid16_t uid16_t;
typedef __kernel_gid16_t gid16_t;



typedef __kernel_old_uid_t old_uid_t;
typedef __kernel_old_gid_t old_gid_t;
# 55 "include/linux/types.h"
typedef __kernel_loff_t loff_t;
# 64 "include/linux/types.h"
typedef __kernel_size_t size_t;




typedef __kernel_ssize_t ssize_t;




typedef __kernel_ptrdiff_t ptrdiff_t;




typedef __kernel_time_t time_t;




typedef __kernel_clock_t clock_t;




typedef __kernel_caddr_t caddr_t;



typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;


typedef unsigned char unchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;




typedef __u8 u_int8_t;
typedef __s8 int8_t;
typedef __u16 u_int16_t;
typedef __s16 int16_t;
typedef __u32 u_int32_t;
typedef __s32 int32_t;



typedef __u8 uint8_t;
typedef __u16 uint16_t;
typedef __u32 uint32_t;


typedef __u64 uint64_t;
typedef __u64 u_int64_t;
typedef __s64 int64_t;
# 135 "include/linux/types.h"
typedef unsigned long sector_t;
# 159 "include/linux/types.h"
typedef __u16 __le16;
typedef __u16 __be16;
typedef __u32 __le32;
typedef __u32 __be32;

typedef __u64 __le64;
typedef __u64 __be64;



typedef unsigned gfp_t;


struct ustat {
        __kernel_daddr_t f_tfree;
        __kernel_ino_t f_tinode;
        char f_fname[6];
        char f_fpack[6];
};
# 17 "include/linux/capability.h" 2
# 32 "include/linux/capability.h"
typedef struct __user_cap_header_struct {
        __u32 version;
        int pid;
} *cap_user_header_t;

typedef struct __user_cap_data_struct {
        __u32 effective;
        __u32 permitted;
        __u32 inheritable;
} *cap_user_data_t;



# 1 "include/linux/spinlock.h" 1
# 50 "include/linux/spinlock.h"
# 1 "include/linux/preempt.h" 1
# 10 "include/linux/preempt.h"
# 1 "include/linux/linkage.h" 1




# 1 "include/asm/linkage.h" 1
# 6 "include/linux/linkage.h" 2
# 11 "include/linux/preempt.h" 2
# 51 "include/linux/spinlock.h" 2


# 1 "include/linux/thread_info.h" 1
# 13 "include/linux/thread_info.h"
struct restart_block {
        long (*fn)(struct restart_block *);
        unsigned long arg0, arg1, arg2, arg3;
};

extern long do_no_restart_syscall(struct restart_block *parm);

# 1 "include/linux/bitops.h" 1
# 11 "include/linux/bitops.h"
static inline __attribute__((always_inline)) int generic_ffs(int x)
{
        int r = 1;

        if (!x)
                return 0;
        if (!(x & 0xffff)) {
                x >>= 16;
                r += 16;
        }
        if (!(x & 0xff)) {
                x >>= 8;
                r += 8;
        }
        if (!(x & 0xf)) {
                x >>= 4;
                r += 4;
        }
        if (!(x & 3)) {
                x >>= 2;
                r += 2;
        }
        if (!(x & 1)) {
                x >>= 1;
                r += 1;
        }
        return r;
}





static __inline__ __attribute__((always_inline)) int generic_fls(int x)
{
        int r = 32;

        if (!x)
                return 0;
        if (!(x & 0xffff0000u)) {
                x <<= 16;
                r -= 16;
        }
        if (!(x & 0xff000000u)) {
                x <<= 8;
                r -= 8;
        }
        if (!(x & 0xf0000000u)) {
                x <<= 4;
                r -= 4;
        }
        if (!(x & 0xc0000000u)) {
                x <<= 2;
                r -= 2;
        }
        if (!(x & 0x80000000u)) {
                x <<= 1;
                r -= 1;
        }
        return r;
}





# 1 "include/asm/bitops.h" 1
# 42 "include/asm/bitops.h"
static inline __attribute__((always_inline)) void set_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btsl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
# 59 "include/asm/bitops.h"
static inline __attribute__((always_inline)) void __set_bit(int nr, volatile unsigned long * addr)
{
        __asm__(
                "btsl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
# 77 "include/asm/bitops.h"
static inline __attribute__((always_inline)) void clear_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btrl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}

static inline __attribute__((always_inline)) void __clear_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__(
                "btrl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
# 104 "include/asm/bitops.h"
static inline __attribute__((always_inline)) void __change_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__(
                "btcl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
# 122 "include/asm/bitops.h"
static inline __attribute__((always_inline)) void change_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btcl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
# 139 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int test_and_set_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;

        __asm__ __volatile__( "lock ; "
                "btsl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
# 159 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int __test_and_set_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;

        __asm__(
                "btsl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
        return oldbit;
}
# 179 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int test_and_clear_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;

        __asm__ __volatile__( "lock ; "
                "btrl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
# 199 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int __test_and_clear_bit(int nr, volatile unsigned long *addr)
{
        int oldbit;

        __asm__(
                "btrl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
        return oldbit;
}


static inline __attribute__((always_inline)) int __test_and_change_bit(int nr, volatile unsigned long *addr)
{
        int oldbit;

        __asm__ __volatile__(
                "btcl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
# 230 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int test_and_change_bit(int nr, volatile unsigned long* addr)
{
        int oldbit;

        __asm__ __volatile__( "lock ; "
                "btcl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
# 250 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int constant_test_bit(int nr, const volatile unsigned long *addr)
{
        return ((1UL << (nr & 31)) & (addr[nr >> 5])) != 0;
}

static inline __attribute__((always_inline)) int variable_test_bit(int nr, const volatile unsigned long * addr)
{
        int oldbit;

        __asm__ __volatile__(
                "btl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit)
                :"m" ((*(volatile long *) addr)),"Ir" (nr));
        return oldbit;
}
# 281 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int find_first_zero_bit(const unsigned long *addr, unsigned size)
{
        int d0, d1, d2;
        int res;

        if (!size)
                return 0;

        __asm__ __volatile__(
                "movl $-1,%%eax\n\t"
                "xorl %%edx,%%edx\n\t"
                "repe; scasl\n\t"
                "je 1f\n\t"
                "xorl -4(%%edi),%%eax\n\t"
                "subl $4,%%edi\n\t"
                "bsfl %%eax,%%edx\n"
                "1:\tsubl %%ebx,%%edi\n\t"
                "shll $3,%%edi\n\t"
                "addl %%edi,%%edx"
                :"=d" (res), "=&c" (d0), "=&D" (d1), "=&a" (d2)
                :"1" ((size + 31) >> 5), "2" (addr), "b" (addr) : "memory");
        return res;
}







int find_next_zero_bit(const unsigned long *addr, int size, int offset);







static inline __attribute__((always_inline)) unsigned long __ffs(unsigned long word)
{
        __asm__("bsfl %1,%0"
                :"=r" (word)
                :"rm" (word));
        return word;
}
# 335 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int find_first_bit(const unsigned long *addr, unsigned size)
{
        int x = 0;

        while (x < size) {
                unsigned long val = *addr++;
                if (val)
                        return __ffs(val) + x;
                x += (sizeof(*addr)<<3);
        }
        return x;
}







int find_next_bit(const unsigned long *addr, int size, int offset);







static inline __attribute__((always_inline)) unsigned long ffz(unsigned long word)
{
        __asm__("bsfl %1,%0"
                :"=r" (word)
                :"r" (~word));
        return word;
}
# 384 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int sched_find_first_bit(const unsigned long *b)
{
        if (__builtin_expect(!!(b[0]), 0))
                return __ffs(b[0]);
        if (__builtin_expect(!!(b[1]), 0))
                return __ffs(b[1]) + 32;
        if (__builtin_expect(!!(b[2]), 0))
                return __ffs(b[2]) + 64;
        if (b[3])
                return __ffs(b[3]) + 96;
        return __ffs(b[4]) + 128;
}
# 405 "include/asm/bitops.h"
static inline __attribute__((always_inline)) int ffs(int x)
{
        int r;

        __asm__("bsfl %1,%0\n\t"
                "jnz 1f\n\t"
                "movl $-1,%0\n"
                "1:" : "=r" (r) : "rm" (x));
        return r+1;
}
# 78 "include/linux/bitops.h" 2

static __inline__ __attribute__((always_inline)) int get_bitmask_order(unsigned int count)
{
        int order;

        order = generic_fls(count);
        return order;
}






static inline __attribute__((always_inline)) unsigned int generic_hweight32(unsigned int w)
{
        unsigned int res = (w & 0x55555555) + ((w >> 1) & 0x55555555);
        res = (res & 0x33333333) + ((res >> 2) & 0x33333333);
        res = (res & 0x0F0F0F0F) + ((res >> 4) & 0x0F0F0F0F);
        res = (res & 0x00FF00FF) + ((res >> 8) & 0x00FF00FF);
        return (res & 0x0000FFFF) + ((res >> 16) & 0x0000FFFF);
}

static inline __attribute__((always_inline)) unsigned int generic_hweight16(unsigned int w)
{
        unsigned int res = (w & 0x5555) + ((w >> 1) & 0x5555);
        res = (res & 0x3333) + ((res >> 2) & 0x3333);
        res = (res & 0x0F0F) + ((res >> 4) & 0x0F0F);
        return (res & 0x00FF) + ((res >> 8) & 0x00FF);
}

static inline __attribute__((always_inline)) unsigned int generic_hweight8(unsigned int w)
{
        unsigned int res = (w & 0x55) + ((w >> 1) & 0x55);
        res = (res & 0x33) + ((res >> 2) & 0x33);
        return (res & 0x0F) + ((res >> 4) & 0x0F);
}

static inline __attribute__((always_inline)) unsigned long generic_hweight64(__u64 w)
{

        return generic_hweight32((unsigned int)(w >> 32)) +
                                generic_hweight32((unsigned int)w);
# 130 "include/linux/bitops.h"
}

static inline __attribute__((always_inline)) unsigned long hweight_long(unsigned long w)
{
        return sizeof(w) == 4 ? generic_hweight32(w) : generic_hweight64(w);
}







static inline __attribute__((always_inline)) __u32 rol32(__u32 word, unsigned int shift)
{
        return (word << shift) | (word >> (32 - shift));
}







static inline __attribute__((always_inline)) __u32 ror32(__u32 word, unsigned int shift)
{
        return (word >> shift) | (word << (32 - shift));
}
# 21 "include/linux/thread_info.h" 2
# 1 "include/asm/thread_info.h" 1
# 14 "include/asm/thread_info.h"
# 1 "include/asm/page.h" 1
# 45 "include/asm/page.h"
extern int nx_enabled;

extern unsigned long long __supported_pte_mask;
typedef struct { unsigned long pte_low, pte_high; } pte_t;
typedef struct { unsigned long long pmd; } pmd_t;
typedef struct { unsigned long long pgd; } pgd_t;
typedef struct { unsigned long long pgprot; } pgprot_t;
# 104 "include/asm/page.h"
extern unsigned int __VMALLOC_RESERVE;

extern int sysctl_legacy_va_layout;

extern int page_is_ram(unsigned long pagenr);
# 144 "include/asm/page.h"
# 1 "include/asm-generic/page.h" 1
# 10 "include/asm-generic/page.h"
static __inline__ __attribute__((always_inline)) __attribute__((__const__)) int get_order(unsigned long size)
{
        int order;

        size = (size - 1) >> (12 - 1);
        order = -1;
        do {
                size >>= 1;
                order++;
        } while (size);
        return order;
}
# 145 "include/asm/page.h" 2
# 15 "include/asm/thread_info.h" 2


# 1 "include/asm/processor.h" 1
# 10 "include/asm/processor.h"
# 1 "include/asm/vm86.h" 1
# 68 "include/asm/vm86.h"
struct vm86_regs {



        long ebx;
        long ecx;
        long edx;
        long esi;
        long edi;
        long ebp;
        long eax;
        long __null_ds;
        long __null_es;
        long __null_fs;
        long __null_gs;
        long orig_eax;
        long eip;
        unsigned short cs, __csh;
        long eflags;
        long esp;
        unsigned short ss, __ssh;



        unsigned short es, __esh;
        unsigned short ds, __dsh;
        unsigned short fs, __fsh;
        unsigned short gs, __gsh;
};

struct revectored_struct {
        unsigned long __map[8];
};

struct vm86_struct {
        struct vm86_regs regs;
        unsigned long flags;
        unsigned long screen_bitmap;
        unsigned long cpu_type;
        struct revectored_struct int_revectored;
        struct revectored_struct int21_revectored;
};






struct vm86plus_info_struct {
        unsigned long force_return_for_pic:1;
        unsigned long vm86dbg_active:1;
        unsigned long vm86dbg_TFpendig:1;
        unsigned long unused:28;
        unsigned long is_vm86pus:1;
        unsigned char vm86dbg_intxxtab[32];
};

struct vm86plus_struct {
        struct vm86_regs regs;
        unsigned long flags;
        unsigned long screen_bitmap;
        unsigned long cpu_type;
        struct revectored_struct int_revectored;
        struct revectored_struct int21_revectored;
        struct vm86plus_info_struct vm86plus;
};
# 145 "include/asm/vm86.h"
struct kernel_vm86_regs {



        long ebx;
        long ecx;
        long edx;
        long esi;
        long edi;
        long ebp;
        long eax;
        long __null_ds;
        long __null_es;
        long orig_eax;
        long eip;
        unsigned short cs, __csh;
        long eflags;
        long esp;
        unsigned short ss, __ssh;



        unsigned short es, __esh;
        unsigned short ds, __dsh;
        unsigned short fs, __fsh;
        unsigned short gs, __gsh;
};

struct kernel_vm86_struct {
        struct kernel_vm86_regs regs;
# 184 "include/asm/vm86.h"
        unsigned long flags;
        unsigned long screen_bitmap;
        unsigned long cpu_type;
        struct revectored_struct int_revectored;
        struct revectored_struct int21_revectored;
        struct vm86plus_info_struct vm86plus;
        struct pt_regs *regs32;
# 201 "include/asm/vm86.h"
};

void handle_vm86_fault(struct kernel_vm86_regs *, long);
int handle_vm86_trap(struct kernel_vm86_regs *, long, int);
# 11 "include/asm/processor.h" 2
# 1 "include/asm/math_emu.h" 1



# 1 "include/asm/sigcontext.h" 1
# 20 "include/asm/sigcontext.h"
struct _fpreg {
        unsigned short significand[4];
        unsigned short exponent;
};

struct _fpxreg {
        unsigned short significand[4];
        unsigned short exponent;
        unsigned short padding[3];
};

struct _xmmreg {
        unsigned long element[4];
};

struct _fpstate {

        unsigned long cw;
        unsigned long sw;
        unsigned long tag;
        unsigned long ipoff;
        unsigned long cssel;
        unsigned long dataoff;
        unsigned long datasel;
        struct _fpreg _st[8];
        unsigned short status;
        unsigned short magic;


        unsigned long _fxsr_env[6];
        unsigned long mxcsr;
        unsigned long reserved;
        struct _fpxreg _fxsr_st[8];
        struct _xmmreg _xmm[8];
        unsigned long padding[56];
};



struct sigcontext {
        unsigned short gs, __gsh;
        unsigned short fs, __fsh;
        unsigned short es, __esh;
        unsigned short ds, __dsh;
        unsigned long edi;
        unsigned long esi;
        unsigned long ebp;
        unsigned long esp;
        unsigned long ebx;
        unsigned long edx;
        unsigned long ecx;
        unsigned long eax;
        unsigned long trapno;
        unsigned long err;
        unsigned long eip;
        unsigned short cs, __csh;
        unsigned long eflags;
        unsigned long esp_at_signal;
        unsigned short ss, __ssh;
        struct _fpstate * fpstate;
        unsigned long oldmask;
        unsigned long cr2;
};
# 5 "include/asm/math_emu.h" 2

int restore_i387_soft(void *s387, struct _fpstate *buf);
int save_i387_soft(void *s387, struct _fpstate *buf);





struct info {
        long ___orig_eip;
        long ___ebx;
        long ___ecx;
        long ___edx;
        long ___esi;
        long ___edi;
        long ___ebp;
        long ___eax;
        long ___ds;
        long ___es;
        long ___orig_eax;
        long ___eip;
        long ___cs;
        long ___eflags;
        long ___esp;
        long ___ss;
        long ___vm86_es;
        long ___vm86_ds;
        long ___vm86_fs;
        long ___vm86_gs;
};
# 12 "include/asm/processor.h" 2
# 1 "include/asm/segment.h" 1
# 13 "include/asm/processor.h" 2



# 1 "include/asm/cpufeature.h" 1
# 17 "include/asm/processor.h" 2
# 1 "include/asm/msr.h" 1
# 27 "include/asm/msr.h"
static inline __attribute__((always_inline)) void wrmsrl (unsigned long msr, unsigned long long val)
{
        unsigned long lo, hi;
        lo = (unsigned long) val;
        hi = val >> 32;
        __asm__ __volatile__("wrmsr" : : "c" (msr), "a" (lo), "d" (hi));
}
# 18 "include/asm/processor.h" 2
# 1 "include/asm/system.h" 1




# 1 "include/linux/kernel.h" 1
# 10 "include/linux/kernel.h"
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.3/include/stdarg.h" 1
# 43 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.3/include/stdarg.h"
typedef __builtin_va_list __gnuc_va_list;
# 110 "/usr/lib/gcc-lib/i386-redhat-linux/3.2.3/include/stdarg.h"
typedef __gnuc_va_list va_list;
# 11 "include/linux/kernel.h" 2





# 1 "include/asm/byteorder.h" 1
# 14 "include/asm/byteorder.h"
static __inline__ __attribute__((always_inline)) __attribute__((__const__)) __u32 ___arch__swab32(__u32 x)
{



        __asm__("xchgb %b0,%h0\n\t"
                "rorl $16,%0\n\t"
                "xchgb %b0,%h0"
                :"=q" (x)
                : "0" (x));

        return x;
}

static __inline__ __attribute__((always_inline)) __attribute__((__const__)) __u64 ___arch__swab64(__u64 val)
{
        union {
                struct { __u32 a,b; } s;
                __u64 u;
        } v;
        v.u = val;





   v.s.a = ___arch__swab32(v.s.a);
        v.s.b = ___arch__swab32(v.s.b);
        asm("xchgl %0,%1" : "=r" (v.s.a), "=r" (v.s.b) : "0" (v.s.a), "1" (v.s.b));

        return v.u;
}
# 57 "include/asm/byteorder.h"
# 1 "include/linux/byteorder/little_endian.h" 1
# 12 "include/linux/byteorder/little_endian.h"
# 1 "include/linux/byteorder/swab.h" 1
# 133 "include/linux/byteorder/swab.h"
static __inline__ __attribute__((always_inline)) __attribute__((__const__)) __u16 __fswab16(__u16 x)
{
        return ({ __u16 __tmp = (x) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); });
}
static __inline__ __attribute__((always_inline)) __u16 __swab16p(const __u16 *x)
{
        return ({ __u16 __tmp = (*(x)) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); });
}
static __inline__ __attribute__((always_inline)) void __swab16s(__u16 *addr)
{
        do { *(addr) = ({ __u16 __tmp = (*((addr))) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); }); } while (0);
}

static __inline__ __attribute__((always_inline)) __attribute__((__const__)) __u32 __fswab32(__u32 x)
{
        return ___arch__swab32(x);
}
static __inline__ __attribute__((always_inline)) __u32 __swab32p(const __u32 *x)
{
        return ___arch__swab32(*(x));
}
static __inline__ __attribute__((always_inline)) void __swab32s(__u32 *addr)
{
        do { *(addr) = ___arch__swab32(*((addr))); } while (0);
}


static __inline__ __attribute__((always_inline)) __attribute__((__const__)) __u64 __fswab64(__u64 x)
{





        return ___arch__swab64(x);

}
static __inline__ __attribute__((always_inline)) __u64 __swab64p(const __u64 *x)
{
        return ___arch__swab64(*(x));
}
static __inline__ __attribute__((always_inline)) void __swab64s(__u64 *addr)
{
        do { *(addr) = ___arch__swab64(*((addr))); } while (0);
}
# 13 "include/linux/byteorder/little_endian.h" 2
# 43 "include/linux/byteorder/little_endian.h"
static inline __attribute__((always_inline)) __le64 __cpu_to_le64p(const __u64 *p)
{
        return ( __le64)*p;
}
static inline __attribute__((always_inline)) __u64 __le64_to_cpup(const __le64 *p)
{
        return ( __u64)*p;
}
static inline __attribute__((always_inline)) __le32 __cpu_to_le32p(const __u32 *p)
{
        return ( __le32)*p;
}
static inline __attribute__((always_inline)) __u32 __le32_to_cpup(const __le32 *p)
{
        return ( __u32)*p;
}
static inline __attribute__((always_inline)) __le16 __cpu_to_le16p(const __u16 *p)
{
        return ( __le16)*p;
}
static inline __attribute__((always_inline)) __u16 __le16_to_cpup(const __le16 *p)
{
        return ( __u16)*p;
}
static inline __attribute__((always_inline)) __be64 __cpu_to_be64p(const __u64 *p)
{
        return ( __be64)__swab64p(p);
}
static inline __attribute__((always_inline)) __u64 __be64_to_cpup(const __be64 *p)
{
        return __swab64p((__u64 *)p);
}
static inline __attribute__((always_inline)) __be32 __cpu_to_be32p(const __u32 *p)
{
        return ( __be32)__swab32p(p);
}
static inline __attribute__((always_inline)) __u32 __be32_to_cpup(const __be32 *p)
{
        return __swab32p((__u32 *)p);
}
static inline __attribute__((always_inline)) __be16 __cpu_to_be16p(const __u16 *p)
{
        return ( __be16)__swab16p(p);
}
static inline __attribute__((always_inline)) __u16 __be16_to_cpup(const __be16 *p)
{
        return __swab16p((__u16 *)p);
}
# 104 "include/linux/byteorder/little_endian.h"
# 1 "include/linux/byteorder/generic.h" 1
# 154 "include/linux/byteorder/generic.h"
extern __u32 ntohl(__be32);
extern __be32 htonl(__u32);
extern __u16 ntohs(__be16);
extern __be16 htons(__u16);
# 105 "include/linux/byteorder/little_endian.h" 2
# 58 "include/asm/byteorder.h" 2
# 17 "include/linux/kernel.h" 2
# 1 "include/asm/bug.h" 1
# 25 "include/asm/bug.h"
# 1 "include/asm-generic/bug.h" 1
# 26 "include/asm/bug.h" 2
# 18 "include/linux/kernel.h" 2

extern const char linux_banner[];
# 42 "include/linux/kernel.h"
extern int console_printk[];






struct completion;
# 88 "include/linux/kernel.h"
extern struct notifier_block *panic_notifier_list;
extern long (*panic_blink)(long time);
 void panic(const char * fmt, ...)
        __attribute__ ((noreturn, format (printf, 1, 2)));
__attribute__((regparm(3))) void do_exit(long error_code)
        __attribute__((noreturn));
 void complete_and_exit(struct completion *, long)
        __attribute__((noreturn));
extern unsigned long simple_strtoul(const char *,char **,unsigned int);
extern long simple_strtol(const char *,char **,unsigned int);
extern unsigned long long simple_strtoull(const char *,char **,unsigned int);
extern long long simple_strtoll(const char *,char **,unsigned int);
extern int sprintf(char * buf, const char * fmt, ...)
        __attribute__ ((format (printf, 2, 3)));
extern int vsprintf(char *buf, const char *, va_list)
        __attribute__ ((format (printf, 2, 0)));
extern int snprintf(char * buf, size_t size, const char * fmt, ...)
        __attribute__ ((format (printf, 3, 4)));
extern int vsnprintf(char *buf, size_t size, const char *fmt, va_list args)
        __attribute__ ((format (printf, 3, 0)));
extern int scnprintf(char * buf, size_t size, const char * fmt, ...)
        __attribute__ ((format (printf, 3, 4)));
extern int vscnprintf(char *buf, size_t size, const char *fmt, va_list args)
        __attribute__ ((format (printf, 3, 0)));

extern int sscanf(const char *, const char *, ...)
        __attribute__ ((format (scanf, 2, 3)));
extern int vsscanf(const char *, const char *, va_list)
        __attribute__ ((format (scanf, 2, 0)));

extern int get_option(char **str, int *pint);
extern char *get_options(const char *str, int nints, int *ints);
extern unsigned long long memparse(char *ptr, char **retptr);

extern int __kernel_text_address(unsigned long addr);
extern int kernel_text_address(unsigned long addr);
extern int session_of_pgrp(int pgrp);


 __attribute__((regparm(0))) int vprintk(const char *fmt, va_list args)
        __attribute__ ((format (printf, 1, 0)));
 __attribute__((regparm(0))) int printk(const char * fmt, ...)
        __attribute__ ((format (printf, 1, 2)));
# 140 "include/linux/kernel.h"
unsigned long int_sqrt(unsigned long);

static inline __attribute__((always_inline)) int __attribute__((pure)) long_log2(unsigned long x)
{
        int r = 0;
        for (x >>= 1; x > 0; x >>= 1)
                r++;
        return r;
}

static inline __attribute__((always_inline)) unsigned long __attribute__((__const__)) roundup_pow_of_two(unsigned long x)
{
        return (1UL << generic_fls(x - 1));
}

extern int printk_ratelimit(void);
extern int __printk_ratelimit(int ratelimit_jiffies, int ratelimit_burst);

static inline __attribute__((always_inline)) void console_silent(void)
{
        (console_printk[0]) = 0;
}

static inline __attribute__((always_inline)) void console_verbose(void)
{
        if ((console_printk[0]))
                (console_printk[0]) = 15;
}

extern void bust_spinlocks(int yes);
extern int oops_in_progress;
extern int panic_timeout;
extern int panic_on_oops;
extern int tainted;
extern const char *print_tainted(void);
extern void add_taint(unsigned);


extern enum system_states {
        SYSTEM_BOOTING,
        SYSTEM_RUNNING,
        SYSTEM_HALT,
        SYSTEM_POWER_OFF,
        SYSTEM_RESTART,
} system_state;
# 193 "include/linux/kernel.h"
extern void dump_stack(void);
# 293 "include/linux/kernel.h"
struct sysinfo {
        long uptime;
        unsigned long loads[3];
        unsigned long totalram;
        unsigned long freeram;
        unsigned long sharedram;
        unsigned long bufferram;
        unsigned long totalswap;
        unsigned long freeswap;
        unsigned short procs;
        unsigned short pad;
        unsigned long totalhigh;
        unsigned long freehigh;
        unsigned int mem_unit;
        char _f[20-2*sizeof(long)-sizeof(int)];
};





extern int randomize_va_space;
# 6 "include/asm/system.h" 2






struct task_struct;
extern struct task_struct * __switch_to(struct task_struct *prev, struct task_struct *next) __attribute__((regparm(3)));
# 59 "include/asm/system.h"
static inline __attribute__((always_inline)) unsigned long _get_base(char * addr)
{
        unsigned long __base;
        __asm__("movb %3,%%dh\n\t"
                "movb %2,%%dl\n\t"
                "shll $16,%%edx\n\t"
                "movw %1,%%dx"
                :"=&d" (__base)
                :"m" (*((addr)+2)),
                 "m" (*((addr)+4)),
                 "m" (*((addr)+7)));
        return __base;
}
# 152 "include/asm/system.h"
static inline __attribute__((always_inline)) unsigned long get_limit(unsigned long segment)
{
        unsigned long __limit;
        __asm__("lsll %1,%0"
                :"=r" (__limit):"r" (segment));
        return __limit+1;
}







struct __xchg_dummy { unsigned long a[100]; };
# 184 "include/asm/system.h"
static inline __attribute__((always_inline)) void __set_64bit (unsigned long long * ptr,
                unsigned int low, unsigned int high)
{
        __asm__ __volatile__ (
                "\n1:\t"
                "movl (%0), %%eax\n\t"
                "movl 4(%0), %%edx\n\t"
                "lock cmpxchg8b (%0)\n\t"
                "jnz 1b"
                :
                : "D"(ptr),
                        "b"(low),
                        "c"(high)
                : "ax","dx","memory");
}

static inline __attribute__((always_inline)) void __set_64bit_constant (unsigned long long *ptr,
                                                 unsigned long long value)
{
        __set_64bit(ptr,(unsigned int)(value), (unsigned int)((value)>>32ULL));
}



static inline __attribute__((always_inline)) void __set_64bit_var (unsigned long long *ptr,
                         unsigned long long value)
{
        __set_64bit(ptr,*(((unsigned int*)&(value))+0), *(((unsigned int*)&(value))+1));
}
# 229 "include/asm/system.h"
static inline __attribute__((always_inline)) unsigned long __xchg(unsigned long x, volatile void * ptr, int size)
{
        switch (size) {
                case 1:
                        __asm__ __volatile__("xchgb %b0,%1"
                                :"=q" (x)
                                :"m" (*((struct __xchg_dummy *)(ptr))), "0" (x)
                                :"memory");
                        break;
                case 2:
                        __asm__ __volatile__("xchgw %w0,%1"
                                :"=r" (x)
                                :"m" (*((struct __xchg_dummy *)(ptr))), "0" (x)
                                :"memory");
                        break;
                case 4:
                        __asm__ __volatile__("xchgl %0,%1"
                                :"=r" (x)
                                :"m" (*((struct __xchg_dummy *)(ptr))), "0" (x)
                                :"memory");
                        break;
        }
        return x;
}
# 264 "include/asm/system.h"
static inline __attribute__((always_inline)) unsigned long __cmpxchg(volatile void *ptr, unsigned long old,
                                      unsigned long new, int size)
{
        unsigned long prev;
        switch (size) {
        case 1:
                __asm__ __volatile__("lock ; " "cmpxchgb %b1,%2"
                                     : "=a"(prev)
                                     : "q"(new), "m"(*((struct __xchg_dummy *)(ptr))), "0"(old)
                                     : "memory");
                return prev;
        case 2:
                __asm__ __volatile__("lock ; " "cmpxchgw %w1,%2"
                                     : "=a"(prev)
                                     : "q"(new), "m"(*((struct __xchg_dummy *)(ptr))), "0"(old)
                                     : "memory");
                return prev;
        case 4:
                __asm__ __volatile__("lock ; " "cmpxchgl %1,%2"
                                     : "=a"(prev)
                                     : "q"(new), "m"(*((struct __xchg_dummy *)(ptr))), "0"(old)
                                     : "memory");
                return prev;
        }
        return old;
}






struct alt_instr {
        __u8 *instr;
        __u8 *replacement;
        __u8 cpuid;
        __u8 instrlen;
        __u8 replacementlen;
        __u8 pad;
};
# 485 "include/asm/system.h"
void disable_hlt(void);
void enable_hlt(void);

extern int es7000_plat;
void cpu_idle_wait(void);

extern unsigned long arch_align_stack(unsigned long sp);
# 19 "include/asm/processor.h" 2
# 1 "include/linux/cache.h" 1





# 1 "include/asm/cache.h" 1
# 7 "include/linux/cache.h" 2
# 20 "include/asm/processor.h" 2

# 1 "include/linux/threads.h" 1
# 22 "include/asm/processor.h" 2
# 1 "include/asm/percpu.h" 1



# 1 "include/asm-generic/percpu.h" 1







extern unsigned long __per_cpu_offset[8];
# 5 "include/asm/percpu.h" 2
# 23 "include/asm/processor.h" 2


extern int tsc_disable;

struct desc_struct {
        unsigned long a,b;
};
# 48 "include/asm/processor.h"
struct cpuinfo_x86 {
        __u8 x86;
        __u8 x86_vendor;
        __u8 x86_model;
        __u8 x86_mask;
        char wp_works_ok;
        char hlt_works_ok;
        char hard_math;
        char rfu;
        int cpuid_level;
        unsigned long x86_capability[7];
        char x86_vendor_id[16];
        char x86_model_id[64];
        int x86_cache_size;

        int x86_cache_alignment;
        int fdiv_bug;
        int f00f_bug;
        int coma_bug;
        unsigned long loops_per_jiffy;
        unsigned char x86_num_cores;
} __attribute__((__aligned__((1 << (4)))));
# 87 "include/asm/processor.h"
extern struct cpuinfo_x86 boot_cpu_data;
extern struct cpuinfo_x86 new_cpu_data;
extern struct tss_struct doublefault_tss;
extern __typeof__(struct tss_struct) per_cpu__init_tss;


extern struct cpuinfo_x86 cpu_data[];






extern int phys_proc_id[8];
extern int cpu_core_id[8];
extern char ignore_fpu_irq;

extern void identify_cpu(struct cpuinfo_x86 *);
extern void print_cpu_info(struct cpuinfo_x86 *);
extern unsigned int init_intel_cacheinfo(struct cpuinfo_x86 *c);


extern void detect_ht(struct cpuinfo_x86 *c);
# 140 "include/asm/processor.h"
static inline __attribute__((always_inline)) void cpuid(unsigned int op, unsigned int *eax, unsigned int *ebx, unsigned int *ecx, unsigned int *edx)
{
        __asm__("cpuid"
                : "=a" (*eax),
                  "=b" (*ebx),
                  "=c" (*ecx),
                  "=d" (*edx)
                : "0" (op), "c"(0));
}


static inline __attribute__((always_inline)) void cpuid_count(int op, int count, int *eax, int *ebx, int *ecx,
                int *edx)
{
        __asm__("cpuid"
                : "=a" (*eax),
                  "=b" (*ebx),
                  "=c" (*ecx),
                  "=d" (*edx)
                : "0" (op), "c" (count));
}




static inline __attribute__((always_inline)) unsigned int cpuid_eax(unsigned int op)
{
        unsigned int eax;

        __asm__("cpuid"
                : "=a" (eax)
                : "0" (op)
                : "bx", "cx", "dx");
        return eax;
}
static inline __attribute__((always_inline)) unsigned int cpuid_ebx(unsigned int op)
{
        unsigned int eax, ebx;

        __asm__("cpuid"
                : "=a" (eax), "=b" (ebx)
                : "0" (op)
                : "cx", "dx" );
        return ebx;
}
static inline __attribute__((always_inline)) unsigned int cpuid_ecx(unsigned int op)
{
        unsigned int eax, ecx;

        __asm__("cpuid"
                : "=a" (eax), "=c" (ecx)
                : "0" (op)
                : "bx", "dx" );
        return ecx;
}
static inline __attribute__((always_inline)) unsigned int cpuid_edx(unsigned int op)
{
        unsigned int eax, edx;

        __asm__("cpuid"
                : "=a" (eax), "=d" (edx)
                : "0" (op)
                : "bx", "cx");
        return edx;
}
# 229 "include/asm/processor.h"
extern unsigned long mmu_cr4_features;

static inline __attribute__((always_inline)) void set_in_cr4 (unsigned long mask)
{
        unsigned cr4;
        mmu_cr4_features |= mask;
        cr4 = ({ unsigned int __dummy; __asm__( "movl %%cr4,%0\n\t" :"=r" (__dummy)); __dummy; });
        cr4 |= mask;
        __asm__ __volatile__("movl %0,%%cr4": :"r" (cr4));;
}

static inline __attribute__((always_inline)) void clear_in_cr4 (unsigned long mask)
{
        unsigned cr4;
        mmu_cr4_features &= ~mask;
        cr4 = ({ unsigned int __dummy; __asm__( "movl %%cr4,%0\n\t" :"=r" (__dummy)); __dummy; });
        cr4 &= ~mask;
        __asm__ __volatile__("movl %0,%%cr4": :"r" (cr4));;
}
# 280 "include/asm/processor.h"
static inline __attribute__((always_inline)) void serialize_cpu(void)
{
         __asm__ __volatile__ ("cpuid" : : : "ax", "bx", "cx", "dx");
}

static inline __attribute__((always_inline)) void __monitor(const void *eax, unsigned long ecx,
                unsigned long edx)
{

        asm volatile(
                ".byte 0x0f,0x01,0xc8;"
                : :"a" (eax), "c" (ecx), "d"(edx));
}

static inline __attribute__((always_inline)) void __mwait(unsigned long eax, unsigned long ecx)
{

        asm volatile(
                ".byte 0x0f,0x01,0xc9;"
                : :"a" (eax), "c" (ecx));
}



extern unsigned int machine_id;
extern unsigned int machine_submodel_id;
extern unsigned int BIOS_revision;
extern unsigned int mca_pentium_flag;


extern int bootloader_type;
# 334 "include/asm/processor.h"
struct i387_fsave_struct {
        long cwd;
        long swd;
        long twd;
        long fip;
        long fcs;
        long foo;
        long fos;
        long st_space[20];
        long status;
};

struct i387_fxsave_struct {
        unsigned short cwd;
        unsigned short swd;
        unsigned short twd;
        unsigned short fop;
        long fip;
        long fcs;
        long foo;
        long fos;
        long mxcsr;
        long mxcsr_mask;
        long st_space[32];
        long xmm_space[32];
        long padding[56];
} __attribute__ ((aligned (16)));

struct i387_soft_struct {
        long cwd;
        long swd;
        long twd;
        long fip;
        long fcs;
        long foo;
        long fos;
        long st_space[20];
        unsigned char ftop, changed, lookahead, no_update, rm, alimit;
        struct info *info;
        unsigned long entry_eip;
};

union i387_union {
        struct i387_fsave_struct fsave;
        struct i387_fxsave_struct fxsave;
        struct i387_soft_struct soft;
};

typedef struct {
        unsigned long seg;
} mm_segment_t;

struct thread_struct;

struct tss_struct {
        unsigned short back_link,__blh;
        unsigned long esp0;
        unsigned short ss0,__ss0h;
        unsigned long esp1;
        unsigned short ss1,__ss1h;
        unsigned long esp2;
        unsigned short ss2,__ss2h;
        unsigned long __cr3;
        unsigned long eip;
        unsigned long eflags;
        unsigned long eax,ecx,edx,ebx;
        unsigned long esp;
        unsigned long ebp;
        unsigned long esi;
        unsigned long edi;
        unsigned short es, __esh;
        unsigned short cs, __csh;
        unsigned short ss, __ssh;
        unsigned short ds, __dsh;
        unsigned short fs, __fsh;
        unsigned short gs, __gsh;
        unsigned short ldt, __ldth;
        unsigned short trace, io_bitmap_base;






        unsigned long io_bitmap[((65536/8)/sizeof(long)) + 1];



        unsigned long io_bitmap_max;
        struct thread_struct *io_bitmap_owner;



        unsigned long __cacheline_filler[35];



        unsigned long stack[64];
} __attribute__((packed));



struct thread_struct {

        struct desc_struct tls_array[3];
        unsigned long esp0;
        unsigned long sysenter_cs;
        unsigned long eip;
        unsigned long esp;
        unsigned long fs;
        unsigned long gs;

        unsigned long debugreg[8];

        unsigned long cr2, trap_no, error_code;

        union i387_union i387;

        struct vm86_struct * vm86_info;
        unsigned long screen_bitmap;
        unsigned long v86flags, v86mask, saved_esp0;
        unsigned int saved_fs, saved_gs;

        unsigned long *io_bitmap_ptr;
        unsigned long iopl;

        unsigned long io_bitmap_max;
};
# 483 "include/asm/processor.h"
static inline __attribute__((always_inline)) void load_esp0(struct tss_struct *tss, struct thread_struct *thread)
{
        tss->esp0 = thread->esp0;

        if (__builtin_expect(!!(tss->ss1 != thread->sysenter_cs), 0)) {
                tss->ss1 = thread->sysenter_cs;
                __asm__ __volatile__("wrmsr" : : "c" (0x174), "a" (thread->sysenter_cs), "d" (0));
        }
}
# 518 "include/asm/processor.h"
static inline __attribute__((always_inline)) void set_iopl_mask(unsigned mask)
{
        unsigned int reg;
        __asm__ __volatile__ ("pushfl;"
                              "popl %0;"
                              "andl %1, %0;"
                              "orl %2, %0;"
                              "pushl %0;"
                              "popfl"
                                : "=&r" (reg)
                                : "i" (~0x00003000), "r" (mask));
}


struct task_struct;
struct mm_struct;


extern void release_thread(struct task_struct *);


extern void prepare_to_copy(struct task_struct *tsk);




extern int kernel_thread(int (*fn)(void *), void * arg, unsigned long flags);

extern unsigned long thread_saved_pc(struct task_struct *tsk);
void show_trace(struct task_struct *task, unsigned long *stack);

unsigned long get_wchan(struct task_struct *p);
# 569 "include/asm/processor.h"
struct microcode_header {
        unsigned int hdrver;
        unsigned int rev;
        unsigned int date;
        unsigned int sig;
        unsigned int cksum;
        unsigned int ldrver;
        unsigned int pf;
        unsigned int datasize;
        unsigned int totalsize;
        unsigned int reserved[3];
};

struct microcode {
        struct microcode_header hdr;
        unsigned int bits[0];
};

typedef struct microcode microcode_t;
typedef struct microcode_header microcode_header_t;


struct extended_signature {
        unsigned int sig;
        unsigned int pf;
        unsigned int cksum;
};

struct extended_sigtable {
        unsigned int count;
        unsigned int cksum;
        unsigned int reserved[3];
        struct extended_signature sigs[0];
};




static inline __attribute__((always_inline)) void rep_nop(void)
{
        __asm__ __volatile__("rep;nop": : :"memory");
}
# 682 "include/asm/processor.h"
static inline __attribute__((always_inline)) void prefetch(const void *x)
{
        asm volatile ("661:\n\t" ".byte 0x8d,0x74,0x26,0x00\n" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "prefetchnta (%1)" "\n664:\n" ".previous" :: "i" ((0*32+25)), "r" (x));



}







static inline __attribute__((always_inline)) void prefetchw(const void *x)
{
        asm volatile ("661:\n\t" ".byte 0x8d,0x74,0x26,0x00\n" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "prefetchw (%1)" "\n664:\n" ".previous" :: "i" ((1*32+31)), "r" (x));



}


extern void select_idle_routine(const struct cpuinfo_x86 *c);



extern unsigned long boot_option_idle_override;
extern void enable_sep_cpu(void);
extern int sysenter_setup(void);


extern void mtrr_ap_init(void);
extern void mtrr_bp_init(void);
# 18 "include/asm/thread_info.h" 2
# 28 "include/asm/thread_info.h"
struct thread_info {
        struct task_struct *task;
        struct exec_domain *exec_domain;
        unsigned long flags;
        unsigned long status;
        __u32 cpu;
        int preempt_count;


        mm_segment_t addr_limit;



        struct restart_block restart_block;

        unsigned long previous_esp;


        __u8 supervisor_stack[0];
};
# 88 "include/asm/thread_info.h"
static inline __attribute__((always_inline)) struct thread_info *current_thread_info(void)
{
        struct thread_info *ti;
        __asm__("andl %%esp,%0; ":"=r" (ti) : "0" (~((8192) - 1)));
        return ti;
}


register unsigned long current_stack_pointer asm("esp") __attribute__((__unused__));
# 22 "include/linux/thread_info.h" 2
# 30 "include/linux/thread_info.h"
static inline __attribute__((always_inline)) void set_thread_flag(int flag)
{
        set_bit(flag,&current_thread_info()->flags);
}

static inline __attribute__((always_inline)) void clear_thread_flag(int flag)
{
        clear_bit(flag,&current_thread_info()->flags);
}

static inline __attribute__((always_inline)) int test_and_set_thread_flag(int flag)
{
        return test_and_set_bit(flag,&current_thread_info()->flags);
}

static inline __attribute__((always_inline)) int test_and_clear_thread_flag(int flag)
{
        return test_and_clear_bit(flag,&current_thread_info()->flags);
}

static inline __attribute__((always_inline)) int test_thread_flag(int flag)
{
        return (__builtin_constant_p(flag) ? constant_test_bit((flag),(&current_thread_info()->flags)) : variable_test_bit((flag),(&current_thread_info()->flags)));
}

static inline __attribute__((always_inline)) void set_ti_thread_flag(struct thread_info *ti, int flag)
{
        set_bit(flag,&ti->flags);
}

static inline __attribute__((always_inline)) void clear_ti_thread_flag(struct thread_info *ti, int flag)
{
        clear_bit(flag,&ti->flags);
}

static inline __attribute__((always_inline)) int test_and_set_ti_thread_flag(struct thread_info *ti, int flag)
{
        return test_and_set_bit(flag,&ti->flags);
}

static inline __attribute__((always_inline)) int test_and_clear_ti_thread_flag(struct thread_info *ti, int flag)
{
        return test_and_clear_bit(flag,&ti->flags);
}

static inline __attribute__((always_inline)) int test_ti_thread_flag(struct thread_info *ti, int flag)
{
        return (__builtin_constant_p(flag) ? constant_test_bit((flag),(&ti->flags)) : variable_test_bit((flag),(&ti->flags)));
}

static inline __attribute__((always_inline)) void set_need_resched(void)
{
        set_thread_flag(3);
}

static inline __attribute__((always_inline)) void clear_need_resched(void)
{
        clear_thread_flag(3);
}
# 54 "include/linux/spinlock.h" 2

# 1 "include/linux/stringify.h" 1
# 56 "include/linux/spinlock.h" 2
# 80 "include/linux/spinlock.h"
# 1 "include/linux/spinlock_types.h" 1
# 13 "include/linux/spinlock_types.h"
# 1 "include/asm/spinlock_types.h" 1







typedef struct {
        volatile unsigned int slock;
} raw_spinlock_t;



typedef struct {
        volatile unsigned int lock;
} raw_rwlock_t;
# 14 "include/linux/spinlock_types.h" 2




typedef struct {
        raw_spinlock_t raw_lock;







} spinlock_t;



typedef struct {
        raw_rwlock_t raw_lock;







} rwlock_t;
# 81 "include/linux/spinlock.h" 2

extern int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) generic__raw_read_trylock(raw_rwlock_t *lock);





# 1 "include/asm/spinlock.h" 1



# 1 "include/asm/atomic.h" 1
# 24 "include/asm/atomic.h"
typedef struct { volatile int counter; } atomic_t;
# 52 "include/asm/atomic.h"
static __inline__ __attribute__((always_inline)) void atomic_add(int i, atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "addl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}
# 67 "include/asm/atomic.h"
static __inline__ __attribute__((always_inline)) void atomic_sub(int i, atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "subl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}
# 84 "include/asm/atomic.h"
static __inline__ __attribute__((always_inline)) int atomic_sub_and_test(int i, atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "subl %2,%0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"ir" (i), "m" (v->counter) : "memory");
        return c;
}







static __inline__ __attribute__((always_inline)) void atomic_inc(atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "incl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}







static __inline__ __attribute__((always_inline)) void atomic_dec(atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "decl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}
# 131 "include/asm/atomic.h"
static __inline__ __attribute__((always_inline)) int atomic_dec_and_test(atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "decl %0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"m" (v->counter) : "memory");
        return c != 0;
}
# 150 "include/asm/atomic.h"
static __inline__ __attribute__((always_inline)) int atomic_inc_and_test(atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "incl %0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"m" (v->counter) : "memory");
        return c != 0;
}
# 170 "include/asm/atomic.h"
static __inline__ __attribute__((always_inline)) int atomic_add_negative(int i, atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "addl %2,%0; sets %1"
                :"=m" (v->counter), "=qm" (c)
                :"ir" (i), "m" (v->counter) : "memory");
        return c;
}
# 188 "include/asm/atomic.h"
static __inline__ __attribute__((always_inline)) int atomic_add_return(int i, atomic_t *v)
{
        int __i;

        if(__builtin_expect(!!(boot_cpu_data.x86==3), 0))
                goto no_xadd;


        __i = i;
        __asm__ __volatile__(
                "lock ; " "xaddl %0, %1;"
                :"=r"(i)
                :"m"(v->counter), "0"(i));
        return i + __i;


no_xadd:
        __asm__ __volatile__("cli": : :"memory");
        __i = ((v)->counter);
        (((v)->counter) = (i + __i));
        __asm__ __volatile__("sti": : :"memory");
        return i + __i;

}

static __inline__ __attribute__((always_inline)) int atomic_sub_return(int i, atomic_t *v)
{
        return atomic_add_return(-i,v);
}
# 5 "include/asm/spinlock.h" 2
# 1 "include/asm/rwlock.h" 1
# 6 "include/asm/spinlock.h" 2
# 51 "include/asm/spinlock.h"
static inline __attribute__((always_inline)) void __raw_spin_lock(raw_spinlock_t *lock)
{
        __asm__ __volatile__(
                "\n1:\t" "lock ; decb %0\n\t" "jns 3f\n" "2:\t" "rep;nop\n\t" "cmpb $0,%0\n\t" "jle 2b\n\t" "jmp 1b\n" "3:\n\t"
                :"=m" (lock->slock) : : "memory");
}

static inline __attribute__((always_inline)) void __raw_spin_lock_flags(raw_spinlock_t *lock, unsigned long flags)
{
        __asm__ __volatile__(
                "\n1:\t" "lock ; decb %0\n\t" "jns 4f\n\t" "2:\t" "testl $0x200, %1\n\t" "jz 3f\n\t" "sti\n\t" "3:\t" "rep;nop\n\t" "cmpb $0, %0\n\t" "jle 3b\n\t" "cli\n\t" "jmp 1b\n" "4:\n\t"
                :"=m" (lock->slock) : "r" (flags) : "memory");
}

static inline __attribute__((always_inline)) int __raw_spin_trylock(raw_spinlock_t *lock)
{
        char oldval;
        __asm__ __volatile__(
                "xchgb %b0,%1"
                :"=q" (oldval), "=m" (lock->slock)
                :"0" (0) : "memory");
        return oldval > 0;
}
# 103 "include/asm/spinlock.h"
static inline __attribute__((always_inline)) void __raw_spin_unlock(raw_spinlock_t *lock)
{
        char oldval = 1;

        __asm__ __volatile__(
                "xchgb %b0, %1" :"=q" (oldval), "=m" (lock->slock) :"0" (oldval) : "memory"
        );
}
# 150 "include/asm/spinlock.h"
static inline __attribute__((always_inline)) void __raw_read_lock(raw_rwlock_t *rw)
{
        do { if (__builtin_constant_p(rw)) asm volatile("lock ; " "subl $1,%0\n\t" "jns 1f\n" "pushl %%eax\n\t" "leal %0,%%eax\n\t" "call " "__read_lock_failed" "\n\t" "popl %%eax\n\t" "1:\n" :"=m" (*(volatile int *)rw) : : "memory"); else asm volatile("lock ; " "subl $1,(%0)\n\t" "jns 1f\n" "call " "__read_lock_failed" "\n\t" "1:\n" ::"a" (rw) : "memory"); } while (0);
}

static inline __attribute__((always_inline)) void __raw_write_lock(raw_rwlock_t *rw)
{
        do { if (__builtin_constant_p(rw)) asm volatile("lock ; " "subl $" "0x01000000" ",%0\n\t" "jz 1f\n" "pushl %%eax\n\t" "leal %0,%%eax\n\t" "call " "__write_lock_failed" "\n\t" "popl %%eax\n\t" "1:\n" :"=m" (*(volatile int *)rw) : : "memory"); else asm volatile("lock ; " "subl $" "0x01000000" ",(%0)\n\t" "jz 1f\n" "call " "__write_lock_failed" "\n\t" "1:\n" ::"a" (rw) : "memory"); } while (0);
}

static inline __attribute__((always_inline)) int __raw_read_trylock(raw_rwlock_t *lock)
{
        atomic_t *count = (atomic_t *)lock;
        atomic_dec(count);
        if (((count)->counter) >= 0)
                return 1;
        atomic_inc(count);
        return 0;
}

static inline __attribute__((always_inline)) int __raw_write_trylock(raw_rwlock_t *lock)
{
        atomic_t *count = (atomic_t *)lock;
        if (atomic_sub_and_test(0x01000000, count))
                return 1;
        atomic_add(0x01000000, count);
        return 0;
}

static inline __attribute__((always_inline)) void __raw_read_unlock(raw_rwlock_t *rw)
{
        asm volatile("lock ; incl %0" :"=m" (rw->lock) : : "memory");
}

static inline __attribute__((always_inline)) void __raw_write_unlock(raw_rwlock_t *rw)
{
        asm volatile("lock ; addl $" "0x01000000" ", %0"
                                 : "=m" (rw->lock) : : "memory");
}
# 89 "include/linux/spinlock.h" 2





void spin_lock_init(spinlock_t *lock);
# 109 "include/linux/spinlock.h"
# 1 "include/linux/spinlock_api_smp.h" 1
# 18 "include/linux/spinlock_api_smp.h"
int in_lock_functions(unsigned long addr);



void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_lock(spinlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_lock(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_lock(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_lock_bh(spinlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_lock_bh(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_lock_bh(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_lock_irq(spinlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_lock_irq(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_lock_irq(rwlock_t *lock) ;
unsigned long __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_lock_irqsave(spinlock_t *lock)
                                                        ;
unsigned long __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_lock_irqsave(rwlock_t *lock)
                                                        ;
unsigned long __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_lock_irqsave(rwlock_t *lock)
                                                        ;
int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_trylock(spinlock_t *lock);
int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_trylock(rwlock_t *lock);
int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_trylock(rwlock_t *lock);
int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_trylock_bh(spinlock_t *lock);
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_unlock(spinlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_unlock(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_unlock(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_unlock_bh(spinlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_unlock_bh(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_unlock_bh(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_unlock_irq(spinlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_unlock_irq(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_unlock_irq(rwlock_t *lock) ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _spin_unlock_irqrestore(spinlock_t *lock, unsigned long flags)
                                                        ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _read_unlock_irqrestore(rwlock_t *lock, unsigned long flags)
                                                        ;
void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) _write_unlock_irqrestore(rwlock_t *lock, unsigned long flags)
                                                        ;
# 110 "include/linux/spinlock.h" 2
# 220 "include/linux/spinlock.h"
extern int _atomic_dec_and_lock(atomic_t *atomic, spinlock_t *lock);
# 46 "include/linux/capability.h" 2
# 57 "include/linux/capability.h"
typedef __u32 kernel_cap_t;
# 294 "include/linux/capability.h"
extern kernel_cap_t cap_bset;
# 322 "include/linux/capability.h"
static inline __attribute__((always_inline)) kernel_cap_t cap_combine(kernel_cap_t a, kernel_cap_t b)
{
     kernel_cap_t dest;
     (dest) = (a) | (b);
     return dest;
}

static inline __attribute__((always_inline)) kernel_cap_t cap_intersect(kernel_cap_t a, kernel_cap_t b)
{
     kernel_cap_t dest;
     (dest) = (a) & (b);
     return dest;
}

static inline __attribute__((always_inline)) kernel_cap_t cap_drop(kernel_cap_t a, kernel_cap_t drop)
{
     kernel_cap_t dest;
     (dest) = (a) & ~(drop);
     return dest;
}

static inline __attribute__((always_inline)) kernel_cap_t cap_invert(kernel_cap_t c)
{
     kernel_cap_t dest;
     (dest) = ~(c);
     return dest;
}
# 8 "include/linux/sched.h" 2



# 1 "include/linux/timex.h" 1
# 58 "include/linux/timex.h"
# 1 "include/linux/time.h" 1






# 1 "include/linux/seqlock.h" 1
# 33 "include/linux/seqlock.h"
typedef struct {
        unsigned sequence;
        spinlock_t lock;
} seqlock_t;
# 50 "include/linux/seqlock.h"
static inline __attribute__((always_inline)) void write_seqlock(seqlock_t *sl)
{
        _spin_lock(&sl->lock);
        ++sl->sequence;
        __asm__ __volatile__ ("": : :"memory");
}

static inline __attribute__((always_inline)) void write_sequnlock(seqlock_t *sl)
{
        __asm__ __volatile__ ("": : :"memory");
        sl->sequence++;
        _spin_unlock(&sl->lock);
}

static inline __attribute__((always_inline)) int write_tryseqlock(seqlock_t *sl)
{
        int ret = (_spin_trylock(&sl->lock));

        if (ret) {
                ++sl->sequence;
                __asm__ __volatile__ ("": : :"memory");
        }
        return ret;
}


static inline __attribute__((always_inline)) unsigned read_seqbegin(const seqlock_t *sl)
{
        unsigned ret = sl->sequence;
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return ret;
}
# 91 "include/linux/seqlock.h"
static inline __attribute__((always_inline)) int read_seqretry(const seqlock_t *sl, unsigned iv)
{
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return (iv & 1) | (sl->sequence ^ iv);
}
# 105 "include/linux/seqlock.h"
typedef struct seqcount {
        unsigned sequence;
} seqcount_t;





static inline __attribute__((always_inline)) unsigned read_seqcount_begin(const seqcount_t *s)
{
        unsigned ret = s->sequence;
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return ret;
}






static inline __attribute__((always_inline)) int read_seqcount_retry(const seqcount_t *s, unsigned iv)
{
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return (iv & 1) | (s->sequence ^ iv);
}






static inline __attribute__((always_inline)) void write_seqcount_begin(seqcount_t *s)
{
        s->sequence++;
        __asm__ __volatile__ ("": : :"memory");
}

static inline __attribute__((always_inline)) void write_seqcount_end(seqcount_t *s)
{
        __asm__ __volatile__ ("": : :"memory");
        s->sequence++;
}
# 8 "include/linux/time.h" 2




struct timespec {
        time_t tv_sec;
        long tv_nsec;
};


struct timeval {
        time_t tv_sec;
        suseconds_t tv_usec;
};

struct timezone {
        int tz_minuteswest;
        int tz_dsttime;
};
# 36 "include/linux/time.h"
static __inline__ __attribute__((always_inline)) int timespec_equal(struct timespec *a, struct timespec *b)
{
        return (a->tv_sec == b->tv_sec) && (a->tv_nsec == b->tv_nsec);
}
# 56 "include/linux/time.h"
static inline __attribute__((always_inline)) unsigned long
mktime (unsigned int year, unsigned int mon,
        unsigned int day, unsigned int hour,
        unsigned int min, unsigned int sec)
{
        if (0 >= (int) (mon -= 2)) {
                mon += 12;
                year -= 1;
        }

        return (((
                (unsigned long) (year/4 - year/100 + year/400 + 367*mon/12 + day) +
                        year*365 - 719499
            )*24 + hour
          )*60 + min
        )*60 + sec;
}

extern struct timespec xtime;
extern struct timespec wall_to_monotonic;
extern seqlock_t xtime_lock;

static inline __attribute__((always_inline)) unsigned long get_seconds(void)
{
        return xtime.tv_sec;
}

struct timespec current_kernel_time(void);




extern void do_gettimeofday(struct timeval *tv);
extern int do_settimeofday(struct timespec *tv);
extern int do_sys_settimeofday(struct timespec *tv, struct timezone *tz);
extern void clock_was_set(void);
extern int do_posix_clock_monotonic_gettime(struct timespec *tp);
extern long do_utimes(char * filename, struct timeval * times);
struct itimerval;
extern int do_setitimer(int which, struct itimerval *value, struct itimerval *ovalue);
extern int do_getitimer(int which, struct itimerval *value);
extern void getnstimeofday (struct timespec *tv);

extern struct timespec timespec_trunc(struct timespec t, unsigned gran);

static inline __attribute__((always_inline)) void
set_normalized_timespec (struct timespec *ts, time_t sec, long nsec)
{
        while (nsec > (1000000000L)) {
                nsec -= (1000000000L);
                ++sec;
        }
        while (nsec < 0) {
                nsec += (1000000000L);
                --sec;
        }
        ts->tv_sec = sec;
        ts->tv_nsec = nsec;
}
# 134 "include/linux/time.h"
struct itimerspec {
        struct timespec it_interval;
        struct timespec it_value;
};

struct itimerval {
        struct timeval it_interval;
        struct timeval it_value;
};
# 59 "include/linux/timex.h" 2

# 1 "include/asm/param.h" 1
# 61 "include/linux/timex.h" 2
# 1 "include/asm/timex.h" 1
# 33 "include/asm/timex.h"
typedef unsigned long long cycles_t;

static inline __attribute__((always_inline)) cycles_t get_cycles (void)
{
        unsigned long long ret=0;


        if (!(__builtin_constant_p((0*32+ 4)) ? constant_test_bit(((0*32+ 4)),(boot_cpu_data.x86_capability)) : variable_test_bit(((0*32+ 4)),(boot_cpu_data.x86_capability))))
                return 0;





        return ret;
}

extern unsigned int cpu_khz;

extern int read_current_timer(unsigned long *timer_value);
# 62 "include/linux/timex.h" 2
# 135 "include/linux/timex.h"
struct timex {
        unsigned int modes;
        long offset;
        long freq;
        long maxerror;
        long esterror;
        int status;
        long constant;
        long precision;
        long tolerance;


        struct timeval time;
        long tick;

        long ppsfreq;
        long jitter;
        int shift;
        long stabil;
        long jitcnt;
        long calcnt;
        long errcnt;
        long stbcnt;

        int :32; int :32; int :32; int :32;
        int :32; int :32; int :32; int :32;
        int :32; int :32; int :32; int :32;
};
# 227 "include/linux/timex.h"
extern unsigned long tick_usec;
extern unsigned long tick_nsec;
extern int tickadj;




extern int time_state;
extern int time_status;
extern long time_offset;
extern long time_constant;
extern long time_tolerance;
extern long time_precision;
extern long time_maxerror;
extern long time_esterror;

extern long time_freq;
extern long time_reftime;

extern long time_adjust;
extern long time_next_adjust;


extern long pps_offset;
extern long pps_jitter;
extern long pps_freq;
extern long pps_stabil;
extern long pps_valid;


extern int pps_shift;
extern long pps_jitcnt;
extern long pps_calcnt;
extern long pps_errcnt;
extern long pps_stbcnt;






static inline __attribute__((always_inline)) void ntp_clear(void)
{
        time_adjust = 0;
        time_status |= 0x0040;
        time_maxerror = (512000L << 5);
        time_esterror = (512000L << 5);
}





static inline __attribute__((always_inline)) int ntp_synced(void)
{
        return !(time_status & 0x0040);
}
# 334 "include/linux/timex.h"
static inline __attribute__((always_inline)) void
time_interpolator_reset(void)
{
}
# 12 "include/linux/sched.h" 2
# 1 "include/linux/jiffies.h" 1







# 1 "include/asm/param.h" 1
# 9 "include/linux/jiffies.h" 2
# 1 "include/asm/div64.h" 1
# 38 "include/asm/div64.h"
static inline __attribute__((always_inline)) long
div_ll_X_l_rem(long long divs, long div, long *rem)
{
        long dum2;
      __asm__("divl %2":"=a"(dum2), "=d"(*rem)
      : "rm"(div), "A"(divs));

        return dum2;

}
# 10 "include/linux/jiffies.h" 2
# 84 "include/linux/jiffies.h"
extern u64 __attribute__((section(".data"))) jiffies_64;
extern unsigned long volatile __attribute__((section(".data"))) jiffies;


u64 get_jiffies_64(void);
# 255 "include/linux/jiffies.h"
static inline __attribute__((always_inline)) unsigned int jiffies_to_msecs(const unsigned long j)
{

        return ((1000L) / 250) * j;





}

static inline __attribute__((always_inline)) unsigned int jiffies_to_usecs(const unsigned long j)
{

        return ((1000000L) / 250) * j;





}

static inline __attribute__((always_inline)) unsigned long msecs_to_jiffies(const unsigned int m)
{
        if (m > jiffies_to_msecs(((~0UL >> 1)-1)))
                return ((~0UL >> 1)-1);

        return (m + ((1000L) / 250) - 1) / ((1000L) / 250);





}

static inline __attribute__((always_inline)) unsigned long usecs_to_jiffies(const unsigned int u)
{
        if (u > jiffies_to_usecs(((~0UL >> 1)-1)))
                return ((~0UL >> 1)-1);

        return (u + ((1000000L) / 250) - 1) / ((1000000L) / 250);





}
# 314 "include/linux/jiffies.h"
static __inline__ __attribute__((always_inline)) unsigned long
timespec_to_jiffies(const struct timespec *value)
{
        unsigned long sec = value->tv_sec;
        long nsec = value->tv_nsec + (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))))) - 1;

        if (sec >= (long)((u64)((u64)((~0UL >> 1)-1) * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))))) / (1000000000L))){
                sec = (long)((u64)((u64)((~0UL >> 1)-1) * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))))) / (1000000000L));
                nsec = 0;
        }
        return (((u64)sec * ((unsigned long)((((u64)(1000000000L) << (32 - 8)) + (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))))) -1) / (u64)(( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))))))) +
                (((u64)nsec * ((unsigned long)((((u64)1 << ((32 - 8) + 29)) + (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))))) -1) / (u64)(( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))))))) >>
                 (((32 - 8) + 29) - (32 - 8)))) >> (32 - 8);

}

static __inline__ __attribute__((always_inline)) void
jiffies_to_timespec(const unsigned long jiffies, struct timespec *value)
{




        u64 nsec = (u64)jiffies * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        value->tv_sec = div_ll_X_l_rem(nsec,(1000000000L),&value->tv_nsec);
}
# 353 "include/linux/jiffies.h"
static __inline__ __attribute__((always_inline)) unsigned long
timeval_to_jiffies(const struct timeval *value)
{
        unsigned long sec = value->tv_sec;
        long usec = value->tv_usec;

        if (sec >= (long)((u64)((u64)((~0UL >> 1)-1) * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))))) / (1000000000L))){
                sec = (long)((u64)((u64)((~0UL >> 1)-1) * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))))) / (1000000000L));
                usec = 0;
        }
        return (((u64)sec * ((unsigned long)((((u64)(1000000000L) << (32 - 8)) + (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))))) -1) / (u64)(( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))))))) +
                (((u64)usec * ((unsigned long)((((u64)(1000L) << ((32 - 8) + 19)) + (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))))) -1) / (u64)(( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))))))) + (u64)(((u64)1 << ((32 - 8) + 19)) - 1)) >>
                 (((32 - 8) + 19) - (32 - 8)))) >> (32 - 8);
}

static __inline__ __attribute__((always_inline)) void
jiffies_to_timeval(const unsigned long jiffies, struct timeval *value)
{




        u64 nsec = (u64)jiffies * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        value->tv_sec = div_ll_X_l_rem(nsec,(1000000000L),&value->tv_usec);
        value->tv_usec /= (1000L);
}




static inline __attribute__((always_inline)) clock_t jiffies_to_clock_t(long x)
{



        u64 tmp = (u64)x * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (((1000000000L) / 100)); asm("":"=a" (__low), "=d" (__high):"A" (tmp)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (tmp):"a" (__low),"d" (__high)); __mod; });
        return (long)tmp;

}

static inline __attribute__((always_inline)) unsigned long clock_t_to_jiffies(unsigned long x)
{





        u64 jif;


        if (x >= ~0UL / 250 * 100)
                return ~0UL;


        jif = x * (u64) 250;
        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (100); asm("":"=a" (__low), "=d" (__high):"A" (jif)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (jif):"a" (__low),"d" (__high)); __mod; });
        return jif;

}

static inline __attribute__((always_inline)) u64 jiffies_64_to_clock_t(u64 x)
{
# 424 "include/linux/jiffies.h"
        x *= (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (((1000000000L) / 100)); asm("":"=a" (__low), "=d" (__high):"A" (x)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (x):"a" (__low),"d" (__high)); __mod; });

        return x;
}

static inline __attribute__((always_inline)) u64 nsec_to_clock_t(u64 x)
{

        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (((1000000000L) / 100)); asm("":"=a" (__low), "=d" (__high):"A" (x)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (x):"a" (__low),"d" (__high)); __mod; });
# 447 "include/linux/jiffies.h"
        return x;
}
# 13 "include/linux/sched.h" 2
# 1 "include/linux/rbtree.h" 1
# 100 "include/linux/rbtree.h"
struct rb_node
{
        struct rb_node *rb_parent;
        int rb_color;


        struct rb_node *rb_right;
        struct rb_node *rb_left;
};

struct rb_root
{
        struct rb_node *rb_node;
};




extern void rb_insert_color(struct rb_node *, struct rb_root *);
extern void rb_erase(struct rb_node *, struct rb_root *);


extern struct rb_node *rb_next(struct rb_node *);
extern struct rb_node *rb_prev(struct rb_node *);
extern struct rb_node *rb_first(struct rb_root *);
extern struct rb_node *rb_last(struct rb_root *);


extern void rb_replace_node(struct rb_node *victim, struct rb_node *new,
                            struct rb_root *root);

static inline __attribute__((always_inline)) void rb_link_node(struct rb_node * node, struct rb_node * parent,
                                struct rb_node ** rb_link)
{
        node->rb_parent = parent;
        node->rb_color = 0;
        node->rb_left = node->rb_right = ((void *)0);

        *rb_link = node;
}
# 14 "include/linux/sched.h" 2

# 1 "include/linux/cpumask.h" 1
# 82 "include/linux/cpumask.h"
# 1 "include/linux/bitmap.h" 1







# 1 "include/linux/string.h" 1
# 16 "include/linux/string.h"
extern char * strpbrk(const char *,const char *);
extern char * strsep(char **,const char *);
extern __kernel_size_t strspn(const char *,const char *);
extern __kernel_size_t strcspn(const char *,const char *);




# 1 "include/asm/string.h" 1
# 30 "include/asm/string.h"
static inline __attribute__((always_inline)) char * strcpy(char * dest,const char *src)
{
int d0, d1, d2;
__asm__ __volatile__(
        "1:\tlodsb\n\t"
        "stosb\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b"
        : "=&S" (d0), "=&D" (d1), "=&a" (d2)
        :"0" (src),"1" (dest) : "memory");
return dest;
}


static inline __attribute__((always_inline)) char * strncpy(char * dest,const char *src,size_t count)
{
int d0, d1, d2, d3;
__asm__ __volatile__(
        "1:\tdecl %2\n\t"
        "js 2f\n\t"
        "lodsb\n\t"
        "stosb\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b\n\t"
        "rep\n\t"
        "stosb\n"
        "2:"
        : "=&S" (d0), "=&D" (d1), "=&c" (d2), "=&a" (d3)
        :"0" (src),"1" (dest),"2" (count) : "memory");
return dest;
}


static inline __attribute__((always_inline)) char * strcat(char * dest,const char * src)
{
int d0, d1, d2, d3;
__asm__ __volatile__(
        "repne\n\t"
        "scasb\n\t"
        "decl %1\n"
        "1:\tlodsb\n\t"
        "stosb\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b"
        : "=&S" (d0), "=&D" (d1), "=&a" (d2), "=&c" (d3)
        : "0" (src), "1" (dest), "2" (0), "3" (0xffffffffu):"memory");
return dest;
}


static inline __attribute__((always_inline)) char * strncat(char * dest,const char * src,size_t count)
{
int d0, d1, d2, d3;
__asm__ __volatile__(
        "repne\n\t"
        "scasb\n\t"
        "decl %1\n\t"
        "movl %8,%3\n"
        "1:\tdecl %3\n\t"
        "js 2f\n\t"
        "lodsb\n\t"
        "stosb\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b\n"
        "2:\txorl %2,%2\n\t"
        "stosb"
        : "=&S" (d0), "=&D" (d1), "=&a" (d2), "=&c" (d3)
        : "0" (src),"1" (dest),"2" (0),"3" (0xffffffffu), "g" (count)
        : "memory");
return dest;
}


static inline __attribute__((always_inline)) int strcmp(const char * cs,const char * ct)
{
int d0, d1;
register int __res;
__asm__ __volatile__(
        "1:\tlodsb\n\t"
        "scasb\n\t"
        "jne 2f\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b\n\t"
        "xorl %%eax,%%eax\n\t"
        "jmp 3f\n"
        "2:\tsbbl %%eax,%%eax\n\t"
        "orb $1,%%al\n"
        "3:"
        :"=a" (__res), "=&S" (d0), "=&D" (d1)
        :"1" (cs),"2" (ct)
        :"memory");
return __res;
}


static inline __attribute__((always_inline)) int strncmp(const char * cs,const char * ct,size_t count)
{
register int __res;
int d0, d1, d2;
__asm__ __volatile__(
        "1:\tdecl %3\n\t"
        "js 2f\n\t"
        "lodsb\n\t"
        "scasb\n\t"
        "jne 3f\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b\n"
        "2:\txorl %%eax,%%eax\n\t"
        "jmp 4f\n"
        "3:\tsbbl %%eax,%%eax\n\t"
        "orb $1,%%al\n"
        "4:"
        :"=a" (__res), "=&S" (d0), "=&D" (d1), "=&c" (d2)
        :"1" (cs),"2" (ct),"3" (count)
        :"memory");
return __res;
}


static inline __attribute__((always_inline)) char * strchr(const char * s, int c)
{
int d0;
register char * __res;
__asm__ __volatile__(
        "movb %%al,%%ah\n"
        "1:\tlodsb\n\t"
        "cmpb %%ah,%%al\n\t"
        "je 2f\n\t"
        "testb %%al,%%al\n\t"
        "jne 1b\n\t"
        "movl $1,%1\n"
        "2:\tmovl %1,%0\n\t"
        "decl %0"
        :"=a" (__res), "=&S" (d0)
        :"1" (s),"0" (c)
        :"memory");
return __res;
}


static inline __attribute__((always_inline)) char * strrchr(const char * s, int c)
{
int d0, d1;
register char * __res;
__asm__ __volatile__(
        "movb %%al,%%ah\n"
        "1:\tlodsb\n\t"
        "cmpb %%ah,%%al\n\t"
        "jne 2f\n\t"
        "leal -1(%%esi),%0\n"
        "2:\ttestb %%al,%%al\n\t"
        "jne 1b"
        :"=g" (__res), "=&S" (d0), "=&a" (d1)
        :"0" (0),"1" (s),"2" (c)
        :"memory");
return __res;
}


static inline __attribute__((always_inline)) size_t strlen(const char * s)
{
int d0;
register int __res;
__asm__ __volatile__(
        "repne\n\t"
        "scasb\n\t"
        "notl %0\n\t"
        "decl %0"
        :"=c" (__res), "=&D" (d0)
        :"1" (s),"a" (0), "0" (0xffffffffu)
        :"memory");
return __res;
}

static inline __attribute__((always_inline)) void * __memcpy(void * to, const void * from, size_t n)
{
int d0, d1, d2;
__asm__ __volatile__(
        "rep ; movsl\n\t"
        "movl %4,%%ecx\n\t"
        "andl $3,%%ecx\n\t"

        "jz 1f\n\t"

        "rep ; movsb\n\t"
        "1:"
        : "=&c" (d0), "=&D" (d1), "=&S" (d2)
        : "0" (n/4), "g" (n), "1" ((long) to), "2" ((long) from)
        : "memory");
return (to);
}





static inline __attribute__((always_inline)) void * __constant_memcpy(void * to, const void * from, size_t n)
{
        long esi, edi;
        if (!n) return to;

        switch (n) {
                case 1: *(char*)to = *(char*)from; return to;
                case 2: *(short*)to = *(short*)from; return to;
                case 4: *(int*)to = *(int*)from; return to;

                case 3: *(short*)to = *(short*)from;
                        *((char*)to+2) = *((char*)from+2); return to;
                case 5: *(int*)to = *(int*)from;
                        *((char*)to+4) = *((char*)from+4); return to;
                case 6: *(int*)to = *(int*)from;
                        *((short*)to+2) = *((short*)from+2); return to;
                case 8: *(int*)to = *(int*)from;
                        *((int*)to+1) = *((int*)from+1); return to;

        }

        esi = (long) from;
        edi = (long) to;
        if (n >= 5*4) {

                int ecx;
                __asm__ __volatile__(
                        "rep ; movsl"
                        : "=&c" (ecx), "=&D" (edi), "=&S" (esi)
                        : "0" (n/4), "1" (edi),"2" (esi)
                        : "memory"
                );
        } else {

                if (n >= 4*4) __asm__ __volatile__("movsl"
                        :"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
                if (n >= 3*4) __asm__ __volatile__("movsl"
                        :"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
                if (n >= 2*4) __asm__ __volatile__("movsl"
                        :"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
                if (n >= 1*4) __asm__ __volatile__("movsl"
                        :"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
        }
        switch (n % 4) {

                case 0: return to;
                case 1: __asm__ __volatile__("movsb"
                        :"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
                        return to;
                case 2: __asm__ __volatile__("movsw"
                        :"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
                        return to;
                default: __asm__ __volatile__("movsw\n\tmovsb"
                        :"=&D"(edi),"=&S"(esi):"0"(edi),"1"(esi):"memory");
                        return to;
        }
}
# 327 "include/asm/string.h"
void *memmove(void * dest,const void * src, size_t n);




static inline __attribute__((always_inline)) void * memchr(const void * cs,int c,size_t count)
{
int d0;
register void * __res;
if (!count)
        return ((void *)0);
__asm__ __volatile__(
        "repne\n\t"
        "scasb\n\t"
        "je 1f\n\t"
        "movl $1,%0\n"
        "1:\tdecl %0"
        :"=D" (__res), "=&c" (d0)
        :"a" (c),"0" (cs),"1" (count)
        :"memory");
return __res;
}

static inline __attribute__((always_inline)) void * __memset_generic(void * s, char c,size_t count)
{
int d0, d1;
__asm__ __volatile__(
        "rep\n\t"
        "stosb"
        : "=&c" (d0), "=&D" (d1)
        :"a" (c),"1" (s),"0" (count)
        :"memory");
return s;
}
# 370 "include/asm/string.h"
static inline __attribute__((always_inline)) void * __constant_c_memset(void * s, unsigned long c, size_t count)
{
int d0, d1;
__asm__ __volatile__(
        "rep ; stosl\n\t"
        "testb $2,%b3\n\t"
        "je 1f\n\t"
        "stosw\n"
        "1:\ttestb $1,%b3\n\t"
        "je 2f\n\t"
        "stosb\n"
        "2:"
        :"=&c" (d0), "=&D" (d1)
        :"a" (c), "q" (count), "0" (count/4), "1" ((long) s)
        :"memory");
return (s);
}



static inline __attribute__((always_inline)) size_t strnlen(const char * s, size_t count)
{
int d0;
register int __res;
__asm__ __volatile__(
        "movl %2,%0\n\t"
        "jmp 2f\n"
        "1:\tcmpb $0,(%0)\n\t"
        "je 3f\n\t"
        "incl %0\n"
        "2:\tdecl %1\n\t"
        "cmpl $-1,%1\n\t"
        "jne 1b\n"
        "3:\tsubl %2,%0"
        :"=a" (__res), "=&d" (d0)
        :"c" (s),"1" (count)
        :"memory");
return __res;
}




extern char *strstr(const char *cs, const char *ct);





static inline __attribute__((always_inline)) void * __constant_c_and_count_memset(void * s, unsigned long pattern, size_t count)
{
        switch (count) {
                case 0:
                        return s;
                case 1:
                        *(unsigned char *)s = pattern;
                        return s;
                case 2:
                        *(unsigned short *)s = pattern;
                        return s;
                case 3:
                        *(unsigned short *)s = pattern;
                        *(2+(unsigned char *)s) = pattern;
                        return s;
                case 4:
                        *(unsigned long *)s = pattern;
                        return s;
        }







{
        int d0, d1;
        switch (count % 4) {
                case 0: __asm__ __volatile__( "rep ; stosl" "" : "=&c" (d0), "=&D" (d1) : "a" (pattern),"0" (count/4),"1" ((long) s) : "memory"); return s;
                case 1: __asm__ __volatile__( "rep ; stosl" "\n\tstosb" : "=&c" (d0), "=&D" (d1) : "a" (pattern),"0" (count/4),"1" ((long) s) : "memory"); return s;
                case 2: __asm__ __volatile__( "rep ; stosl" "\n\tstosw" : "=&c" (d0), "=&D" (d1) : "a" (pattern),"0" (count/4),"1" ((long) s) : "memory"); return s;
                default: __asm__ __volatile__( "rep ; stosl" "\n\tstosw\n\tstosb" : "=&c" (d0), "=&D" (d1) : "a" (pattern),"0" (count/4),"1" ((long) s) : "memory"); return s;
        }
}


}
# 478 "include/asm/string.h"
static inline __attribute__((always_inline)) void * memscan(void * addr, int c, size_t size)
{
        if (!size)
                return addr;
        __asm__("repnz; scasb\n\t"
                "jnz 1f\n\t"
                "dec %%edi\n"
                "1:"
                : "=D" (addr), "=c" (size)
                : "0" (addr), "1" (size), "a" (c)
                : "memory");
        return addr;
}
# 25 "include/linux/string.h" 2
# 33 "include/linux/string.h"
size_t strlcpy(char *, const char *, size_t);
# 42 "include/linux/string.h"
extern size_t strlcat(char *, const char *, __kernel_size_t);
# 51 "include/linux/string.h"
extern int strnicmp(const char *, const char *, __kernel_size_t);





extern char * strnchr(const char *, size_t, int);
# 85 "include/linux/string.h"
extern int __builtin_memcmp(const void *,const void *,__kernel_size_t);





extern char *kstrdup(const char *s, gfp_t gfp);
# 9 "include/linux/bitmap.h" 2
# 75 "include/linux/bitmap.h"
extern int __bitmap_empty(const unsigned long *bitmap, int bits);
extern int __bitmap_full(const unsigned long *bitmap, int bits);
extern int __bitmap_equal(const unsigned long *bitmap1,
                        const unsigned long *bitmap2, int bits);
extern void __bitmap_complement(unsigned long *dst, const unsigned long *src,
                        int bits);
extern void __bitmap_shift_right(unsigned long *dst,
                        const unsigned long *src, int shift, int bits);
extern void __bitmap_shift_left(unsigned long *dst,
                        const unsigned long *src, int shift, int bits);
extern void __bitmap_and(unsigned long *dst, const unsigned long *bitmap1,
                        const unsigned long *bitmap2, int bits);
extern void __bitmap_or(unsigned long *dst, const unsigned long *bitmap1,
                        const unsigned long *bitmap2, int bits);
extern void __bitmap_xor(unsigned long *dst, const unsigned long *bitmap1,
                        const unsigned long *bitmap2, int bits);
extern void __bitmap_andnot(unsigned long *dst, const unsigned long *bitmap1,
                        const unsigned long *bitmap2, int bits);
extern int __bitmap_intersects(const unsigned long *bitmap1,
                        const unsigned long *bitmap2, int bits);
extern int __bitmap_subset(const unsigned long *bitmap1,
                        const unsigned long *bitmap2, int bits);
extern int __bitmap_weight(const unsigned long *bitmap, int bits);

extern int bitmap_scnprintf(char *buf, unsigned int len,
                        const unsigned long *src, int nbits);
extern int bitmap_parse(const char *ubuf, unsigned int ulen,
                        unsigned long *dst, int nbits);
extern int bitmap_scnlistprintf(char *buf, unsigned int len,
                        const unsigned long *src, int nbits);
extern int bitmap_parselist(const char *buf, unsigned long *maskp,
                        int nmaskbits);
extern int bitmap_find_free_region(unsigned long *bitmap, int bits, int order);
extern void bitmap_release_region(unsigned long *bitmap, int pos, int order);
extern int bitmap_allocate_region(unsigned long *bitmap, int pos, int order);







static inline __attribute__((always_inline)) void bitmap_zero(unsigned long *dst, int nbits)
{
        if (nbits <= 32)
                *dst = 0UL;
        else {
                int len = (((nbits)+32 -1)/32) * sizeof(unsigned long);
                (__builtin_constant_p(0) ? (__builtin_constant_p((len)) ? __constant_c_and_count_memset(((dst)),((0x01010101UL*(unsigned char)(0))),((len))) : __constant_c_memset(((dst)),((0x01010101UL*(unsigned char)(0))),((len)))) : (__builtin_constant_p((len)) ? __memset_generic((((dst))),(((0))),(((len)))) : __memset_generic(((dst)),((0)),((len)))));
        }
}

static inline __attribute__((always_inline)) void bitmap_fill(unsigned long *dst, int nbits)
{
        size_t nlongs = (((nbits)+32 -1)/32);
        if (nlongs > 1) {
                int len = (nlongs - 1) * sizeof(unsigned long);
                (__builtin_constant_p(0xff) ? (__builtin_constant_p((len)) ? __constant_c_and_count_memset(((dst)),((0x01010101UL*(unsigned char)(0xff))),((len))) : __constant_c_memset(((dst)),((0x01010101UL*(unsigned char)(0xff))),((len)))) : (__builtin_constant_p((len)) ? __memset_generic((((dst))),(((0xff))),(((len)))) : __memset_generic(((dst)),((0xff)),((len)))));
        }
        dst[nlongs - 1] = ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL );
}

static inline __attribute__((always_inline)) void bitmap_copy(unsigned long *dst, const unsigned long *src,
                        int nbits)
{
        if (nbits <= 32)
                *dst = *src;
        else {
                int len = (((nbits)+32 -1)/32) * sizeof(unsigned long);
                (__builtin_constant_p(len) ? __constant_memcpy((dst),(src),(len)) : __memcpy((dst),(src),(len)));
        }
}

static inline __attribute__((always_inline)) void bitmap_and(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 & *src2;
        else
                __bitmap_and(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_or(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 | *src2;
        else
                __bitmap_or(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_xor(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 ^ *src2;
        else
                __bitmap_xor(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_andnot(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 & ~(*src2);
        else
                __bitmap_andnot(dst, src1, src2, nbits);
}

static inline __attribute__((always_inline)) void bitmap_complement(unsigned long *dst, const unsigned long *src,
                        int nbits)
{
        if (nbits <= 32)
                *dst = ~(*src) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL );
        else
                __bitmap_complement(dst, src, nbits);
}

static inline __attribute__((always_inline)) int bitmap_equal(const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                return ! ((*src1 ^ *src2) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_equal(src1, src2, nbits);
}

static inline __attribute__((always_inline)) int bitmap_intersects(const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                return ((*src1 & *src2) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL )) != 0;
        else
                return __bitmap_intersects(src1, src2, nbits);
}

static inline __attribute__((always_inline)) int bitmap_subset(const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                return ! ((*src1 & ~(*src2)) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_subset(src1, src2, nbits);
}

static inline __attribute__((always_inline)) int bitmap_empty(const unsigned long *src, int nbits)
{
        if (nbits <= 32)
                return ! (*src & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_empty(src, nbits);
}

static inline __attribute__((always_inline)) int bitmap_full(const unsigned long *src, int nbits)
{
        if (nbits <= 32)
                return ! (~(*src) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_full(src, nbits);
}

static inline __attribute__((always_inline)) int bitmap_weight(const unsigned long *src, int nbits)
{
        return __bitmap_weight(src, nbits);
}

static inline __attribute__((always_inline)) void bitmap_shift_right(unsigned long *dst,
                        const unsigned long *src, int n, int nbits)
{
        if (nbits <= 32)
                *dst = *src >> n;
        else
                __bitmap_shift_right(dst, src, n, nbits);
}

static inline __attribute__((always_inline)) void bitmap_shift_left(unsigned long *dst,
                        const unsigned long *src, int n, int nbits)
{
        if (nbits <= 32)
                *dst = (*src << n) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL );
        else
                __bitmap_shift_left(dst, src, n, nbits);
}
# 83 "include/linux/cpumask.h" 2


typedef struct { unsigned long bits[(((8)+32 -1)/32)]; } cpumask_t;
extern cpumask_t _unused_cpumask_arg_;


static inline __attribute__((always_inline)) void __cpu_set(int cpu, volatile cpumask_t *dstp)
{
        set_bit(cpu, dstp->bits);
}


static inline __attribute__((always_inline)) void __cpu_clear(int cpu, volatile cpumask_t *dstp)
{
        clear_bit(cpu, dstp->bits);
}


static inline __attribute__((always_inline)) void __cpus_setall(cpumask_t *dstp, int nbits)
{
        bitmap_fill(dstp->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_clear(cpumask_t *dstp, int nbits)
{
        bitmap_zero(dstp->bits, nbits);
}





static inline __attribute__((always_inline)) int __cpu_test_and_set(int cpu, cpumask_t *addr)
{
        return test_and_set_bit(cpu, addr->bits);
}


static inline __attribute__((always_inline)) void __cpus_and(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_and(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_or(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_or(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_xor(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_xor(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __cpus_andnot(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_andnot(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) void __cpus_complement(cpumask_t *dstp,
                                        const cpumask_t *srcp, int nbits)
{
        bitmap_complement(dstp->bits, srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_equal(const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        return bitmap_equal(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_intersects(const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        return bitmap_intersects(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_subset(const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        return bitmap_subset(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_empty(const cpumask_t *srcp, int nbits)
{
        return bitmap_empty(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_full(const cpumask_t *srcp, int nbits)
{
        return bitmap_full(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpus_weight(const cpumask_t *srcp, int nbits)
{
        return bitmap_weight(srcp->bits, nbits);
}



static inline __attribute__((always_inline)) void __cpus_shift_right(cpumask_t *dstp,
                                        const cpumask_t *srcp, int n, int nbits)
{
        bitmap_shift_right(dstp->bits, srcp->bits, n, nbits);
}



static inline __attribute__((always_inline)) void __cpus_shift_left(cpumask_t *dstp,
                                        const cpumask_t *srcp, int n, int nbits)
{
        bitmap_shift_left(dstp->bits, srcp->bits, n, nbits);
}


static inline __attribute__((always_inline)) int __first_cpu(const cpumask_t *srcp, int nbits)
{
        return ({ int __x = (nbits); int __y = (find_first_bit(srcp->bits, nbits)); __x < __y ? __x: __y; });
}


static inline __attribute__((always_inline)) int __next_cpu(int n, const cpumask_t *srcp, int nbits)
{
        return ({ int __x = (nbits); int __y = (find_next_bit(srcp->bits, nbits, n+1)); __x < __y ? __x: __y; });
}
# 269 "include/linux/cpumask.h"
static inline __attribute__((always_inline)) int __cpumask_scnprintf(char *buf, int len,
                                        const cpumask_t *srcp, int nbits)
{
        return bitmap_scnprintf(buf, len, srcp->bits, nbits);
}



static inline __attribute__((always_inline)) int __cpumask_parse(const char *buf, int len,
                                        cpumask_t *dstp, int nbits)
{
        return bitmap_parse(buf, len, dstp->bits, nbits);
}



static inline __attribute__((always_inline)) int __cpulist_scnprintf(char *buf, int len,
                                        const cpumask_t *srcp, int nbits)
{
        return bitmap_scnlistprintf(buf, len, srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __cpulist_parse(const char *buf, cpumask_t *dstp, int nbits)
{
        return bitmap_parselist(buf, dstp->bits, nbits);
}
# 362 "include/linux/cpumask.h"
extern cpumask_t cpu_possible_map;
extern cpumask_t cpu_online_map;
extern cpumask_t cpu_present_map;
# 16 "include/linux/sched.h" 2
# 1 "include/linux/errno.h" 1



# 1 "include/asm/errno.h" 1



# 1 "include/asm-generic/errno.h" 1



# 1 "include/asm-generic/errno-base.h" 1
# 5 "include/asm-generic/errno.h" 2
# 5 "include/asm/errno.h" 2
# 5 "include/linux/errno.h" 2
# 17 "include/linux/sched.h" 2
# 1 "include/linux/nodemask.h" 1
# 82 "include/linux/nodemask.h"
# 1 "include/linux/numa.h" 1
# 83 "include/linux/nodemask.h" 2


typedef struct { unsigned long bits[((((1 << 0))+32 -1)/32)]; } nodemask_t;
extern nodemask_t _unused_nodemask_arg_;


static inline __attribute__((always_inline)) void __node_set(int node, volatile nodemask_t *dstp)
{
        set_bit(node, dstp->bits);
}


static inline __attribute__((always_inline)) void __node_clear(int node, volatile nodemask_t *dstp)
{
        clear_bit(node, dstp->bits);
}


static inline __attribute__((always_inline)) void __nodes_setall(nodemask_t *dstp, int nbits)
{
        bitmap_fill(dstp->bits, nbits);
}


static inline __attribute__((always_inline)) void __nodes_clear(nodemask_t *dstp, int nbits)
{
        bitmap_zero(dstp->bits, nbits);
}






static inline __attribute__((always_inline)) int __node_test_and_set(int node, nodemask_t *addr)
{
        return test_and_set_bit(node, addr->bits);
}



static inline __attribute__((always_inline)) void __nodes_and(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_and(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_or(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_or(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_xor(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_xor(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_andnot(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_andnot(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_complement(nodemask_t *dstp,
                                        const nodemask_t *srcp, int nbits)
{
        bitmap_complement(dstp->bits, srcp->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodes_equal(const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        return bitmap_equal(src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodes_intersects(const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        return bitmap_intersects(src1p->bits, src2p->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodes_subset(const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        return bitmap_subset(src1p->bits, src2p->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodes_empty(const nodemask_t *srcp, int nbits)
{
        return bitmap_empty(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodes_full(const nodemask_t *srcp, int nbits)
{
        return bitmap_full(srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodes_weight(const nodemask_t *srcp, int nbits)
{
        return bitmap_weight(srcp->bits, nbits);
}



static inline __attribute__((always_inline)) void __nodes_shift_right(nodemask_t *dstp,
                                        const nodemask_t *srcp, int n, int nbits)
{
        bitmap_shift_right(dstp->bits, srcp->bits, n, nbits);
}



static inline __attribute__((always_inline)) void __nodes_shift_left(nodemask_t *dstp,
                                        const nodemask_t *srcp, int n, int nbits)
{
        bitmap_shift_left(dstp->bits, srcp->bits, n, nbits);
}





static inline __attribute__((always_inline)) int __first_node(const nodemask_t *srcp)
{
        return ({ int __x = ((1 << 0)); int __y = (find_first_bit(srcp->bits, (1 << 0))); __x < __y ? __x: __y; });
}


static inline __attribute__((always_inline)) int __next_node(int n, const nodemask_t *srcp)
{
        return ({ int __x = ((1 << 0)); int __y = (find_next_bit(srcp->bits, (1 << 0), n+1)); __x < __y ? __x: __y; });
}
# 248 "include/linux/nodemask.h"
static inline __attribute__((always_inline)) int __first_unset_node(const nodemask_t *maskp)
{
        return ({ int __x = ((1 << 0)); int __y = (find_first_zero_bit(maskp->bits, (1 << 0))); __x < __y ? __x: __y; });

}
# 282 "include/linux/nodemask.h"
static inline __attribute__((always_inline)) int __nodemask_scnprintf(char *buf, int len,
                                        const nodemask_t *srcp, int nbits)
{
        return bitmap_scnprintf(buf, len, srcp->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodemask_parse(const char *buf, int len,
                                        nodemask_t *dstp, int nbits)
{
        return bitmap_parse(buf, len, dstp->bits, nbits);
}



static inline __attribute__((always_inline)) int __nodelist_scnprintf(char *buf, int len,
                                        const nodemask_t *srcp, int nbits)
{
        return bitmap_scnlistprintf(buf, len, srcp->bits, nbits);
}


static inline __attribute__((always_inline)) int __nodelist_parse(const char *buf, nodemask_t *dstp, int nbits)
{
        return bitmap_parselist(buf, dstp->bits, nbits);
}
# 326 "include/linux/nodemask.h"
extern nodemask_t node_online_map;
extern nodemask_t node_possible_map;
# 18 "include/linux/sched.h" 2


# 1 "include/asm/semaphore.h" 1
# 41 "include/asm/semaphore.h"
# 1 "include/linux/wait.h" 1
# 23 "include/linux/wait.h"
# 1 "include/linux/list.h" 1






# 1 "include/linux/prefetch.h" 1
# 58 "include/linux/prefetch.h"
static inline __attribute__((always_inline)) void prefetch_range(void *addr, size_t len)
{

        char *cp;
        char *end = addr + len;

        for (cp = addr; cp < end; cp += (4*(1 << (4))))
                prefetch(cp);

}
# 8 "include/linux/list.h" 2
# 28 "include/linux/list.h"
struct list_head {
        struct list_head *next, *prev;
};
# 47 "include/linux/list.h"
static inline __attribute__((always_inline)) void __list_add(struct list_head *new,
                              struct list_head *prev,
                              struct list_head *next)
{
        next->prev = new;
        new->next = next;
        new->prev = prev;
        prev->next = new;
}
# 65 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_add(struct list_head *new, struct list_head *head)
{
        __list_add(new, head, head->next);
}
# 78 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_add_tail(struct list_head *new, struct list_head *head)
{
        __list_add(new, head->prev, head);
}







static inline __attribute__((always_inline)) void __list_add_rcu(struct list_head * new,
                struct list_head * prev, struct list_head * next)
{
        new->next = next;
        new->prev = prev;
        __asm__ __volatile__ ("": : :"memory");
        next->prev = new;
        prev->next = new;
}
# 115 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_add_rcu(struct list_head *new, struct list_head *head)
{
        __list_add_rcu(new, head, head->next);
}
# 136 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_add_tail_rcu(struct list_head *new,
                                        struct list_head *head)
{
        __list_add_rcu(new, head->prev, head);
}
# 149 "include/linux/list.h"
static inline __attribute__((always_inline)) void __list_del(struct list_head * prev, struct list_head * next)
{
        next->prev = prev;
        prev->next = next;
}







static inline __attribute__((always_inline)) void list_del(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
        entry->next = ((void *) 0x00100100);
        entry->prev = ((void *) 0x00200200);
}
# 192 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_del_rcu(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
        entry->prev = ((void *) 0x00200200);
}
# 205 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_replace_rcu(struct list_head *old, struct list_head *new){
        new->next = old->next;
        new->prev = old->prev;
        __asm__ __volatile__ ("": : :"memory");
        new->next->prev = new;
        new->prev->next = new;
}





static inline __attribute__((always_inline)) void list_del_init(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
        do { (entry)->next = (entry); (entry)->prev = (entry); } while (0);
}






static inline __attribute__((always_inline)) void list_move(struct list_head *list, struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add(list, head);
}






static inline __attribute__((always_inline)) void list_move_tail(struct list_head *list,
                                  struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add_tail(list, head);
}





static inline __attribute__((always_inline)) int list_empty(const struct list_head *head)
{
        return head->next == head;
}
# 267 "include/linux/list.h"
static inline __attribute__((always_inline)) int list_empty_careful(const struct list_head *head)
{
        struct list_head *next = head->next;
        return (next == head) && (next == head->prev);
}

static inline __attribute__((always_inline)) void __list_splice(struct list_head *list,
                                 struct list_head *head)
{
        struct list_head *first = list->next;
        struct list_head *last = list->prev;
        struct list_head *at = head->next;

        first->prev = head;
        head->next = first;

        last->next = at;
        at->prev = last;
}






static inline __attribute__((always_inline)) void list_splice(struct list_head *list, struct list_head *head)
{
        if (!list_empty(list))
                __list_splice(list, head);
}
# 305 "include/linux/list.h"
static inline __attribute__((always_inline)) void list_splice_init(struct list_head *list,
                                    struct list_head *head)
{
        if (!list_empty(list)) {
                __list_splice(list, head);
                do { (list)->next = (list); (list)->prev = (list); } while (0);
        }
}
# 509 "include/linux/list.h"
struct hlist_head {
        struct hlist_node *first;
};

struct hlist_node {
        struct hlist_node *next, **pprev;
};






static inline __attribute__((always_inline)) int hlist_unhashed(const struct hlist_node *h)
{
        return !h->pprev;
}

static inline __attribute__((always_inline)) int hlist_empty(const struct hlist_head *h)
{
        return !h->first;
}

static inline __attribute__((always_inline)) void __hlist_del(struct hlist_node *n)
{
        struct hlist_node *next = n->next;
        struct hlist_node **pprev = n->pprev;
        *pprev = next;
        if (next)
                next->pprev = pprev;
}

static inline __attribute__((always_inline)) void hlist_del(struct hlist_node *n)
{
        __hlist_del(n);
        n->next = ((void *) 0x00100100);
        n->pprev = ((void *) 0x00200200);
}
# 567 "include/linux/list.h"
static inline __attribute__((always_inline)) void hlist_del_rcu(struct hlist_node *n)
{
        __hlist_del(n);
        n->pprev = ((void *) 0x00200200);
}

static inline __attribute__((always_inline)) void hlist_del_init(struct hlist_node *n)
{
        if (n->pprev) {
                __hlist_del(n);
                ((n)->next = ((void *)0), (n)->pprev = ((void *)0));
        }
}

static inline __attribute__((always_inline)) void hlist_add_head(struct hlist_node *n, struct hlist_head *h)
{
        struct hlist_node *first = h->first;
        n->next = first;
        if (first)
                first->pprev = &n->next;
        h->first = n;
        n->pprev = &h->first;
}
# 608 "include/linux/list.h"
static inline __attribute__((always_inline)) void hlist_add_head_rcu(struct hlist_node *n,
                                        struct hlist_head *h)
{
        struct hlist_node *first = h->first;
        n->next = first;
        n->pprev = &h->first;
        __asm__ __volatile__ ("": : :"memory");
        if (first)
                first->pprev = &n->next;
        h->first = n;
}


static inline __attribute__((always_inline)) void hlist_add_before(struct hlist_node *n,
                                        struct hlist_node *next)
{
        n->pprev = next->pprev;
        n->next = next;
        next->pprev = &n->next;
        *(n->pprev) = n;
}

static inline __attribute__((always_inline)) void hlist_add_after(struct hlist_node *n,
                                        struct hlist_node *next)
{
        next->next = n->next;
        n->next = next;
        next->pprev = &n->next;

        if(next->next)
                next->next->pprev = &next->next;
}
# 656 "include/linux/list.h"
static inline __attribute__((always_inline)) void hlist_add_before_rcu(struct hlist_node *n,
                                        struct hlist_node *next)
{
        n->pprev = next->pprev;
        n->next = next;
        __asm__ __volatile__ ("": : :"memory");
        next->pprev = &n->next;
        *(n->pprev) = n;
}
# 681 "include/linux/list.h"
static inline __attribute__((always_inline)) void hlist_add_after_rcu(struct hlist_node *prev,
                                       struct hlist_node *n)
{
        n->next = prev->next;
        n->pprev = &prev->next;
        __asm__ __volatile__ ("": : :"memory");
        prev->next = n;
        if (n->next)
                n->next->pprev = &n->next;
}
# 24 "include/linux/wait.h" 2



# 1 "include/asm/current.h" 1





struct task_struct;

static inline __attribute__((always_inline)) struct task_struct * get_current(void)
{
        return current_thread_info()->task;
}
# 28 "include/linux/wait.h" 2

typedef struct __wait_queue wait_queue_t;
typedef int (*wait_queue_func_t)(wait_queue_t *wait, unsigned mode, int sync, void *key);
int default_wake_function(wait_queue_t *wait, unsigned mode, int sync, void *key);

struct __wait_queue {
        unsigned int flags;

        void *private;
        wait_queue_func_t func;
        struct list_head task_list;
};

struct wait_bit_key {
        void *flags;
        int bit_nr;
};

struct wait_bit_queue {
        struct wait_bit_key key;
        wait_queue_t wait;
};

struct __wait_queue_head {
        spinlock_t lock;
        struct list_head task_list;
};
typedef struct __wait_queue_head wait_queue_head_t;
# 80 "include/linux/wait.h"
static inline __attribute__((always_inline)) void init_waitqueue_head(wait_queue_head_t *q)
{
        spin_lock_init(&q->lock);
        do { (&q->task_list)->next = (&q->task_list); (&q->task_list)->prev = (&q->task_list); } while (0);
}

static inline __attribute__((always_inline)) void init_waitqueue_entry(wait_queue_t *q, struct task_struct *p)
{
        q->flags = 0;
        q->private = p;
        q->func = default_wake_function;
}

static inline __attribute__((always_inline)) void init_waitqueue_func_entry(wait_queue_t *q,
                                        wait_queue_func_t func)
{
        q->flags = 0;
        q->private = ((void *)0);
        q->func = func;
}

static inline __attribute__((always_inline)) int waitqueue_active(wait_queue_head_t *q)
{
        return !list_empty(&q->task_list);
}
# 115 "include/linux/wait.h"
extern void add_wait_queue(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));
extern void add_wait_queue_exclusive(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));
extern void remove_wait_queue(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));

static inline __attribute__((always_inline)) void __add_wait_queue(wait_queue_head_t *head, wait_queue_t *new)
{
        list_add(&new->task_list, &head->task_list);
}




static inline __attribute__((always_inline)) void __add_wait_queue_tail(wait_queue_head_t *head,
                                                wait_queue_t *new)
{
        list_add_tail(&new->task_list, &head->task_list);
}

static inline __attribute__((always_inline)) void __remove_wait_queue(wait_queue_head_t *head,
                                                        wait_queue_t *old)
{
        list_del(&old->task_list);
}

void __wake_up(wait_queue_head_t *q, unsigned int mode, int nr, void *key) __attribute__((regparm(3)));
extern void __wake_up_locked(wait_queue_head_t *q, unsigned int mode) __attribute__((regparm(3)));
extern void __wake_up_sync(wait_queue_head_t *q, unsigned int mode, int nr) __attribute__((regparm(3)));
void __wake_up_bit(wait_queue_head_t *, void *, int) __attribute__((regparm(3)));
int __wait_on_bit(wait_queue_head_t *, struct wait_bit_queue *, int (*)(void *), unsigned) __attribute__((regparm(3)));
int __wait_on_bit_lock(wait_queue_head_t *, struct wait_bit_queue *, int (*)(void *), unsigned) __attribute__((regparm(3)));
void wake_up_bit(void *, int) __attribute__((regparm(3)));
int out_of_line_wait_on_bit(void *, int, int (*)(void *), unsigned) __attribute__((regparm(3)));
int out_of_line_wait_on_bit_lock(void *, int, int (*)(void *), unsigned) __attribute__((regparm(3)));
wait_queue_head_t *bit_waitqueue(void *, int) __attribute__((regparm(3)));
# 346 "include/linux/wait.h"
static inline __attribute__((always_inline)) void add_wait_queue_exclusive_locked(wait_queue_head_t *q,
                                                   wait_queue_t * wait)
{
        wait->flags |= 0x01;
        __add_wait_queue_tail(q, wait);
}




static inline __attribute__((always_inline)) void remove_wait_queue_locked(wait_queue_head_t *q,
                                            wait_queue_t * wait)
{
        __remove_wait_queue(q, wait);
}






extern void sleep_on(wait_queue_head_t *q) __attribute__((regparm(3)));
extern long sleep_on_timeout(wait_queue_head_t *q, signed long timeout) __attribute__((regparm(3)));

extern void interruptible_sleep_on(wait_queue_head_t *q) __attribute__((regparm(3)));
extern long interruptible_sleep_on_timeout(wait_queue_head_t *q, signed long timeout) __attribute__((regparm(3)));





void prepare_to_wait(wait_queue_head_t *q, wait_queue_t *wait, int state) __attribute__((regparm(3)));

void prepare_to_wait_exclusive(wait_queue_head_t *q, wait_queue_t *wait, int state) __attribute__((regparm(3)));

void finish_wait(wait_queue_head_t *q, wait_queue_t *wait) __attribute__((regparm(3)));
int autoremove_wake_function(wait_queue_t *wait, unsigned mode, int sync, void *key);
int wake_bit_function(wait_queue_t *wait, unsigned mode, int sync, void *key);
# 424 "include/linux/wait.h"
static inline __attribute__((always_inline)) int wait_on_bit(void *word, int bit,
                                int (*action)(void *), unsigned mode)
{
        if (!(__builtin_constant_p(bit) ? constant_test_bit((bit),(word)) : variable_test_bit((bit),(word))))
                return 0;
        return out_of_line_wait_on_bit(word, bit, action, mode);
}
# 448 "include/linux/wait.h"
static inline __attribute__((always_inline)) int wait_on_bit_lock(void *word, int bit,
                                int (*action)(void *), unsigned mode)
{
        if (!test_and_set_bit(bit, word))
                return 0;
        return out_of_line_wait_on_bit_lock(word, bit, action, mode);
}
# 42 "include/asm/semaphore.h" 2
# 1 "include/linux/rwsem.h" 1
# 22 "include/linux/rwsem.h"
struct rw_semaphore;


# 1 "include/linux/rwsem-spinlock.h" 1
# 22 "include/linux/rwsem-spinlock.h"
struct rwsem_waiter;
# 31 "include/linux/rwsem-spinlock.h"
struct rw_semaphore {
        __s32 activity;
        spinlock_t wait_lock;
        struct list_head wait_list;



};
# 55 "include/linux/rwsem-spinlock.h"
extern void init_rwsem(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __down_read(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern int __down_read_trylock(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __down_write(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern int __down_write_trylock(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __up_read(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __up_write(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __downgrade_write(struct rw_semaphore *sem) __attribute__((regparm(3)));
# 26 "include/linux/rwsem.h" 2
# 41 "include/linux/rwsem.h"
static inline __attribute__((always_inline)) void down_read(struct rw_semaphore *sem)
{
        do { do { } while (0); } while (0);
        ;
        __down_read(sem);
        ;
}




static inline __attribute__((always_inline)) int down_read_trylock(struct rw_semaphore *sem)
{
        int ret;
        ;
        ret = __down_read_trylock(sem);
        ;
        return ret;
}




static inline __attribute__((always_inline)) void down_write(struct rw_semaphore *sem)
{
        do { do { } while (0); } while (0);
        ;
        __down_write(sem);
        ;
}




static inline __attribute__((always_inline)) int down_write_trylock(struct rw_semaphore *sem)
{
        int ret;
        ;
        ret = __down_write_trylock(sem);
        ;
        return ret;
}




static inline __attribute__((always_inline)) void up_read(struct rw_semaphore *sem)
{
        ;
        __up_read(sem);
        ;
}




static inline __attribute__((always_inline)) void up_write(struct rw_semaphore *sem)
{
        ;
        __up_write(sem);
        ;
}




static inline __attribute__((always_inline)) void downgrade_write(struct rw_semaphore *sem)
{
        ;
        __downgrade_write(sem);
        ;
}
# 43 "include/asm/semaphore.h" 2

struct semaphore {
        atomic_t count;
        int sleepers;
        wait_queue_head_t wait;
};
# 67 "include/asm/semaphore.h"
static inline __attribute__((always_inline)) void sema_init (struct semaphore *sem, int val)
{






        (((&sem->count)->counter) = (val));
        sem->sleepers = 0;
        init_waitqueue_head(&sem->wait);
}

static inline __attribute__((always_inline)) void init_MUTEX (struct semaphore *sem)
{
        sema_init(sem, 1);
}

static inline __attribute__((always_inline)) void init_MUTEX_LOCKED (struct semaphore *sem)
{
        sema_init(sem, 0);
}

# 21 "include/linux/sched.h" 2

# 1 "include/asm/ptrace.h" 1
# 26 "include/asm/ptrace.h"
struct pt_regs {
        long ebx;
        long ecx;
        long edx;
        long esi;
        long edi;
        long ebp;
        long eax;
        int xds;
        int xes;
        long orig_eax;
        long eip;
        int xcs;
        long eflags;
        long esp;
        int xss;
};
# 61 "include/asm/ptrace.h"
struct task_struct;
extern void send_sigtrap(struct task_struct *tsk, struct pt_regs *regs, int error_code);
# 71 "include/asm/ptrace.h"
static inline __attribute__((always_inline)) int user_mode(struct pt_regs *regs)
{
        return (regs->xcs & 3) != 0;
}
static inline __attribute__((always_inline)) int user_mode_vm(struct pt_regs *regs)
{
        return ((regs->xcs & 3) | (regs->eflags & 0x00020000)) != 0;
}
# 23 "include/linux/sched.h" 2
# 1 "include/asm/mmu.h" 1
# 11 "include/asm/mmu.h"
typedef struct {
        int size;
        struct semaphore sem;
        void *ldt;
} mm_context_t;
# 24 "include/linux/sched.h" 2
# 1 "include/asm/cputime.h" 1



# 1 "include/asm-generic/cputime.h" 1






typedef unsigned long cputime_t;
# 23 "include/asm-generic/cputime.h"
typedef u64 cputime64_t;
# 5 "include/asm/cputime.h" 2
# 25 "include/linux/sched.h" 2

# 1 "include/linux/smp.h" 1
# 11 "include/linux/smp.h"
extern void cpu_idle(void);







# 1 "include/asm/smp.h" 1
# 16 "include/asm/smp.h"
# 1 "include/asm/fixmap.h" 1
# 27 "include/asm/fixmap.h"
# 1 "include/asm/acpi.h" 1
# 31 "include/asm/acpi.h"
# 1 "include/acpi/pdc_intel.h" 1
# 32 "include/asm/acpi.h" 2
# 60 "include/asm/acpi.h"
static inline __attribute__((always_inline)) int
__acpi_acquire_global_lock (unsigned int *lock)
{
        unsigned int old, new, val;
        do {
                old = *lock;
                new = (((old & ~0x3) + 2) + ((old >> 1) & 0x1));
                val = ((__typeof__(*(lock)))__cmpxchg((lock),(unsigned long)(old), (unsigned long)(new),sizeof(*(lock))));
        } while (__builtin_expect(!!(val != old), 0));
        return (new < 3) ? -1 : 0;
}

static inline __attribute__((always_inline)) int
__acpi_release_global_lock (unsigned int *lock)
{
        unsigned int old, new, val;
        do {
                old = *lock;
                new = old & ~0x3;
                val = ((__typeof__(*(lock)))__cmpxchg((lock),(unsigned long)(old), (unsigned long)(new),sizeof(*(lock))));
        } while (__builtin_expect(!!(val != old), 0));
        return old & 0x1;
}
# 161 "include/asm/acpi.h"
static inline __attribute__((always_inline)) void acpi_noirq_set(void) { }
static inline __attribute__((always_inline)) void acpi_disable_pci(void) { }
# 180 "include/asm/acpi.h"
extern u8 x86_acpiid_to_apicid[];
# 28 "include/asm/fixmap.h" 2
# 1 "include/asm/apicdef.h" 1
# 125 "include/asm/apicdef.h"
struct local_apic {

        struct { unsigned int __reserved[4]; } __reserved_01;

        struct { unsigned int __reserved[4]; } __reserved_02;

        struct {
                unsigned int __reserved_1 : 24,
                        phys_apic_id : 4,
                        __reserved_2 : 4;
                unsigned int __reserved[3];
        } id;

        const
        struct {
                unsigned int version : 8,
                        __reserved_1 : 8,
                        max_lvt : 8,
                        __reserved_2 : 8;
                unsigned int __reserved[3];
        } version;

        struct { unsigned int __reserved[4]; } __reserved_03;

        struct { unsigned int __reserved[4]; } __reserved_04;

        struct { unsigned int __reserved[4]; } __reserved_05;

        struct { unsigned int __reserved[4]; } __reserved_06;

        struct {
                unsigned int priority : 8,
                        __reserved_1 : 24;
                unsigned int __reserved_2[3];
        } tpr;

        const
        struct {
                unsigned int priority : 8,
                        __reserved_1 : 24;
                unsigned int __reserved_2[3];
        } apr;

        const
        struct {
                unsigned int priority : 8,
                        __reserved_1 : 24;
                unsigned int __reserved_2[3];
        } ppr;

        struct {
                unsigned int eoi;
                unsigned int __reserved[3];
        } eoi;

        struct { unsigned int __reserved[4]; } __reserved_07;

        struct {
                unsigned int __reserved_1 : 24,
                        logical_dest : 8;
                unsigned int __reserved_2[3];
        } ldr;

        struct {
                unsigned int __reserved_1 : 28,
                        model : 4;
                unsigned int __reserved_2[3];
        } dfr;

        struct {
                unsigned int spurious_vector : 8,
                        apic_enabled : 1,
                        focus_cpu : 1,
                        __reserved_2 : 22;
                unsigned int __reserved_3[3];
        } svr;

        struct {
                unsigned int bitfield;
                unsigned int __reserved[3];
        } isr [8];

        struct {
                unsigned int bitfield;
                unsigned int __reserved[3];
        } tmr [8];

        struct {
                unsigned int bitfield;
                unsigned int __reserved[3];
        } irr [8];

        union {
                struct {
                        unsigned int send_cs_error : 1,
                                receive_cs_error : 1,
                                send_accept_error : 1,
                                receive_accept_error : 1,
                                __reserved_1 : 1,
                                send_illegal_vector : 1,
                                receive_illegal_vector : 1,
                                illegal_register_address : 1,
                                __reserved_2 : 24;
                        unsigned int __reserved_3[3];
                } error_bits;
                struct {
                        unsigned int errors;
                        unsigned int __reserved_3[3];
                } all_errors;
        } esr;

        struct { unsigned int __reserved[4]; } __reserved_08;

        struct { unsigned int __reserved[4]; } __reserved_09;

        struct { unsigned int __reserved[4]; } __reserved_10;

        struct { unsigned int __reserved[4]; } __reserved_11;

        struct { unsigned int __reserved[4]; } __reserved_12;

        struct { unsigned int __reserved[4]; } __reserved_13;

        struct { unsigned int __reserved[4]; } __reserved_14;

        struct {
                unsigned int vector : 8,
                        delivery_mode : 3,
                        destination_mode : 1,
                        delivery_status : 1,
                        __reserved_1 : 1,
                        level : 1,
                        trigger : 1,
                        __reserved_2 : 2,
                        shorthand : 2,
                        __reserved_3 : 12;
                unsigned int __reserved_4[3];
        } icr1;

        struct {
                union {
                        unsigned int __reserved_1 : 24,
                                phys_dest : 4,
                                __reserved_2 : 4;
                        unsigned int __reserved_3 : 24,
                                logical_dest : 8;
                } dest;
                unsigned int __reserved_4[3];
        } icr2;

        struct {
                unsigned int vector : 8,
                        __reserved_1 : 4,
                        delivery_status : 1,
                        __reserved_2 : 3,
                        mask : 1,
                        timer_mode : 1,
                        __reserved_3 : 14;
                unsigned int __reserved_4[3];
        } lvt_timer;

        struct {
                unsigned int vector : 8,
                        delivery_mode : 3,
                        __reserved_1 : 1,
                        delivery_status : 1,
                        __reserved_2 : 3,
                        mask : 1,
                        __reserved_3 : 15;
                unsigned int __reserved_4[3];
        } lvt_thermal;

        struct {
                unsigned int vector : 8,
                        delivery_mode : 3,
                        __reserved_1 : 1,
                        delivery_status : 1,
                        __reserved_2 : 3,
                        mask : 1,
                        __reserved_3 : 15;
                unsigned int __reserved_4[3];
        } lvt_pc;

        struct {
                unsigned int vector : 8,
                        delivery_mode : 3,
                        __reserved_1 : 1,
                        delivery_status : 1,
                        polarity : 1,
                        remote_irr : 1,
                        trigger : 1,
                        mask : 1,
                        __reserved_2 : 15;
                unsigned int __reserved_3[3];
        } lvt_lint0;

        struct {
                unsigned int vector : 8,
                        delivery_mode : 3,
                        __reserved_1 : 1,
                        delivery_status : 1,
                        polarity : 1,
                        remote_irr : 1,
                        trigger : 1,
                        mask : 1,
                        __reserved_2 : 15;
                unsigned int __reserved_3[3];
        } lvt_lint1;

        struct {
                unsigned int vector : 8,
                        __reserved_1 : 4,
                        delivery_status : 1,
                        __reserved_2 : 3,
                        mask : 1,
                        __reserved_3 : 15;
                unsigned int __reserved_4[3];
        } lvt_error;

        struct {
                unsigned int initial_count;
                unsigned int __reserved_2[3];
        } timer_icr;

        const
        struct {
                unsigned int curr_count;
                unsigned int __reserved_2[3];
        } timer_ccr;

        struct { unsigned int __reserved[4]; } __reserved_16;

        struct { unsigned int __reserved[4]; } __reserved_17;

        struct { unsigned int __reserved[4]; } __reserved_18;

        struct { unsigned int __reserved[4]; } __reserved_19;

        struct {
                unsigned int divisor : 4,
                        __reserved_1 : 28;
                unsigned int __reserved_2[3];
        } timer_dcr;

        struct { unsigned int __reserved[4]; } __reserved_20;

} __attribute__ ((packed));
# 29 "include/asm/fixmap.h" 2



# 1 "include/asm/kmap_types.h" 1
# 12 "include/asm/kmap_types.h"
enum km_type {
__KM_FENCE_0 , KM_BOUNCE_READ,
__KM_FENCE_1 , KM_SKB_SUNRPC_DATA,
__KM_FENCE_2 , KM_SKB_DATA_SOFTIRQ,
__KM_FENCE_3 , KM_USER0,
__KM_FENCE_4 , KM_USER1,
__KM_FENCE_5 , KM_BIO_SRC_IRQ,
__KM_FENCE_6 , KM_BIO_DST_IRQ,
__KM_FENCE_7 , KM_PTE0,
__KM_FENCE_8 , KM_PTE1,
__KM_FENCE_9 , KM_IRQ0,
__KM_FENCE_10 , KM_IRQ1,
__KM_FENCE_11 , KM_SOFTIRQ0,
__KM_FENCE_12 , KM_SOFTIRQ1,
__KM_FENCE_13 , KM_TYPE_NR
};
# 33 "include/asm/fixmap.h" 2
# 53 "include/asm/fixmap.h"
enum fixed_addresses {
        FIX_HOLE,
        FIX_VSYSCALL,

        FIX_APIC_BASE,


        FIX_IO_APIC_BASE_0,
        FIX_IO_APIC_BASE_END = FIX_IO_APIC_BASE_0 + 64 -1,
# 70 "include/asm/fixmap.h"
        FIX_F00F_IDT,





        FIX_KMAP_BEGIN,
        FIX_KMAP_END = FIX_KMAP_BEGIN+(KM_TYPE_NR*8)-1,
# 86 "include/asm/fixmap.h"
        __end_of_permanent_fixed_addresses,


        FIX_BTMAP_END = __end_of_permanent_fixed_addresses,
        FIX_BTMAP_BEGIN = FIX_BTMAP_END + 16 - 1,
        FIX_WP_TEST,
        __end_of_fixed_addresses
};

extern void __set_fixmap (enum fixed_addresses idx,
                                        unsigned long phys, pgprot_t flags);
# 127 "include/asm/fixmap.h"
extern void __this_fixmap_does_not_exist(void);






static inline __attribute__((always_inline)) unsigned long fix_to_virt(const unsigned int idx)
{
# 145 "include/asm/fixmap.h"
        if (idx >= __end_of_fixed_addresses)
                __this_fixmap_does_not_exist();

        return (((unsigned long)0xfffff000) - ((idx) << 12));
}

static inline __attribute__((always_inline)) unsigned long virt_to_fix(const unsigned long vaddr)
{
        do { if (__builtin_expect(!!((vaddr >= ((unsigned long)0xfffff000) || vaddr < (((unsigned long)0xfffff000) - (__end_of_permanent_fixed_addresses << 12)))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (153), "i" ("include/asm/fixmap.h")); } while(0);
        return ((((unsigned long)0xfffff000) - ((vaddr)&(~((1UL << 12)-1)))) >> 12);
}
# 17 "include/asm/smp.h" 2

# 1 "include/asm/mpspec.h" 1




# 1 "include/asm/mpspec_def.h" 1
# 19 "include/asm/mpspec_def.h"
struct intel_mp_floating
{
        char mpf_signature[4];
        unsigned long mpf_physptr;
        unsigned char mpf_length;
        unsigned char mpf_specification;
        unsigned char mpf_checksum;
        unsigned char mpf_feature1;
        unsigned char mpf_feature2;
        unsigned char mpf_feature3;
        unsigned char mpf_feature4;
        unsigned char mpf_feature5;
};

struct mp_config_table
{
        char mpc_signature[4];

        unsigned short mpc_length;
        char mpc_spec;
        char mpc_checksum;
        char mpc_oem[8];
        char mpc_productid[12];
        unsigned long mpc_oemptr;
        unsigned short mpc_oemsize;
        unsigned short mpc_oemcount;
        unsigned long mpc_lapic;
        unsigned long reserved;
};
# 58 "include/asm/mpspec_def.h"
struct mpc_config_processor
{
        unsigned char mpc_type;
        unsigned char mpc_apicid;
        unsigned char mpc_apicver;
        unsigned char mpc_cpuflag;


        unsigned long mpc_cpufeature;



        unsigned long mpc_featureflag;
        unsigned long mpc_reserved[2];
};

struct mpc_config_bus
{
        unsigned char mpc_type;
        unsigned char mpc_busid;
        unsigned char mpc_bustype[6] __attribute((packed));
};
# 102 "include/asm/mpspec_def.h"
struct mpc_config_ioapic
{
        unsigned char mpc_type;
        unsigned char mpc_apicid;
        unsigned char mpc_apicver;
        unsigned char mpc_flags;

        unsigned long mpc_apicaddr;
};

struct mpc_config_intsrc
{
        unsigned char mpc_type;
        unsigned char mpc_irqtype;
        unsigned short mpc_irqflag;
        unsigned char mpc_srcbus;
        unsigned char mpc_srcbusirq;
        unsigned char mpc_dstapic;
        unsigned char mpc_dstirq;
};

enum mp_irq_source_types {
        mp_INT = 0,
        mp_NMI = 1,
        mp_SMI = 2,
        mp_ExtINT = 3
};






struct mpc_config_lintsrc
{
        unsigned char mpc_type;
        unsigned char mpc_irqtype;
        unsigned short mpc_irqflag;
        unsigned char mpc_srcbusid;
        unsigned char mpc_srcbusirq;
        unsigned char mpc_destapic;

        unsigned char mpc_destapiclint;
};

struct mp_config_oemtable
{
        char oem_signature[4];

        unsigned short oem_length;
        char oem_rev;
        char oem_checksum;
        char mpc_oem[8];
};

struct mpc_config_translation
{
        unsigned char mpc_type;
        unsigned char trans_len;
        unsigned char trans_type;
        unsigned char trans_quad;
        unsigned char trans_global;
        unsigned char trans_local;
        unsigned short trans_reserved;
};
# 180 "include/asm/mpspec_def.h"
enum mp_bustype {
        MP_BUS_ISA = 1,
        MP_BUS_EISA,
        MP_BUS_PCI,
        MP_BUS_MCA,
        MP_BUS_NEC98
};
# 6 "include/asm/mpspec.h" 2
# 1 "include/asm/mach-summit/mach_mpspec.h" 1
# 7 "include/asm/mpspec.h" 2

extern int mp_bus_id_to_type [260];
extern int mp_bus_id_to_node [260];
extern int mp_bus_id_to_local [260];
extern int quad_local_to_mp_bus_id [8/4][4];
extern int mp_bus_id_to_pci_bus [260];

extern unsigned int def_to_bigsmp;
extern unsigned int boot_cpu_physical_apicid;
extern int smp_found_config;
extern void find_smp_config (void);
extern void get_smp_config (void);
extern int nr_ioapics;
extern int apic_version [256];
extern int mp_bus_id_to_type [260];
extern int mp_irq_entries;
extern struct mpc_config_intsrc mp_irqs [256];
extern int mpc_default_type;
extern int mp_bus_id_to_pci_bus [260];
extern unsigned long mp_lapic_addr;
extern int pic_mode;
extern int using_apic_timer;
# 41 "include/asm/mpspec.h"
struct physid_mask
{
        unsigned long mask[(((256)+32 -1)/32)];
};

typedef struct physid_mask physid_mask_t;
# 81 "include/asm/mpspec.h"
extern physid_mask_t phys_cpu_present_map;
# 19 "include/asm/smp.h" 2

# 1 "include/asm/io_apic.h" 1
# 36 "include/asm/io_apic.h"
static inline __attribute__((always_inline)) int use_pci_vector(void) {return 0;}
static inline __attribute__((always_inline)) void disable_edge_ioapic_irq(unsigned int irq) { }
static inline __attribute__((always_inline)) void mask_and_ack_level_ioapic_irq(unsigned int irq) { }
static inline __attribute__((always_inline)) void end_edge_ioapic_irq (unsigned int irq) { }
# 63 "include/asm/io_apic.h"
union IO_APIC_reg_00 {
        u32 raw;
        struct {
                u32 __reserved_2 : 14,
                        LTS : 1,
                        delivery_type : 1,
                        __reserved_1 : 8,
                        ID : 8;
        } __attribute__ ((packed)) bits;
};

union IO_APIC_reg_01 {
        u32 raw;
        struct {
                u32 version : 8,
                        __reserved_2 : 7,
                        PRQ : 1,
                        entries : 8,
                        __reserved_1 : 8;
        } __attribute__ ((packed)) bits;
};

union IO_APIC_reg_02 {
        u32 raw;
        struct {
                u32 __reserved_2 : 24,
                        arbitration : 4,
                        __reserved_1 : 4;
        } __attribute__ ((packed)) bits;
};

union IO_APIC_reg_03 {
        u32 raw;
        struct {
                u32 boot_DT : 1,
                        __reserved_1 : 31;
        } __attribute__ ((packed)) bits;
};




extern int nr_ioapics;
extern int nr_ioapic_registers[64];

enum ioapic_irq_destination_types {
        dest_Fixed = 0,
        dest_LowestPrio = 1,
        dest_SMI = 2,
        dest__reserved_1 = 3,
        dest_NMI = 4,
        dest_INIT = 5,
        dest__reserved_2 = 6,
        dest_ExtINT = 7
};

struct IO_APIC_route_entry {
        __u32 vector : 8,
                delivery_mode : 3,



                dest_mode : 1,
                delivery_status : 1,
                polarity : 1,
                irr : 1,
                trigger : 1,
                mask : 1,
                __reserved_2 : 15;

        union { struct { __u32
                                        __reserved_1 : 24,
                                        physical_dest : 4,
                                        __reserved_2 : 4;
                        } physical;

                        struct { __u32
                                        __reserved_1 : 24,
                                        logical_dest : 8;
                        } logical;
        } dest;

} __attribute__ ((packed));






extern struct mpc_config_ioapic mp_ioapics[64];


extern int mp_irq_entries;


extern struct mpc_config_intsrc mp_irqs[256];


extern int mpc_default_type;

static inline __attribute__((always_inline)) unsigned int io_apic_read(unsigned int apic, unsigned int reg)
{
        *((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        return *(((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4);
}

static inline __attribute__((always_inline)) void io_apic_write(unsigned int apic, unsigned int reg, unsigned int value)
{
        *((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        *(((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4) = value;
}







extern int sis_apic_bug;
static inline __attribute__((always_inline)) void io_apic_modify(unsigned int apic, unsigned int reg, unsigned int value)
{
        if (sis_apic_bug)
                *((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        *(((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4) = value;
}


extern int skip_ioapic_setup;
# 205 "include/asm/io_apic.h"
extern int (*ioapic_renumber_irq)(int ioapic, int irq);





extern int assign_irq_vector(int irq);
# 21 "include/asm/smp.h" 2

# 1 "include/asm/apic.h" 1




# 1 "include/linux/pm.h" 1
# 35 "include/linux/pm.h"
typedef int pm_request_t;
# 44 "include/linux/pm.h"
typedef int pm_dev_t;
# 57 "include/linux/pm.h"
enum
{
        PM_SYS_UNKNOWN = 0x00000000,
        PM_SYS_KBC = 0x41d00303,
        PM_SYS_COM = 0x41d00500,
        PM_SYS_IRDA = 0x41d00510,
        PM_SYS_FDC = 0x41d00700,
        PM_SYS_VGA = 0x41d00900,
        PM_SYS_PCMCIA = 0x41d00e00,
};
# 76 "include/linux/pm.h"
struct pm_dev;

typedef int (*pm_callback)(struct pm_dev *dev, pm_request_t rqst, void *data);




struct pm_dev
{
        pm_dev_t type;
        unsigned long id;
        pm_callback callback;
        void *data;

        unsigned long flags;
        unsigned long state;
        unsigned long prev_state;

        struct list_head entry;
};



extern int pm_active;






struct pm_dev __attribute__((deprecated)) *
pm_register(pm_dev_t type, unsigned long id, pm_callback callback);




void __attribute__((deprecated)) pm_unregister(struct pm_dev *dev);




void __attribute__((deprecated)) pm_unregister_all(pm_callback callback);




int __attribute__((deprecated)) pm_send_all(pm_request_t rqst, void *data);
# 152 "include/linux/pm.h"
extern void (*pm_idle)(void);
extern void (*pm_power_off)(void);

typedef int suspend_state_t;







typedef int suspend_disk_method_t;







struct pm_ops {
        suspend_disk_method_t pm_disk_mode;
        int (*prepare)(suspend_state_t state);
        int (*enter)(suspend_state_t state);
        int (*finish)(suspend_state_t state);
};

extern void pm_set_ops(struct pm_ops *);
extern struct pm_ops *pm_ops;
extern int pm_suspend(suspend_state_t state);






struct device;

typedef struct pm_message {
        int event;
} pm_message_t;
# 220 "include/linux/pm.h"
struct dev_pm_info {
        pm_message_t power_state;

        pm_message_t prev_state;
        void * saved_state;
        atomic_t pm_users;
        struct device * pm_parent;
        struct list_head entry;

};

extern void device_pm_set_parent(struct device * dev, struct device * parent);

extern int device_power_down(pm_message_t state);
extern void device_power_up(void);
extern void device_resume(void);


extern int device_suspend(pm_message_t state);
# 6 "include/asm/apic.h" 2
# 20 "include/asm/apic.h"
extern int enable_local_apic;
extern int apic_verbosity;

static inline __attribute__((always_inline)) void lapic_disable(void)
{
        enable_local_apic = -1;
        clear_bit((0*32+ 9), boot_cpu_data.x86_capability);
}

static inline __attribute__((always_inline)) void lapic_enable(void)
{
        enable_local_apic = 1;
}
# 52 "include/asm/apic.h"
static __inline __attribute__((always_inline)) void apic_write(unsigned long reg, unsigned long v)
{
        *((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)) = v;
}

static __inline __attribute__((always_inline)) void apic_write_atomic(unsigned long reg, unsigned long v)
{
        ((__typeof__(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg))))__xchg((unsigned long)(v),((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)),sizeof(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)))));
}

static __inline __attribute__((always_inline)) unsigned long apic_read(unsigned long reg)
{
        return *((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg));
}

static __inline__ __attribute__((always_inline)) void apic_wait_icr_idle(void)
{
        while ( apic_read( 0x300 ) & 0x01000 )
                rep_nop();
}

int get_physical_broadcast(void);
# 85 "include/asm/apic.h"
static inline __attribute__((always_inline)) void ack_APIC_irq(void)
{
# 95 "include/asm/apic.h"
        apic_write_atomic((0xB0),(0));
}

extern void (*wait_timer_tick)(void);

extern int get_maxlvt(void);
extern void clear_local_APIC(void);
extern void connect_bsp_APIC (void);
extern void disconnect_bsp_APIC (int virt_wire_setup);
extern void disable_local_APIC (void);
extern void lapic_shutdown (void);
extern int verify_local_APIC (void);
extern void cache_APIC_registers (void);
extern void sync_Arb_IDs (void);
extern void init_bsp_APIC (void);
extern void setup_local_APIC (void);
extern void init_apic_mappings (void);
extern void smp_local_timer_interrupt (struct pt_regs * regs);
extern void setup_boot_APIC_clock (void);
extern void setup_secondary_APIC_clock (void);
extern void setup_apic_nmi_watchdog (void);
extern int reserve_lapic_nmi(void);
extern void release_lapic_nmi(void);
extern void disable_timer_nmi_watchdog(void);
extern void enable_timer_nmi_watchdog(void);
extern void nmi_watchdog_tick (struct pt_regs * regs);
extern int APIC_init_uniprocessor (void);
extern void disable_APIC_timer(void);
extern void enable_APIC_timer(void);

extern void enable_NMI_through_LVT0 (void * dummy);

extern unsigned int nmi_watchdog;





extern int disable_timer_pin_1;
# 23 "include/asm/smp.h" 2
# 34 "include/asm/smp.h"
extern void smp_alloc_memory(void);
extern int pic_mode;
extern int smp_num_siblings;
extern cpumask_t cpu_sibling_map[];
extern cpumask_t cpu_core_map[];

extern void (*mtrr_hook) (void);
extern void zap_low_mappings (void);
extern void lock_ipi_call_lock(void);
extern void unlock_ipi_call_lock(void);


extern u8 x86_cpu_to_apicid[];
# 60 "include/asm/smp.h"
extern cpumask_t cpu_callout_map;
extern cpumask_t cpu_callin_map;
extern cpumask_t cpu_possible_map;


static inline __attribute__((always_inline)) int num_booting_cpus(void)
{
        return __cpus_weight(&(cpu_callout_map), 8);
}






# 1 "include/asm/mach-summit/mach_apicdef.h" 1





static inline __attribute__((always_inline)) unsigned get_apic_id(unsigned long x)
{
        return (((x)>>24)&0xFF);
}
# 76 "include/asm/smp.h" 2
static inline __attribute__((always_inline)) int hard_smp_processor_id(void)
{

        return get_apic_id(*(unsigned long *)((fix_to_virt(FIX_APIC_BASE))+0x20));
}


static __inline __attribute__((always_inline)) int logical_smp_processor_id(void)
{

        return (((*(unsigned long *)((fix_to_virt(FIX_APIC_BASE))+0xD0))>>24)&0xFF);
}



extern int __cpu_disable(void);
extern void __cpu_die(unsigned int cpu);
# 20 "include/linux/smp.h" 2
# 30 "include/linux/smp.h"
extern void smp_send_stop(void);




extern void smp_send_reschedule(int cpu);





extern void smp_prepare_cpus(unsigned int max_cpus);




extern int __cpu_up(unsigned int cpunum);




extern void smp_cpus_done(unsigned int max_cpus);




extern int smp_call_function (void (*func) (void *info), void *info,
                              int retry, int wait);




static inline __attribute__((always_inline)) int on_each_cpu(void (*func) (void *info), void *info,
                              int retry, int wait)
{
        int ret = 0;

        do { } while (0);
        ret = smp_call_function(func, info, retry, wait);
        func(info);
        do { } while (0);
        return ret;
}
# 88 "include/linux/smp.h"
void smp_prepare_boot_cpu(void);
# 27 "include/linux/sched.h" 2
# 1 "include/linux/sem.h" 1



# 1 "include/linux/ipc.h" 1
# 9 "include/linux/ipc.h"
struct ipc_perm
{
        __kernel_key_t key;
        __kernel_uid_t uid;
        __kernel_gid_t gid;
        __kernel_uid_t cuid;
        __kernel_gid_t cgid;
        __kernel_mode_t mode;
        unsigned short seq;
};


# 1 "include/asm/ipcbuf.h" 1
# 14 "include/asm/ipcbuf.h"
struct ipc64_perm
{
        __kernel_key_t key;
        __kernel_uid32_t uid;
        __kernel_gid32_t gid;
        __kernel_uid32_t cuid;
        __kernel_gid32_t cgid;
        __kernel_mode_t mode;
        unsigned short __pad1;
        unsigned short seq;
        unsigned short __pad2;
        unsigned long __unused1;
        unsigned long __unused2;
};
# 22 "include/linux/ipc.h" 2
# 57 "include/linux/ipc.h"
struct kern_ipc_perm
{
        spinlock_t lock;
        int deleted;
        key_t key;
        uid_t uid;
        gid_t gid;
        uid_t cuid;
        gid_t cgid;
        mode_t mode;
        unsigned long seq;
        void *security;
};
# 5 "include/linux/sem.h" 2
# 24 "include/linux/sem.h"
struct semid_ds {
        struct ipc_perm sem_perm;
        __kernel_time_t sem_otime;
        __kernel_time_t sem_ctime;
        struct sem *sem_base;
        struct sem_queue *sem_pending;
        struct sem_queue **sem_pending_last;
        struct sem_undo *undo;
        unsigned short sem_nsems;
};


# 1 "include/asm/sembuf.h" 1
# 14 "include/asm/sembuf.h"
struct semid64_ds {
        struct ipc64_perm sem_perm;
        __kernel_time_t sem_otime;
        unsigned long __unused1;
        __kernel_time_t sem_ctime;
        unsigned long __unused2;
        unsigned long sem_nsems;
        unsigned long __unused3;
        unsigned long __unused4;
};
# 37 "include/linux/sem.h" 2


struct sembuf {
        unsigned short sem_num;
        short sem_op;
        short sem_flg;
};


union semun {
        int val;
        struct semid_ds *buf;
        unsigned short *array;
        struct seminfo *__buf;
        void *__pad;
};

struct seminfo {
        int semmap;
        int semmni;
        int semmns;
        int semmnu;
        int semmsl;
        int semopm;
        int semume;
        int semusz;
        int semvmx;
        int semaem;
};
# 83 "include/linux/sem.h"
struct sem {
        int semval;
        int sempid;
};


struct sem_array {
        struct kern_ipc_perm sem_perm;
        int sem_id;
        time_t sem_otime;
        time_t sem_ctime;
        struct sem *sem_base;
        struct sem_queue *sem_pending;
        struct sem_queue **sem_pending_last;
        struct sem_undo *undo;
        unsigned long sem_nsems;
};


struct sem_queue {
        struct sem_queue * next;
        struct sem_queue ** prev;
        struct task_struct* sleeper;
        struct sem_undo * undo;
        int pid;
        int status;
        struct sem_array * sma;
        int id;
        struct sembuf * sops;
        int nsops;
        int alter;
};




struct sem_undo {
        struct sem_undo * proc_next;
        struct sem_undo * id_next;
        int semid;
        short * semadj;
};




struct sem_undo_list {
        atomic_t refcnt;
        spinlock_t lock;
        struct sem_undo *proc_list;
};

struct sysv_sem {
        struct sem_undo_list *undo_list;
};



extern int copy_semundo(unsigned long clone_flags, struct task_struct *tsk);
extern void exit_sem(struct task_struct *tsk);
# 28 "include/linux/sched.h" 2
# 1 "include/linux/signal.h" 1





# 1 "include/asm/signal.h" 1
# 10 "include/asm/signal.h"
struct siginfo;
# 20 "include/asm/signal.h"
typedef unsigned long old_sigset_t;

typedef struct {
        unsigned long sig[(64 / 32)];
} sigset_t;
# 113 "include/asm/signal.h"
# 1 "include/asm-generic/signal.h" 1
# 12 "include/asm-generic/signal.h"
typedef void __signalfn_t(int);
typedef __signalfn_t *__sighandler_t;

typedef void __restorefn_t(void);
typedef __restorefn_t *__sigrestore_t;
# 114 "include/asm/signal.h" 2


struct old_sigaction {
        __sighandler_t sa_handler;
        old_sigset_t sa_mask;
        unsigned long sa_flags;
        __sigrestore_t sa_restorer;
};

struct sigaction {
        __sighandler_t sa_handler;
        unsigned long sa_flags;
        __sigrestore_t sa_restorer;
        sigset_t sa_mask;
};

struct k_sigaction {
        struct sigaction sa;
};
# 151 "include/asm/signal.h"
typedef struct sigaltstack {
        void *ss_sp;
        int ss_flags;
        size_t ss_size;
} stack_t;






static __inline__ __attribute__((always_inline)) void sigaddset(sigset_t *set, int _sig)
{
        __asm__("btsl %1,%0" : "=m"(*set) : "Ir"(_sig - 1) : "cc");
}

static __inline__ __attribute__((always_inline)) void sigdelset(sigset_t *set, int _sig)
{
        __asm__("btrl %1,%0" : "=m"(*set) : "Ir"(_sig - 1) : "cc");
}

static __inline__ __attribute__((always_inline)) int __const_sigismember(sigset_t *set, int _sig)
{
        unsigned long sig = _sig - 1;
        return 1 & (set->sig[sig / 32] >> (sig % 32));
}

static __inline__ __attribute__((always_inline)) int __gen_sigismember(sigset_t *set, int _sig)
{
        int ret;
        __asm__("btl %2,%1\n\tsbbl %0,%0"
                : "=r"(ret) : "m"(*set), "Ir"(_sig-1) : "cc");
        return ret;
}






static __inline__ __attribute__((always_inline)) int sigfindinword(unsigned long word)
{
        __asm__("bsfl %1,%0" : "=r"(word) : "rm"(word) : "cc");
        return word;
}

struct pt_regs;
extern int do_signal(struct pt_regs *regs, sigset_t *oldset) __attribute__((regparm(3)));
# 7 "include/linux/signal.h" 2
# 1 "include/asm/siginfo.h" 1



# 1 "include/asm-generic/siginfo.h" 1






typedef union sigval {
        int sival_int;
        void *sival_ptr;
} sigval_t;
# 40 "include/asm-generic/siginfo.h"
typedef struct siginfo {
        int si_signo;
        int si_errno;
        int si_code;

        union {
                int _pad[((128 - (3 * sizeof(int))) / sizeof(int))];


                struct {
                        pid_t _pid;
                        uid_t _uid;
                } _kill;


                struct {
                        timer_t _tid;
                        int _overrun;
                        char _pad[sizeof( uid_t) - sizeof(int)];
                        sigval_t _sigval;
                        int _sys_private;
                } _timer;


                struct {
                        pid_t _pid;
                        uid_t _uid;
                        sigval_t _sigval;
                } _rt;


                struct {
                        pid_t _pid;
                        uid_t _uid;
                        int _status;
                        clock_t _utime;
                        clock_t _stime;
                } _sigchld;


                struct {
                        void *_addr;



                } _sigfault;


                struct {
                        long _band;
                        int _fd;
                } _sigpoll;
        } _sifields;
} siginfo_t;
# 251 "include/asm-generic/siginfo.h"
typedef struct sigevent {
        sigval_t sigev_value;
        int sigev_signo;
        int sigev_notify;
        union {
                int _pad[((64 - (sizeof(int) * 2 + sizeof(sigval_t))) / sizeof(int))];
                 int _tid;

                struct {
                        void (*_function)(sigval_t);
                        void *_attribute;
                } _sigev_thread;
        } _sigev_un;
} sigevent_t;







struct siginfo;
void do_schedule_next_timer(struct siginfo *info);





static inline __attribute__((always_inline)) void copy_siginfo(struct siginfo *to, struct siginfo *from)
{
        if (from->si_code < 0)
                (__builtin_constant_p(sizeof(*to)) ? __constant_memcpy((to),(from),(sizeof(*to))) : __memcpy((to),(from),(sizeof(*to))));
        else

                (__builtin_constant_p((3 * sizeof(int)) + sizeof(from->_sifields._sigchld)) ? __constant_memcpy((to),(from),((3 * sizeof(int)) + sizeof(from->_sifields._sigchld))) : __memcpy((to),(from),((3 * sizeof(int)) + sizeof(from->_sifields._sigchld))));
}



extern int copy_siginfo_to_user(struct siginfo *to, struct siginfo *from);
# 5 "include/asm/siginfo.h" 2
# 8 "include/linux/signal.h" 2
# 26 "include/linux/signal.h"
struct sigqueue {
        struct list_head list;
        spinlock_t *lock;
        int flags;
        siginfo_t info;
        struct user_struct *user;
};




struct sigpending {
        struct list_head list;
        sigset_t signal;
};
# 115 "include/linux/signal.h"
static inline __attribute__((always_inline)) void sigorsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) | (b3)); r->sig[2] = ((a2) | (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) | (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) | (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }


static inline __attribute__((always_inline)) void sigandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & (b3)); r->sig[2] = ((a2) & (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }


static inline __attribute__((always_inline)) void signandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & ~(b3)); r->sig[2] = ((a2) & ~(b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & ~(b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & ~(b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }
# 145 "include/linux/signal.h"
static inline __attribute__((always_inline)) void signotset(sigset_t *set) { extern void _NSIG_WORDS_is_unsupported_size(void); switch ((64 / 32)) { case 4: set->sig[3] = (~(set->sig[3])); set->sig[2] = (~(set->sig[2])); case 2: set->sig[1] = (~(set->sig[1])); case 1: set->sig[0] = (~(set->sig[0])); break; default: _NSIG_WORDS_is_unsupported_size(); } }




static inline __attribute__((always_inline)) void sigemptyset(sigset_t *set)
{
        switch ((64 / 32)) {
        default:
                (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(sigset_t))) ? __constant_c_and_count_memset(((set)),((0x01010101UL*(unsigned char)(0))),((sizeof(sigset_t)))) : __constant_c_memset(((set)),((0x01010101UL*(unsigned char)(0))),((sizeof(sigset_t))))) : (__builtin_constant_p((sizeof(sigset_t))) ? __memset_generic((((set))),(((0))),(((sizeof(sigset_t))))) : __memset_generic(((set)),((0)),((sizeof(sigset_t))))));
                break;
        case 2: set->sig[1] = 0;
        case 1: set->sig[0] = 0;
                break;
        }
}

static inline __attribute__((always_inline)) void sigfillset(sigset_t *set)
{
        switch ((64 / 32)) {
        default:
                (__builtin_constant_p(-1) ? (__builtin_constant_p((sizeof(sigset_t))) ? __constant_c_and_count_memset(((set)),((0x01010101UL*(unsigned char)(-1))),((sizeof(sigset_t)))) : __constant_c_memset(((set)),((0x01010101UL*(unsigned char)(-1))),((sizeof(sigset_t))))) : (__builtin_constant_p((sizeof(sigset_t))) ? __memset_generic((((set))),(((-1))),(((sizeof(sigset_t))))) : __memset_generic(((set)),((-1)),((sizeof(sigset_t))))));
                break;
        case 2: set->sig[1] = -1;
        case 1: set->sig[0] = -1;
                break;
        }
}



static inline __attribute__((always_inline)) void sigaddsetmask(sigset_t *set, unsigned long mask)
{
        set->sig[0] |= mask;
}

static inline __attribute__((always_inline)) void sigdelsetmask(sigset_t *set, unsigned long mask)
{
        set->sig[0] &= ~mask;
}

static inline __attribute__((always_inline)) int sigtestsetmask(sigset_t *set, unsigned long mask)
{
        return (set->sig[0] & mask) != 0;
}

static inline __attribute__((always_inline)) void siginitset(sigset_t *set, unsigned long mask)
{
        set->sig[0] = mask;
        switch ((64 / 32)) {
        default:
                (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(long)*((64 / 32)-1))) ? __constant_c_and_count_memset(((&set->sig[1])),((0x01010101UL*(unsigned char)(0))),((sizeof(long)*((64 / 32)-1)))) : __constant_c_memset(((&set->sig[1])),((0x01010101UL*(unsigned char)(0))),((sizeof(long)*((64 / 32)-1))))) : (__builtin_constant_p((sizeof(long)*((64 / 32)-1))) ? __memset_generic((((&set->sig[1]))),(((0))),(((sizeof(long)*((64 / 32)-1))))) : __memset_generic(((&set->sig[1])),((0)),((sizeof(long)*((64 / 32)-1))))));
                break;
        case 2: set->sig[1] = 0;
        case 1: ;
        }
}

static inline __attribute__((always_inline)) void siginitsetinv(sigset_t *set, unsigned long mask)
{
        set->sig[0] = ~mask;
        switch ((64 / 32)) {
        default:
                (__builtin_constant_p(-1) ? (__builtin_constant_p((sizeof(long)*((64 / 32)-1))) ? __constant_c_and_count_memset(((&set->sig[1])),((0x01010101UL*(unsigned char)(-1))),((sizeof(long)*((64 / 32)-1)))) : __constant_c_memset(((&set->sig[1])),((0x01010101UL*(unsigned char)(-1))),((sizeof(long)*((64 / 32)-1))))) : (__builtin_constant_p((sizeof(long)*((64 / 32)-1))) ? __memset_generic((((&set->sig[1]))),(((-1))),(((sizeof(long)*((64 / 32)-1))))) : __memset_generic(((&set->sig[1])),((-1)),((sizeof(long)*((64 / 32)-1))))));
                break;
        case 2: set->sig[1] = -1;
        case 1: ;
        }
}



static inline __attribute__((always_inline)) void init_sigpending(struct sigpending *sig)
{
        sigemptyset(&sig->signal);
        do { (&sig->list)->next = (&sig->list); (&sig->list)->prev = (&sig->list); } while (0);
}


static inline __attribute__((always_inline)) int valid_signal(unsigned long sig)
{
        return sig <= 64 ? 1 : 0;
}

extern int group_send_sig_info(int sig, struct siginfo *info, struct task_struct *p);
extern int __group_send_sig_info(int, struct siginfo *, struct task_struct *);
extern long do_sigpending(void *, unsigned long);
extern int sigprocmask(int, sigset_t *, sigset_t *);

struct pt_regs;
extern int get_signal_to_deliver(siginfo_t *info, struct k_sigaction *return_ka, struct pt_regs *regs, void *cookie);
# 29 "include/linux/sched.h" 2
# 1 "include/linux/securebits.h" 1





extern unsigned securebits;
# 30 "include/linux/sched.h" 2
# 1 "include/linux/fs_struct.h" 1



struct dentry;
struct vfsmount;

struct fs_struct {
        atomic_t count;
        rwlock_t lock;
        int umask;
        struct dentry * root, * pwd, * altroot;
        struct vfsmount * rootmnt, * pwdmnt, * altrootmnt;
};







extern void exit_fs(struct task_struct *);
extern void set_fs_altroot(void);
extern void set_fs_root(struct fs_struct *, struct vfsmount *, struct dentry *);
extern void set_fs_pwd(struct fs_struct *, struct vfsmount *, struct dentry *);
extern struct fs_struct *copy_fs_struct(struct fs_struct *);
extern void put_fs_struct(struct fs_struct *);
# 31 "include/linux/sched.h" 2

# 1 "include/linux/completion.h" 1
# 13 "include/linux/completion.h"
struct completion {
        unsigned int done;
        wait_queue_head_t wait;
};







static inline __attribute__((always_inline)) void init_completion(struct completion *x)
{
        x->done = 0;
        init_waitqueue_head(&x->wait);
}

extern void wait_for_completion(struct completion *) __attribute__((regparm(3)));
extern int wait_for_completion_interruptible(struct completion *x) __attribute__((regparm(3)));
extern unsigned long wait_for_completion_timeout(struct completion *x, unsigned long timeout) __attribute__((regparm(3)));

extern unsigned long wait_for_completion_interruptible_timeout( struct completion *x, unsigned long timeout) __attribute__((regparm(3)));


extern void complete(struct completion *) __attribute__((regparm(3)));
extern void complete_all(struct completion *) __attribute__((regparm(3)));
# 33 "include/linux/sched.h" 2
# 1 "include/linux/pid.h" 1



enum pid_type
{
        PIDTYPE_PID,
        PIDTYPE_TGID,
        PIDTYPE_PGID,
        PIDTYPE_SID,
        PIDTYPE_MAX
};

struct pid
{

        int nr;
        struct hlist_node pid_chain;

        struct list_head pid_list;
};
# 29 "include/linux/pid.h"
extern int attach_pid(struct task_struct *task, enum pid_type type, int nr) __attribute__((regparm(3)));

extern void detach_pid(struct task_struct *task, enum pid_type) __attribute__((regparm(3)));





extern struct pid *find_pid(enum pid_type, int) __attribute__((regparm(3)));

extern int alloc_pidmap(void);
extern void free_pidmap(int) __attribute__((regparm(3)));
extern void switch_exec_pids(struct task_struct *leader, struct task_struct *thread);
# 34 "include/linux/sched.h" 2
# 1 "include/linux/percpu.h" 1



# 1 "include/linux/slab.h" 1
# 12 "include/linux/slab.h"
typedef struct kmem_cache_s kmem_cache_t;


# 1 "include/linux/gfp.h" 1



# 1 "include/linux/mmzone.h" 1
# 14 "include/linux/mmzone.h"
# 1 "include/linux/init.h" 1
# 64 "include/linux/init.h"
typedef int (*initcall_t)(void);
typedef void (*exitcall_t)(void);

extern initcall_t __con_initcall_start[], __con_initcall_end[];
extern initcall_t __security_initcall_start[], __security_initcall_end[];


extern char saved_command_line[];
# 110 "include/linux/init.h"
struct obs_kernel_param {
        const char *str;
        int (*setup_func)(char *);
        int early;
};
# 145 "include/linux/init.h"
void __attribute__ ((__section__ (".init.text"))) parse_early_param(void);
# 15 "include/linux/mmzone.h" 2
# 24 "include/linux/mmzone.h"
struct free_area {
        struct list_head free_list;
        unsigned long nr_free;
};

struct pglist_data;
# 38 "include/linux/mmzone.h"
struct zone_padding {
        char x[0];
} __attribute__((__aligned__(1 << (7))));





struct per_cpu_pages {
        int count;
        int low;
        int high;
        int batch;
        struct list_head list;
};

struct per_cpu_pageset {
        struct per_cpu_pages pcp[2];
# 64 "include/linux/mmzone.h"
} __attribute__((__aligned__((1 << (4)))));
# 117 "include/linux/mmzone.h"
struct zone {

        unsigned long free_pages;
        unsigned long pages_min, pages_low, pages_high;
# 129 "include/linux/mmzone.h"
        unsigned long lowmem_reserve[3];




        struct per_cpu_pageset pageset[8];




        spinlock_t lock;
        struct free_area free_area[11];


        struct zone_padding _pad1_;


        spinlock_t lru_lock;
        struct list_head active_list;
        struct list_head inactive_list;
        unsigned long nr_scan_active;
        unsigned long nr_scan_inactive;
        unsigned long nr_active;
        unsigned long nr_inactive;
        unsigned long pages_scanned;
        int all_unreclaimable;





        int reclaim_pages;

        atomic_t reclaim_in_progress;
# 180 "include/linux/mmzone.h"
        int temp_priority;
        int prev_priority;


        struct zone_padding _pad2_;
# 211 "include/linux/mmzone.h"
        wait_queue_head_t * wait_table;
        unsigned long wait_table_size;
        unsigned long wait_table_bits;




        struct pglist_data *zone_pgdat;
        struct page *zone_mem_map;

        unsigned long zone_start_pfn;

        unsigned long spanned_pages;
        unsigned long present_pages;




        char *name;
} __attribute__((__aligned__(1 << (7))));
# 251 "include/linux/mmzone.h"
struct zonelist {
        struct zone *zones[(1 << 0) * 3 + 1];
};
# 267 "include/linux/mmzone.h"
struct bootmem_data;
typedef struct pglist_data {
        struct zone node_zones[3];
        struct zonelist node_zonelists[((0x03 + 1) / 2 + 1)];
        int nr_zones;

        struct page *node_mem_map;

        struct bootmem_data *bdata;
        unsigned long node_start_pfn;
        unsigned long node_present_pages;
        unsigned long node_spanned_pages;

        int node_id;
        struct pglist_data *pgdat_next;
        wait_queue_head_t kswapd_wait;
        struct task_struct *kswapd;
        int kswapd_max_order;
} pg_data_t;
# 296 "include/linux/mmzone.h"
extern struct pglist_data *pgdat_list;

void __get_zone_counts(unsigned long *active, unsigned long *inactive,
                        unsigned long *free, struct pglist_data *pgdat);
void get_zone_counts(unsigned long *active, unsigned long *inactive,
                        unsigned long *free);
void build_all_zonelists(void);
void wakeup_kswapd(struct zone *zone, int order);
int zone_watermark_ok(struct zone *z, int order, unsigned long mark,
                int alloc_type, int can_try_harder, int gfp_high);




static inline __attribute__((always_inline)) void memory_present(int nid, unsigned long start, unsigned long end) {}
# 340 "include/linux/mmzone.h"
static inline __attribute__((always_inline)) struct zone *next_zone(struct zone *zone)
{
        pg_data_t *pgdat = zone->zone_pgdat;

        if (zone < pgdat->node_zones + 3 - 1)
                zone++;
        else if (pgdat->pgdat_next) {
                pgdat = pgdat->pgdat_next;
                zone = pgdat->node_zones;
        } else
                zone = ((void *)0);

        return zone;
}
# 373 "include/linux/mmzone.h"
static inline __attribute__((always_inline)) int is_highmem_idx(int idx)
{
        return (idx == 2);
}

static inline __attribute__((always_inline)) int is_normal_idx(int idx)
{
        return (idx == 1);
}






static inline __attribute__((always_inline)) int is_highmem(struct zone *zone)
{
        return zone == zone->zone_pgdat->node_zones + 2;
}

static inline __attribute__((always_inline)) int is_normal(struct zone *zone)
{
        return zone == zone->zone_pgdat->node_zones + 1;
}


struct ctl_table;
struct file;
int min_free_kbytes_sysctl_handler(struct ctl_table *, int, struct file *,
                                        void *, size_t *, loff_t *);
extern int sysctl_lowmem_reserve_ratio[3 -1];
int lowmem_reserve_ratio_sysctl_handler(struct ctl_table *, int, struct file *,
                                        void *, size_t *, loff_t *);

# 1 "include/linux/topology.h" 1
# 32 "include/linux/topology.h"
# 1 "include/linux/mmzone.h" 1
# 33 "include/linux/topology.h" 2

# 1 "include/asm/topology.h" 1
# 103 "include/asm/topology.h"
# 1 "include/asm-generic/topology.h" 1
# 104 "include/asm/topology.h" 2
# 35 "include/linux/topology.h" 2
# 408 "include/linux/mmzone.h" 2





extern struct pglist_data contig_page_data;
# 604 "include/linux/mmzone.h"
void memory_present(int nid, unsigned long start, unsigned long end);
unsigned long __attribute__ ((__section__ (".init.text"))) node_memmap_size_bytes(int, unsigned long, unsigned long);
# 5 "include/linux/gfp.h" 2




struct vm_area_struct;
# 84 "include/linux/gfp.h"
static inline __attribute__((always_inline)) void arch_free_page(struct page *page, int order) { }


extern struct page *
__alloc_pages(gfp_t, unsigned int, struct zonelist *) __attribute__((regparm(3)));

static inline __attribute__((always_inline)) struct page *alloc_pages_node(int nid, gfp_t gfp_mask,
                                                unsigned int order)
{
        if (__builtin_expect(!!(order >= 11), 0))
                return ((void *)0);

        return __alloc_pages(gfp_mask, order,
                (&contig_page_data)->node_zonelists + (gfp_mask & 0x03));
}
# 120 "include/linux/gfp.h"
extern unsigned long __get_free_pages(gfp_t gfp_mask, unsigned int order) __attribute__((regparm(3)));
extern unsigned long get_zeroed_page(gfp_t gfp_mask) __attribute__((regparm(3)));







extern void __free_pages(struct page *page, unsigned int order) __attribute__((regparm(3)));
extern void free_pages(unsigned long addr, unsigned int order) __attribute__((regparm(3)));
extern void free_hot_page(struct page *page) __attribute__((regparm(3)));
extern void free_cold_page(struct page *page) __attribute__((regparm(3)));




void page_alloc_init(void);



static inline __attribute__((always_inline)) void drain_remote_pages(void) { };
# 16 "include/linux/slab.h" 2
# 57 "include/linux/slab.h"
extern void __attribute__ ((__section__ (".init.text"))) kmem_cache_init(void);

extern kmem_cache_t *kmem_cache_create(const char *, size_t, size_t, unsigned long,
                                       void (*)(void *, kmem_cache_t *, unsigned long),
                                       void (*)(void *, kmem_cache_t *, unsigned long));
extern int kmem_cache_destroy(kmem_cache_t *);
extern int kmem_cache_shrink(kmem_cache_t *);
extern void *kmem_cache_alloc(kmem_cache_t *, gfp_t);
extern void kmem_cache_free(kmem_cache_t *, void *);
extern unsigned int kmem_cache_size(kmem_cache_t *);
extern const char *kmem_cache_name(kmem_cache_t *);
extern kmem_cache_t *kmem_find_general_cachep(size_t size, gfp_t gfpflags);


struct cache_sizes {
        size_t cs_size;
        kmem_cache_t *cs_cachep;
        kmem_cache_t *cs_dmacachep;
};
extern struct cache_sizes malloc_sizes[];
extern void *__kmalloc(size_t, gfp_t);

static inline __attribute__((always_inline)) void *kmalloc(size_t size, gfp_t flags)
{
        if (__builtin_constant_p(size)) {
                int i = 0;





# 1 "include/linux/kmalloc_sizes.h" 1

        if (size <= 32) goto found; else i++;

        if (size <= 64) goto found; else i++;

        if (size <= 96) goto found; else i++;

        if (size <= 128) goto found; else i++;

        if (size <= 192) goto found; else i++;

        if (size <= 256) goto found; else i++;
        if (size <= 512) goto found; else i++;
        if (size <= 1024) goto found; else i++;
        if (size <= 2048) goto found; else i++;
        if (size <= 4096) goto found; else i++;
        if (size <= 8192) goto found; else i++;
        if (size <= 16384) goto found; else i++;
        if (size <= 32768) goto found; else i++;
        if (size <= 65536) goto found; else i++;
        if (size <= 131072) goto found; else i++;
# 89 "include/linux/slab.h" 2

                {
                        extern void __you_cannot_kmalloc_that_much(void);
                        __you_cannot_kmalloc_that_much();
                }
found:
                return kmem_cache_alloc((flags & 0x01u) ?
                        malloc_sizes[i].cs_dmacachep :
                        malloc_sizes[i].cs_cachep, flags);
        }
        return __kmalloc(size, flags);
}

extern void *kzalloc(size_t, gfp_t);







static inline __attribute__((always_inline)) void *kcalloc(size_t n, size_t size, gfp_t flags)
{
        if (n != 0 && size > ((int)(~0U>>1)) / n)
                return ((void *)0);
        return kzalloc(n * size, flags);
}

extern void kfree(const void *);
extern unsigned int ksize(const void *);





static inline __attribute__((always_inline)) void *kmem_cache_alloc_node(kmem_cache_t *cachep, int flags, int node)
{
        return kmem_cache_alloc(cachep, flags);
}
static inline __attribute__((always_inline)) void *kmalloc_node(size_t size, gfp_t flags, int node)
{
        return kmalloc(size, flags);
}


extern int kmem_cache_reap(int) __attribute__((regparm(3)));
extern int kmem_ptr_validate(kmem_cache_t *cachep, void *ptr) __attribute__((regparm(3)));


extern kmem_cache_t *vm_area_cachep;
extern kmem_cache_t *names_cachep;
extern kmem_cache_t *files_cachep;
extern kmem_cache_t *filp_cachep;
extern kmem_cache_t *fs_cachep;
extern kmem_cache_t *signal_cachep;
extern kmem_cache_t *sighand_cachep;
extern kmem_cache_t *bio_cachep;

extern atomic_t slab_reclaim_pages;
# 5 "include/linux/percpu.h" 2
# 20 "include/linux/percpu.h"
struct percpu_data {
        void *ptrs[8];
        void *blkp;
};
# 36 "include/linux/percpu.h"
extern void *__alloc_percpu(size_t size, size_t align);
extern void free_percpu(const void *);
# 35 "include/linux/sched.h" 2

# 1 "include/linux/seccomp.h" 1
# 11 "include/linux/seccomp.h"
# 1 "include/asm/seccomp.h" 1
# 9 "include/asm/seccomp.h"
# 1 "include/linux/unistd.h" 1



extern int errno;




# 1 "include/asm/unistd.h" 1
# 10 "include/linux/unistd.h" 2
# 10 "include/asm/seccomp.h" 2
# 12 "include/linux/seccomp.h" 2

typedef struct { int mode; } seccomp_t;

extern void __secure_computing(int);
static inline __attribute__((always_inline)) void secure_computing(int this_syscall)
{
        if (__builtin_expect(!!(test_thread_flag(8)), 0))
                __secure_computing(this_syscall);
}

static inline __attribute__((always_inline)) int has_secure_computing(struct thread_info *ti)
{
        return __builtin_expect(!!(test_ti_thread_flag(ti, 8)), 0);
}
# 37 "include/linux/sched.h" 2

# 1 "include/linux/auxvec.h" 1



# 1 "include/asm/auxvec.h" 1
# 5 "include/linux/auxvec.h" 2
# 39 "include/linux/sched.h" 2

struct exec_domain;
# 80 "include/linux/sched.h"
extern unsigned long avenrun[];
# 94 "include/linux/sched.h"
extern unsigned long total_forks;
extern int nr_threads;
extern int last_pid;
extern __typeof__(unsigned long) per_cpu__process_counts;
extern int nr_processes(void);
extern unsigned long nr_running(void);
extern unsigned long nr_uninterruptible(void);
extern unsigned long nr_iowait(void);


# 1 "include/linux/param.h" 1



# 1 "include/asm/param.h" 1
# 5 "include/linux/param.h" 2
# 105 "include/linux/sched.h" 2
# 1 "include/linux/resource.h" 1
# 21 "include/linux/resource.h"
struct rusage {
        struct timeval ru_utime;
        struct timeval ru_stime;
        long ru_maxrss;
        long ru_ixrss;
        long ru_idrss;
        long ru_isrss;
        long ru_minflt;
        long ru_majflt;
        long ru_nswap;
        long ru_inblock;
        long ru_oublock;
        long ru_msgsnd;
        long ru_msgrcv;
        long ru_nsignals;
        long ru_nvcsw;
        long ru_nivcsw;
};

struct rlimit {
        unsigned long rlim_cur;
        unsigned long rlim_max;
};
# 68 "include/linux/resource.h"
# 1 "include/asm/resource.h" 1



# 1 "include/asm-generic/resource.h" 1
# 5 "include/asm/resource.h" 2
# 69 "include/linux/resource.h" 2
# 106 "include/linux/sched.h" 2
# 1 "include/linux/timer.h" 1
# 9 "include/linux/timer.h"
struct timer_base_s;

struct timer_list {
        struct list_head entry;
        unsigned long expires;

        unsigned long magic;

        void (*function)(unsigned long);
        unsigned long data;

        struct timer_base_s *base;
};



extern struct timer_base_s __init_timer_base;
# 39 "include/linux/timer.h"
void __attribute__((regparm(3))) init_timer(struct timer_list * timer);
# 51 "include/linux/timer.h"
static inline __attribute__((always_inline)) int timer_pending(const struct timer_list * timer)
{
        return timer->entry.next != ((void *)0);
}

extern void add_timer_on(struct timer_list *timer, int cpu);
extern int del_timer(struct timer_list * timer);
extern int __mod_timer(struct timer_list *timer, unsigned long expires);
extern int mod_timer(struct timer_list *timer, unsigned long expires);

extern unsigned long next_timer_interrupt(void);
# 77 "include/linux/timer.h"
static inline __attribute__((always_inline)) void add_timer(struct timer_list * timer)
{
        __mod_timer(timer, timer->expires);
}


  extern int try_to_del_timer_sync(struct timer_list *timer);
  extern int del_timer_sync(struct timer_list *timer);







extern void init_timers(void);
extern void run_local_timers(void);
extern void it_real_fn(unsigned long);
# 107 "include/linux/sched.h" 2
# 162 "include/linux/sched.h"
struct sched_param {
        int sched_priority;
};
# 176 "include/linux/sched.h"
extern rwlock_t tasklist_lock;
extern spinlock_t mmlist_lock;

typedef struct task_struct task_t;

extern void sched_init(void);
extern void sched_init_smp(void);
extern void init_idle(task_t *idle, int cpu);

extern cpumask_t nohz_cpu_mask;

extern void show_state(void);
extern void show_regs(struct pt_regs *);






extern void show_stack(struct task_struct *task, unsigned long *sp);

void io_schedule(void);
long io_schedule_timeout(long timeout);

extern void cpu_init (void);
extern void trap_init(void);
extern void update_process_times(int user);
extern void scheduler_tick(void);


extern void softlockup_tick(struct pt_regs *regs);
extern void spawn_softlockup_task(void);
extern void touch_softlockup_watchdog(void);
# 225 "include/linux/sched.h"
extern int in_sched_functions(unsigned long addr);


extern signed long schedule_timeout(signed long timeout) __attribute__((regparm(3)));
extern signed long schedule_timeout_interruptible(signed long timeout);
extern signed long schedule_timeout_uninterruptible(signed long timeout);
 __attribute__((regparm(0))) void schedule(void);

struct namespace;




extern int sysctl_max_map_count;

# 1 "include/linux/aio.h" 1




# 1 "include/linux/workqueue.h" 1
# 12 "include/linux/workqueue.h"
struct workqueue_struct;

struct work_struct {
        unsigned long pending;
        struct list_head entry;
        void (*func)(void *);
        void *data;
        void *wq_data;
        struct timer_list timer;
};
# 53 "include/linux/workqueue.h"
extern struct workqueue_struct *__create_workqueue(const char *name,
                                                    int singlethread);



extern void destroy_workqueue(struct workqueue_struct *wq);

extern int queue_work(struct workqueue_struct *wq, struct work_struct *work) __attribute__((regparm(3)));
extern int queue_delayed_work(struct workqueue_struct *wq, struct work_struct *work, unsigned long delay) __attribute__((regparm(3)));
extern void flush_workqueue(struct workqueue_struct *wq) __attribute__((regparm(3)));

extern int schedule_work(struct work_struct *work) __attribute__((regparm(3)));
extern int schedule_delayed_work(struct work_struct *work, unsigned long delay) __attribute__((regparm(3)));

extern int schedule_delayed_work_on(int cpu, struct work_struct *work, unsigned long delay);
extern void flush_scheduled_work(void);
extern int current_is_keventd(void);
extern int keventd_up(void);

extern void init_workqueues(void);
void cancel_rearming_delayed_work(struct work_struct *work);
void cancel_rearming_delayed_workqueue(struct workqueue_struct *,
                                       struct work_struct *);






static inline __attribute__((always_inline)) int cancel_delayed_work(struct work_struct *work)
{
        int ret;

        ret = del_timer_sync(&work->timer);
        if (ret)
                clear_bit(0, &work->pending);
        return ret;
}
# 6 "include/linux/aio.h" 2
# 1 "include/linux/aio_abi.h" 1
# 32 "include/linux/aio_abi.h"
typedef unsigned long aio_context_t;

enum {
        IOCB_CMD_PREAD = 0,
        IOCB_CMD_PWRITE = 1,
        IOCB_CMD_FSYNC = 2,
        IOCB_CMD_FDSYNC = 3,




        IOCB_CMD_NOOP = 6,
};


struct io_event {
        __u64 data;
        __u64 obj;
        __s64 res;
        __s64 res2;
};
# 68 "include/linux/aio_abi.h"
struct iocb {

        __u64 aio_data;
        __u32 aio_key, aio_reserved1;



        __u16 aio_lio_opcode;
        __s16 aio_reqprio;
        __u32 aio_fildes;

        __u64 aio_buf;
        __u64 aio_nbytes;
        __s64 aio_offset;


        __u64 aio_reserved2;
        __u64 aio_reserved3;
};
# 7 "include/linux/aio.h" 2






struct kioctx;
# 85 "include/linux/aio.h"
struct kiocb {
        struct list_head ki_run_list;
        long ki_flags;
        int ki_users;
        unsigned ki_key;

        struct file *ki_filp;
        struct kioctx *ki_ctx;
        int (*ki_cancel)(struct kiocb *, struct io_event *);
        ssize_t (*ki_retry)(struct kiocb *);
        void (*ki_dtor)(struct kiocb *);

        struct list_head ki_list;


        union {
                void *user;
                struct task_struct *tsk;
        } ki_obj;
        __u64 ki_user_data;
        loff_t ki_pos;

        unsigned short ki_opcode;
        size_t ki_nbytes;
        char *ki_buf;
        size_t ki_left;
        wait_queue_t ki_wait;
        long ki_retried;
        long ki_kicked;
        long ki_queued;

        void *private;
};
# 138 "include/linux/aio.h"
struct aio_ring {
        unsigned id;
        unsigned nr;
        unsigned head;
        unsigned tail;

        unsigned magic;
        unsigned compat_features;
        unsigned incompat_features;
        unsigned header_length;


        struct io_event io_events[0];
};




struct aio_ring_info {
        unsigned long mmap_base;
        unsigned long mmap_size;

        struct page **ring_pages;
        spinlock_t ring_lock;
        long nr_pages;

        unsigned nr, tail;

        struct page *internal_pages[8];
};

struct kioctx {
        atomic_t users;
        int dead;
        struct mm_struct *mm;


        unsigned long user_id;
        struct kioctx *next;

        wait_queue_head_t wait;

        spinlock_t ctx_lock;

        int reqs_active;
        struct list_head active_reqs;
        struct list_head run_list;

        unsigned max_reqs;

        struct aio_ring_info ring_info;

        struct work_struct wq;
};


extern unsigned aio_max_size;

extern ssize_t wait_on_sync_kiocb(struct kiocb *iocb) __attribute__((regparm(3)));
extern int aio_put_req(struct kiocb *iocb) __attribute__((regparm(3)));
extern void kick_iocb(struct kiocb *iocb) __attribute__((regparm(3)));
extern int aio_complete(struct kiocb *iocb, long res, long res2) __attribute__((regparm(3)));
extern void __put_ioctx(struct kioctx *ctx) __attribute__((regparm(3)));
struct mm_struct;
extern void exit_aio(struct mm_struct *mm) __attribute__((regparm(3)));
extern struct kioctx *lookup_ioctx(unsigned long ctx_id);
extern int io_submit_one(struct kioctx *ctx, struct iocb *user_iocb, struct iocb *iocb) __attribute__((regparm(3)));



struct kioctx *lookup_ioctx(unsigned long ctx_id);
int io_submit_one(struct kioctx *ctx, struct iocb *user_iocb, struct iocb *iocb) __attribute__((regparm(3)));
# 231 "include/linux/aio.h"
static inline __attribute__((always_inline)) struct kiocb *list_kiocb(struct list_head *h)
{
        return ({ const typeof( ((struct kiocb *)0)->ki_list ) *__mptr = (h); (struct kiocb *)( (char *)__mptr - ((size_t) &((struct kiocb *)0)->ki_list) );});
}


extern atomic_t aio_nr;
extern unsigned aio_max_nr;
# 241 "include/linux/sched.h" 2

extern unsigned long
arch_get_unmapped_area(struct file *, unsigned long, unsigned long,
                       unsigned long, unsigned long);
extern unsigned long
arch_get_unmapped_area_topdown(struct file *filp, unsigned long addr,
                          unsigned long len, unsigned long pgoff,
                          unsigned long flags);
extern void arch_unmap_area(struct mm_struct *, unsigned long);
extern void arch_unmap_area_topdown(struct mm_struct *, unsigned long);






typedef unsigned long mm_counter_t;

struct mm_struct {
        struct vm_area_struct * mmap;
        struct rb_root mm_rb;
        struct vm_area_struct * mmap_cache;
        unsigned long (*get_unmapped_area) (struct file *filp,
                                unsigned long addr, unsigned long len,
                                unsigned long pgoff, unsigned long flags);
        void (*unmap_area) (struct mm_struct *mm, unsigned long addr);
        unsigned long mmap_base;
        unsigned long cached_hole_size;
        unsigned long free_area_cache;
        pgd_t * pgd;
        atomic_t mm_users;
        atomic_t mm_count;
        int map_count;
        struct rw_semaphore mmap_sem;
        spinlock_t page_table_lock;

        struct list_head mmlist;




        unsigned long start_code, end_code, start_data, end_data;
        unsigned long start_brk, brk, start_stack;
        unsigned long arg_start, arg_end, env_start, env_end;
        unsigned long total_vm, locked_vm, shared_vm;
        unsigned long exec_vm, stack_vm, reserved_vm, def_flags, nr_ptes;


        mm_counter_t _rss;
        mm_counter_t _anon_rss;

        unsigned long saved_auxv[42];

        unsigned dumpable:2;
        cpumask_t cpu_vm_mask;


        mm_context_t context;


        unsigned long swap_token_time;
        char recent_pagein;


        int core_waiters;
        struct completion *core_startup_done, core_done;


        rwlock_t ioctx_list_lock;
        struct kioctx *ioctx_list;

        struct kioctx default_kioctx;

        unsigned long hiwater_rss;
        unsigned long hiwater_vm;
};

struct sighand_struct {
        atomic_t count;
        struct k_sigaction action[64];
        spinlock_t siglock;
};
# 331 "include/linux/sched.h"
struct signal_struct {
        atomic_t count;
        atomic_t live;

        wait_queue_head_t wait_chldexit;


        task_t *curr_target;


        struct sigpending shared_pending;


        int group_exit_code;





        struct task_struct *group_exit_task;
        int notify_count;


        int group_stop_count;
        unsigned int flags;


        struct list_head posix_timers;


        struct timer_list real_timer;
        unsigned long it_real_value, it_real_incr;


        cputime_t it_prof_expires, it_virt_expires;
        cputime_t it_prof_incr, it_virt_incr;


        pid_t pgrp;
        pid_t tty_old_pgrp;
        pid_t session;

        int leader;

        struct tty_struct *tty;







        cputime_t utime, stime, cutime, cstime;
        unsigned long nvcsw, nivcsw, cnvcsw, cnivcsw;
        unsigned long min_flt, maj_flt, cmin_flt, cmaj_flt;







        unsigned long long sched_time;
# 404 "include/linux/sched.h"
        struct rlimit rlim[15];

        struct list_head cpu_timers[3];







};
# 453 "include/linux/sched.h"
struct user_struct {
        atomic_t __count;
        atomic_t processes;
        atomic_t files;
        atomic_t sigpending;

        atomic_t inotify_watches;
        atomic_t inotify_devs;


        unsigned long mq_bytes;
        unsigned long locked_shm;







        struct list_head uidhash_list;
        uid_t uid;
};

extern struct user_struct *find_user(uid_t);

extern struct user_struct root_user;


typedef struct prio_array prio_array_t;
struct backing_dev_info;
struct reclaim_state;
# 500 "include/linux/sched.h"
enum idle_type
{
        SCHED_IDLE,
        NOT_IDLE,
        NEWLY_IDLE,
        MAX_IDLE_TYPES
};
# 523 "include/linux/sched.h"
struct sched_group {
        struct sched_group *next;
        cpumask_t cpumask;





        unsigned long cpu_power;
};

struct sched_domain {

        struct sched_domain *parent;
        struct sched_group *groups;
        cpumask_t span;
        unsigned long min_interval;
        unsigned long max_interval;
        unsigned int busy_factor;
        unsigned int imbalance_pct;
        unsigned long long cache_hot_time;
        unsigned int cache_nice_tries;
        unsigned int per_cpu_gain;
        unsigned int busy_idx;
        unsigned int idle_idx;
        unsigned int newidle_idx;
        unsigned int wake_idx;
        unsigned int forkexec_idx;
        int flags;


        unsigned long last_balance;
        unsigned int balance_interval;
        unsigned int nr_balance_failed;
# 589 "include/linux/sched.h"
};

extern void partition_sched_domains(cpumask_t *partition1,
                                    cpumask_t *partition2);



struct io_context;
void exit_io_context(void);
struct cpuset;



struct group_info {
        int ngroups;
        atomic_t usage;
        gid_t small_block[32];
        int nblocks;
        gid_t *blocks[0];
};
# 625 "include/linux/sched.h"
extern struct group_info *groups_alloc(int gidsetsize);
extern void groups_free(struct group_info *group_info);
extern int set_current_groups(struct group_info *group_info);
extern int groups_search(struct group_info *group_info, gid_t grp);







static inline __attribute__((always_inline)) void prefetch_stack(struct task_struct *t) { }


struct audit_context;
struct mempolicy;

struct task_struct {
        volatile long state;
        struct thread_info *thread_info;
        atomic_t usage;
        unsigned long flags;
        unsigned long ptrace;

        int lock_depth;




        int prio, static_prio;
        struct list_head run_list;
        prio_array_t *array;

        unsigned short ioprio;

        unsigned long sleep_avg;
        unsigned long long timestamp, last_ran;
        unsigned long long sched_time;
        int activated;

        unsigned long policy;
        cpumask_t cpus_allowed;
        unsigned int time_slice, first_time_slice;





        struct list_head tasks;




        struct list_head ptrace_children;
        struct list_head ptrace_list;

        struct mm_struct *mm, *active_mm;


        struct linux_binfmt *binfmt;
        long exit_state;
        int exit_code, exit_signal;
        int pdeath_signal;

        unsigned long personality;
        unsigned did_exec:1;
        pid_t pid;
        pid_t tgid;





        struct task_struct *real_parent;
        struct task_struct *parent;




        struct list_head children;
        struct list_head sibling;
        struct task_struct *group_leader;


        struct pid pids[PIDTYPE_MAX];

        struct completion *vfork_done;
        int *set_child_tid;
        int *clear_child_tid;

        unsigned long rt_priority;
        cputime_t utime, stime;
        unsigned long nvcsw, nivcsw;
        struct timespec start_time;

        unsigned long min_flt, maj_flt;

        cputime_t it_prof_expires, it_virt_expires;
        unsigned long long it_sched_expires;
        struct list_head cpu_timers[3];


        uid_t uid,euid,suid,fsuid;
        gid_t gid,egid,sgid,fsgid;
        struct group_info *group_info;
        kernel_cap_t cap_effective, cap_inheritable, cap_permitted;
        unsigned keep_capabilities:1;
        struct user_struct *user;




        int oomkilladj;
        char comm[16];




        int link_count, total_link_count;

        struct sysv_sem sysvsem;

        struct thread_struct thread;

        struct fs_struct *fs;

        struct files_struct *files;

        struct namespace *namespace;

        struct signal_struct *signal;
        struct sighand_struct *sighand;

        sigset_t blocked, real_blocked;
        struct sigpending pending;

        unsigned long sas_ss_sp;
        size_t sas_ss_size;
        int (*notifier)(void *priv);
        void *notifier_data;
        sigset_t *notifier_mask;

        void *security;
        struct audit_context *audit_context;
        seccomp_t seccomp;


        u32 parent_exec_id;
        u32 self_exec_id;

        spinlock_t alloc_lock;

        spinlock_t proc_lock;


        void *journal_info;


        struct reclaim_state *reclaim_state;

        struct dentry *proc_dentry;
        struct backing_dev_info *backing_dev_info;

        struct io_context *io_context;

        unsigned long ptrace_message;
        siginfo_t *last_siginfo;






        wait_queue_t *io_wait;

        u64 rchar, wchar, syscr, syscw;

        u64 acct_rss_mem1;
        u64 acct_vm_mem1;
        clock_t acct_stimexpd;
# 815 "include/linux/sched.h"
        atomic_t fs_excl;
};

static inline __attribute__((always_inline)) pid_t process_group(struct task_struct *tsk)
{
        return tsk->signal->pgrp;
}
# 831 "include/linux/sched.h"
static inline __attribute__((always_inline)) int pid_alive(struct task_struct *p)
{
        return p->pids[PIDTYPE_PID].nr != 0;
}

extern void free_task(struct task_struct *tsk);
extern void __put_task_struct(struct task_struct *tsk);
# 894 "include/linux/sched.h"
extern int set_cpus_allowed(task_t *p, cpumask_t new_mask);
# 904 "include/linux/sched.h"
extern unsigned long long sched_clock(void);
extern unsigned long long current_sched_time(const task_t *current_task);



extern void sched_exec(void);







static inline __attribute__((always_inline)) void idle_task_exit(void) {}


extern void sched_idle_next(void);
extern void set_user_nice(task_t *p, long nice);
extern int task_prio(const task_t *p);
extern int task_nice(const task_t *p);
extern int can_nice(const task_t *p, const int nice);
extern int task_curr(const task_t *p);
extern int idle_cpu(int cpu);
extern int sched_setscheduler(struct task_struct *, int, struct sched_param *);
extern task_t *idle_task(int cpu);
extern task_t *curr_task(int cpu);
extern void set_curr_task(int cpu, task_t *p);

void yield(void);




extern struct exec_domain default_exec_domain;

union thread_union {
        struct thread_info thread_info;
        unsigned long stack[(8192)/sizeof(long)];
};


static inline __attribute__((always_inline)) int kstack_end(void *addr)
{



        return !(((unsigned long)addr+sizeof(void*)-1) & ((8192)-sizeof(void*)));
}


extern union thread_union init_thread_union;
extern struct task_struct init_task;

extern struct mm_struct init_mm;


extern struct task_struct *find_task_by_pid_type(int type, int pid);
extern void set_special_pids(pid_t session, pid_t pgrp);
extern void __set_special_pids(pid_t session, pid_t pgrp);


extern struct user_struct * alloc_uid(uid_t);
static inline __attribute__((always_inline)) struct user_struct *get_uid(struct user_struct *u)
{
        atomic_inc(&u->__count);
        return u;
}
extern void free_uid(struct user_struct *);
extern void switch_uid(struct user_struct *);



extern void do_timer(struct pt_regs *);

extern int wake_up_state(struct task_struct * tsk, unsigned int state) __attribute__((regparm(3)));
extern int wake_up_process(struct task_struct * tsk) __attribute__((regparm(3)));
extern void wake_up_new_task(struct task_struct * tsk, unsigned long clone_flags) __attribute__((regparm(3)));


 extern void kick_process(struct task_struct *tsk);



extern void sched_fork(task_t * p, int clone_flags) __attribute__((regparm(3)));
extern void sched_exit(task_t * p) __attribute__((regparm(3)));

extern int in_group_p(gid_t);
extern int in_egroup_p(gid_t);

extern void proc_caches_init(void);
extern void flush_signals(struct task_struct *);
extern void flush_signal_handlers(struct task_struct *, int force_default);
extern int dequeue_signal(struct task_struct *tsk, sigset_t *mask, siginfo_t *info);

static inline __attribute__((always_inline)) int dequeue_signal_lock(struct task_struct *tsk, sigset_t *mask, siginfo_t *info)
{
        unsigned long flags;
        int ret;

        flags = _spin_lock_irqsave(&tsk->sighand->siglock);
        ret = dequeue_signal(tsk, mask, info);
        _spin_unlock_irqrestore(&tsk->sighand->siglock, flags);

        return ret;
}

extern void block_all_signals(int (*notifier)(void *priv), void *priv,
                              sigset_t *mask);
extern void unblock_all_signals(void);
extern void release_task(struct task_struct * p);
extern int send_sig_info(int, struct siginfo *, struct task_struct *);
extern int send_group_sig_info(int, struct siginfo *, struct task_struct *);
extern int force_sigsegv(int, struct task_struct *);
extern int force_sig_info(int, struct siginfo *, struct task_struct *);
extern int __kill_pg_info(int sig, struct siginfo *info, pid_t pgrp);
extern int kill_pg_info(int, struct siginfo *, pid_t);
extern int kill_proc_info(int, struct siginfo *, pid_t);
extern int kill_proc_info_as_uid(int, struct siginfo *, pid_t, uid_t, uid_t);
extern void do_notify_parent(struct task_struct *, int);
extern void force_sig(int, struct task_struct *);
extern void force_sig_specific(int, struct task_struct *);
extern int send_sig(int, struct task_struct *, int);
extern void zap_other_threads(struct task_struct *p);
extern int kill_pg(pid_t, int, int);
extern int kill_sl(pid_t, int, int);
extern int kill_proc(pid_t, int, int);
extern struct sigqueue *sigqueue_alloc(void);
extern void sigqueue_free(struct sigqueue *);
extern int send_sigqueue(int, struct sigqueue *, struct task_struct *);
extern int send_group_sigqueue(int, struct sigqueue *, struct task_struct *);
extern int do_sigaction(int, const struct k_sigaction *, struct k_sigaction *);
extern int do_sigaltstack(const stack_t *, stack_t *, unsigned long);
# 1044 "include/linux/sched.h"
static inline __attribute__((always_inline)) int on_sig_stack(unsigned long sp)
{
        return (sp - get_current()->sas_ss_sp < get_current()->sas_ss_size);
}

static inline __attribute__((always_inline)) int sas_ss_flags(unsigned long sp)
{
        return (get_current()->sas_ss_size == 0 ? 2
                : on_sig_stack(sp) ? 1 : 0);
}






static inline __attribute__((always_inline)) int capable(int cap)
{
        if (((get_current()->cap_effective) & (1 << (cap)))) {
                get_current()->flags |= 0x00000100;
                return 1;
        }
        return 0;
}





extern struct mm_struct * mm_alloc(void);


extern void __mmdrop(struct mm_struct *) __attribute__((regparm(3)));
static inline __attribute__((always_inline)) void mmdrop(struct mm_struct * mm)
{
        if (atomic_dec_and_test(&mm->mm_count))
                __mmdrop(mm);
}


extern void mmput(struct mm_struct *);

extern struct mm_struct *get_task_mm(struct task_struct *task);

extern void mm_release(struct task_struct *, struct mm_struct *);

extern int copy_thread(int, unsigned long, unsigned long, unsigned long, struct task_struct *, struct pt_regs *);
extern void flush_thread(void);
extern void exit_thread(void);

extern void exit_files(struct task_struct *);
extern void exit_signal(struct task_struct *);
extern void __exit_signal(struct task_struct *);
extern void exit_sighand(struct task_struct *);
extern void __exit_sighand(struct task_struct *);
extern void exit_itimers(struct signal_struct *);

extern void do_group_exit(int);

extern void daemonize(const char *, ...);
extern int allow_signal(int);
extern int disallow_signal(int);
extern task_t *child_reaper;

extern int do_execve(char *, char * *, char * *, struct pt_regs *);
extern long do_fork(unsigned long, unsigned long, struct pt_regs *, unsigned long, int *, int *);
task_t *fork_idle(int);

extern void set_task_comm(struct task_struct *tsk, char *from);
extern void get_task_comm(char *to, struct task_struct *tsk);


extern void wait_task_inactive(task_t * p);
# 1152 "include/linux/sched.h"
extern task_t * next_thread(const task_t *p) __attribute__((regparm(3)));



static inline __attribute__((always_inline)) int thread_group_empty(task_t *p)
{
        return list_empty(&p->pids[PIDTYPE_TGID].pid_list);
}




extern void unhash_process(struct task_struct *p);
# 1175 "include/linux/sched.h"
static inline __attribute__((always_inline)) void task_lock(struct task_struct *p)
{
        _spin_lock(&p->alloc_lock);
}

static inline __attribute__((always_inline)) void task_unlock(struct task_struct *p)
{
        _spin_unlock(&p->alloc_lock);
}




static inline __attribute__((always_inline)) void set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        set_ti_thread_flag(tsk->thread_info,flag);
}

static inline __attribute__((always_inline)) void clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        clear_ti_thread_flag(tsk->thread_info,flag);
}

static inline __attribute__((always_inline)) int test_and_set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_and_set_ti_thread_flag(tsk->thread_info,flag);
}

static inline __attribute__((always_inline)) int test_and_clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_and_clear_ti_thread_flag(tsk->thread_info,flag);
}

static inline __attribute__((always_inline)) int test_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_ti_thread_flag(tsk->thread_info,flag);
}

static inline __attribute__((always_inline)) void set_tsk_need_resched(struct task_struct *tsk)
{
        set_tsk_thread_flag(tsk,3);
}

static inline __attribute__((always_inline)) void clear_tsk_need_resched(struct task_struct *tsk)
{
        clear_tsk_thread_flag(tsk,3);
}

static inline __attribute__((always_inline)) int signal_pending(struct task_struct *p)
{
        return __builtin_expect(!!(test_tsk_thread_flag(p,2)), 0);
}

static inline __attribute__((always_inline)) int need_resched(void)
{
        return __builtin_expect(!!(test_thread_flag(3)), 0);
}
# 1240 "include/linux/sched.h"
extern int cond_resched(void);
extern int cond_resched_lock(spinlock_t * lock);
extern int cond_resched_softirq(void);
# 1258 "include/linux/sched.h"
static inline __attribute__((always_inline)) int lock_need_resched(spinlock_t *lock)
{
        if (0 || need_resched())
                return 1;
        return 0;
}





extern void recalc_sigpending_tsk(struct task_struct *t) __attribute__((regparm(3)));
extern void recalc_sigpending(void);

extern void signal_wake_up(struct task_struct *t, int resume_stopped);






static inline __attribute__((always_inline)) unsigned int task_cpu(const struct task_struct *p)
{
        return p->thread_info->cpu;
}

static inline __attribute__((always_inline)) void set_task_cpu(struct task_struct *p, unsigned int cpu)
{
        p->thread_info->cpu = cpu;
}
# 1303 "include/linux/sched.h"
extern void arch_pick_mmap_layout(struct mm_struct *mm);
# 1313 "include/linux/sched.h"
extern long sched_setaffinity(pid_t pid, cpumask_t new_mask);
extern long sched_getaffinity(pid_t pid, cpumask_t *mask);



extern void normalize_rt_tasks(void);







static inline __attribute__((always_inline)) int frozen(struct task_struct *p)
{
        return p->flags & 0x00010000;
}




static inline __attribute__((always_inline)) int freezing(struct task_struct *p)
{
        return p->flags & 0x00004000;
}





static inline __attribute__((always_inline)) void freeze(struct task_struct *p)
{
        p->flags |= 0x00004000;
}




static inline __attribute__((always_inline)) int thaw_process(struct task_struct *p)
{
        if (frozen(p)) {
                p->flags &= ~0x00010000;
                wake_up_process(p);
                return 1;
        }
        return 0;
}




static inline __attribute__((always_inline)) void frozen_process(struct task_struct *p)
{
        p->flags = (p->flags & ~0x00004000) | 0x00010000;
}

extern void refrigerator(void);
extern int freeze_processes(void);
extern void thaw_processes(void);

static inline __attribute__((always_inline)) int try_to_freeze(void)
{
        if (freezing(get_current())) {
                refrigerator();
                return 1;
        } else
                return 0;
}
# 11 "include/linux/module.h" 2


# 1 "include/linux/stat.h" 1





# 1 "include/asm/stat.h" 1



struct __old_kernel_stat {
        unsigned short st_dev;
        unsigned short st_ino;
        unsigned short st_mode;
        unsigned short st_nlink;
        unsigned short st_uid;
        unsigned short st_gid;
        unsigned short st_rdev;
        unsigned long st_size;
        unsigned long st_atime;
        unsigned long st_mtime;
        unsigned long st_ctime;
};

struct stat {
        unsigned long st_dev;
        unsigned long st_ino;
        unsigned short st_mode;
        unsigned short st_nlink;
        unsigned short st_uid;
        unsigned short st_gid;
        unsigned long st_rdev;
        unsigned long st_size;
        unsigned long st_blksize;
        unsigned long st_blocks;
        unsigned long st_atime;
        unsigned long st_atime_nsec;
        unsigned long st_mtime;
        unsigned long st_mtime_nsec;
        unsigned long st_ctime;
        unsigned long st_ctime_nsec;
        unsigned long __unused4;
        unsigned long __unused5;
};




struct stat64 {
        unsigned long long st_dev;
        unsigned char __pad0[4];


        unsigned long __st_ino;

        unsigned int st_mode;
        unsigned int st_nlink;

        unsigned long st_uid;
        unsigned long st_gid;

        unsigned long long st_rdev;
        unsigned char __pad3[4];

        long long st_size;
        unsigned long st_blksize;

        unsigned long st_blocks;
        unsigned long __pad4;

        unsigned long st_atime;
        unsigned long st_atime_nsec;

        unsigned long st_mtime;
        unsigned int st_mtime_nsec;

        unsigned long st_ctime;
        unsigned long st_ctime_nsec;

        unsigned long long st_ino;
};
# 7 "include/linux/stat.h" 2
# 59 "include/linux/stat.h"
struct kstat {
        unsigned long ino;
        dev_t dev;
        umode_t mode;
        unsigned int nlink;
        uid_t uid;
        gid_t gid;
        dev_t rdev;
        loff_t size;
        struct timespec atime;
        struct timespec mtime;
        struct timespec ctime;
        unsigned long blksize;
        unsigned long blocks;
};
# 14 "include/linux/module.h" 2


# 1 "include/linux/kmod.h" 1
# 32 "include/linux/kmod.h"
extern int request_module(const char * name, ...) __attribute__ ((format (printf, 1, 2)));






struct key;
extern int call_usermodehelper_keys(char *path, char *argv[], char *envp[],
                                    struct key *session_keyring, int wait);

static inline __attribute__((always_inline)) int
call_usermodehelper(char *path, char **argv, char **envp, int wait)
{
        return call_usermodehelper_keys(path, argv, envp, ((void *)0), wait);
}

extern void usermodehelper_init(void);
# 17 "include/linux/module.h" 2
# 1 "include/linux/elf.h" 1





# 1 "include/asm/elf.h" 1
# 9 "include/asm/elf.h"
# 1 "include/asm/user.h" 1
# 44 "include/asm/user.h"
struct user_i387_struct {
        long cwd;
        long swd;
        long twd;
        long fip;
        long fcs;
        long foo;
        long fos;
        long st_space[20];
};

struct user_fxsr_struct {
        unsigned short cwd;
        unsigned short swd;
        unsigned short twd;
        unsigned short fop;
        long fip;
        long fcs;
        long foo;
        long fos;
        long mxcsr;
        long reserved;
        long st_space[32];
        long xmm_space[32];
        long padding[56];
};







struct user_regs_struct {
        long ebx, ecx, edx, esi, edi, ebp, eax;
        unsigned short ds, __ds, es, __es;
        unsigned short fs, __fs, gs, __gs;
        long orig_eax, eip;
        unsigned short cs, __cs;
        long eflags, esp;
        unsigned short ss, __ss;
};




struct user{


  struct user_regs_struct regs;

  int u_fpvalid;

  struct user_i387_struct i387;

  unsigned long int u_tsize;
  unsigned long int u_dsize;
  unsigned long int u_ssize;
  unsigned long start_code;
  unsigned long start_stack;



  long int signal;
  int reserved;
  struct user_pt_regs * u_ar0;

  struct user_i387_struct* u_fpstate;
  unsigned long magic;
  char u_comm[32];
  int u_debugreg[8];
};
# 10 "include/asm/elf.h" 2




# 1 "include/linux/utsname.h" 1





struct oldold_utsname {
        char sysname[9];
        char nodename[9];
        char release[9];
        char version[9];
        char machine[9];
};



struct old_utsname {
        char sysname[65];
        char nodename[65];
        char release[65];
        char version[65];
        char machine[65];
};

struct new_utsname {
        char sysname[65];
        char nodename[65];
        char release[65];
        char version[65];
        char machine[65];
        char domainname[65];
};

extern struct new_utsname system_utsname;

extern struct rw_semaphore uts_sem;
# 15 "include/asm/elf.h" 2
# 29 "include/asm/elf.h"
typedef unsigned long elf_greg_t;


typedef elf_greg_t elf_gregset_t[(sizeof (struct user_regs_struct) / sizeof(elf_greg_t))];

typedef struct user_i387_struct elf_fpregset_t;
typedef struct user_fxsr_struct elf_fpxregset_t;
# 122 "include/asm/elf.h"
extern int dump_task_regs (struct task_struct *, elf_gregset_t *);
extern int dump_task_fpu (struct task_struct *, elf_fpregset_t *);
extern int dump_task_extended_fpu (struct task_struct *, struct user_fxsr_struct *);
# 133 "include/asm/elf.h"
extern void __kernel_vsyscall;
# 7 "include/linux/elf.h" 2
# 16 "include/linux/elf.h"
typedef __u32 Elf32_Addr;
typedef __u16 Elf32_Half;
typedef __u32 Elf32_Off;
typedef __s32 Elf32_Sword;
typedef __u32 Elf32_Word;


typedef __u64 Elf64_Addr;
typedef __u16 Elf64_Half;
typedef __s16 Elf64_SHalf;
typedef __u64 Elf64_Off;
typedef __s32 Elf64_Sword;
typedef __u32 Elf64_Word;
typedef __u64 Elf64_Xword;
typedef __s64 Elf64_Sxword;
# 162 "include/linux/elf.h"
typedef struct dynamic{
  Elf32_Sword d_tag;
  union{
    Elf32_Sword d_val;
    Elf32_Addr d_ptr;
  } d_un;
} Elf32_Dyn;

typedef struct {
  Elf64_Sxword d_tag;
  union {
    Elf64_Xword d_val;
    Elf64_Addr d_ptr;
  } d_un;
} Elf64_Dyn;
# 185 "include/linux/elf.h"
typedef struct elf32_rel {
  Elf32_Addr r_offset;
  Elf32_Word r_info;
} Elf32_Rel;

typedef struct elf64_rel {
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
} Elf64_Rel;

typedef struct elf32_rela{
  Elf32_Addr r_offset;
  Elf32_Word r_info;
  Elf32_Sword r_addend;
} Elf32_Rela;

typedef struct elf64_rela {
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
  Elf64_Sxword r_addend;
} Elf64_Rela;

typedef struct elf32_sym{
  Elf32_Word st_name;
  Elf32_Addr st_value;
  Elf32_Word st_size;
  unsigned char st_info;
  unsigned char st_other;
  Elf32_Half st_shndx;
} Elf32_Sym;

typedef struct elf64_sym {
  Elf64_Word st_name;
  unsigned char st_info;
  unsigned char st_other;
  Elf64_Half st_shndx;
  Elf64_Addr st_value;
  Elf64_Xword st_size;
} Elf64_Sym;




typedef struct elf32_hdr{
  unsigned char e_ident[16];
  Elf32_Half e_type;
  Elf32_Half e_machine;
  Elf32_Word e_version;
  Elf32_Addr e_entry;
  Elf32_Off e_phoff;
  Elf32_Off e_shoff;
  Elf32_Word e_flags;
  Elf32_Half e_ehsize;
  Elf32_Half e_phentsize;
  Elf32_Half e_phnum;
  Elf32_Half e_shentsize;
  Elf32_Half e_shnum;
  Elf32_Half e_shstrndx;
} Elf32_Ehdr;

typedef struct elf64_hdr {
  unsigned char e_ident[16];
  Elf64_Half e_type;
  Elf64_Half e_machine;
  Elf64_Word e_version;
  Elf64_Addr e_entry;
  Elf64_Off e_phoff;
  Elf64_Off e_shoff;
  Elf64_Word e_flags;
  Elf64_Half e_ehsize;
  Elf64_Half e_phentsize;
  Elf64_Half e_phnum;
  Elf64_Half e_shentsize;
  Elf64_Half e_shnum;
  Elf64_Half e_shstrndx;
} Elf64_Ehdr;







typedef struct elf32_phdr{
  Elf32_Word p_type;
  Elf32_Off p_offset;
  Elf32_Addr p_vaddr;
  Elf32_Addr p_paddr;
  Elf32_Word p_filesz;
  Elf32_Word p_memsz;
  Elf32_Word p_flags;
  Elf32_Word p_align;
} Elf32_Phdr;

typedef struct elf64_phdr {
  Elf64_Word p_type;
  Elf64_Word p_flags;
  Elf64_Off p_offset;
  Elf64_Addr p_vaddr;
  Elf64_Addr p_paddr;
  Elf64_Xword p_filesz;
  Elf64_Xword p_memsz;
  Elf64_Xword p_align;
} Elf64_Phdr;
# 324 "include/linux/elf.h"
typedef struct {
  Elf32_Word sh_name;
  Elf32_Word sh_type;
  Elf32_Word sh_flags;
  Elf32_Addr sh_addr;
  Elf32_Off sh_offset;
  Elf32_Word sh_size;
  Elf32_Word sh_link;
  Elf32_Word sh_info;
  Elf32_Word sh_addralign;
  Elf32_Word sh_entsize;
} Elf32_Shdr;

typedef struct elf64_shdr {
  Elf64_Word sh_name;
  Elf64_Word sh_type;
  Elf64_Xword sh_flags;
  Elf64_Addr sh_addr;
  Elf64_Off sh_offset;
  Elf64_Xword sh_size;
  Elf64_Word sh_link;
  Elf64_Word sh_info;
  Elf64_Xword sh_addralign;
  Elf64_Xword sh_entsize;
} Elf64_Shdr;
# 397 "include/linux/elf.h"
typedef struct elf32_note {
  Elf32_Word n_namesz;
  Elf32_Word n_descsz;
  Elf32_Word n_type;
} Elf32_Nhdr;


typedef struct elf64_note {
  Elf64_Word n_namesz;
  Elf64_Word n_descsz;
  Elf64_Word n_type;
} Elf64_Nhdr;



extern Elf32_Dyn _DYNAMIC [];
# 18 "include/linux/module.h" 2

# 1 "include/linux/kobject.h" 1
# 22 "include/linux/kobject.h"
# 1 "include/linux/sysfs.h" 1
# 15 "include/linux/sysfs.h"
struct kobject;
struct module;

struct attribute {
        const char * name;
        struct module * owner;
        mode_t mode;
};

struct attribute_group {
        const char * name;
        struct attribute ** attrs;
};
# 51 "include/linux/sysfs.h"
struct vm_area_struct;

struct bin_attribute {
        struct attribute attr;
        size_t size;
        void *private;
        ssize_t (*read)(struct kobject *, char *, loff_t, size_t);
        ssize_t (*write)(struct kobject *, char *, loff_t, size_t);
        int (*mmap)(struct kobject *, struct bin_attribute *attr,
                    struct vm_area_struct *vma);
};

struct sysfs_ops {
        ssize_t (*show)(struct kobject *, struct attribute *,char *);
        ssize_t (*store)(struct kobject *,struct attribute *,const char *, size_t);
};

struct sysfs_dirent {
        atomic_t s_count;
        struct list_head s_sibling;
        struct list_head s_children;
        void * s_element;
        int s_type;
        umode_t s_mode;
        struct dentry * s_dentry;
        struct iattr * s_iattr;
};
# 88 "include/linux/sysfs.h"
extern int
sysfs_create_dir(struct kobject *);

extern void
sysfs_remove_dir(struct kobject *);

extern int
sysfs_rename_dir(struct kobject *, const char *new_name);

extern int
sysfs_create_file(struct kobject *, const struct attribute *);

extern int
sysfs_update_file(struct kobject *, const struct attribute *);

extern int
sysfs_chmod_file(struct kobject *kobj, struct attribute *attr, mode_t mode);

extern void
sysfs_remove_file(struct kobject *, const struct attribute *);

extern int
sysfs_create_link(struct kobject * kobj, struct kobject * target, const char * name);

extern void
sysfs_remove_link(struct kobject *, const char * name);

int sysfs_create_bin_file(struct kobject * kobj, struct bin_attribute * attr);
int sysfs_remove_bin_file(struct kobject * kobj, struct bin_attribute * attr);

int sysfs_create_group(struct kobject *, const struct attribute_group *);
void sysfs_remove_group(struct kobject *, const struct attribute_group *);
# 23 "include/linux/kobject.h" 2


# 1 "include/linux/kref.h" 1
# 23 "include/linux/kref.h"
struct kref {
        atomic_t refcount;
};

void kref_init(struct kref *kref);
void kref_get(struct kref *kref);
int kref_put(struct kref *kref, void (*release) (struct kref *kref));
# 26 "include/linux/kobject.h" 2
# 1 "include/linux/kobject_uevent.h" 1
# 17 "include/linux/kobject_uevent.h"
extern char hotplug_path[];





typedef int kobject_action_t;
enum kobject_action {
        KOBJ_ADD = ( kobject_action_t) 0x01,
        KOBJ_REMOVE = ( kobject_action_t) 0x02,
        KOBJ_CHANGE = ( kobject_action_t) 0x03,
        KOBJ_MOUNT = ( kobject_action_t) 0x04,
        KOBJ_UMOUNT = ( kobject_action_t) 0x05,
        KOBJ_OFFLINE = ( kobject_action_t) 0x06,
        KOBJ_ONLINE = ( kobject_action_t) 0x07,
};



int kobject_uevent(struct kobject *kobj,
                   enum kobject_action action,
                   struct attribute *attr);
int kobject_uevent_atomic(struct kobject *kobj,
                          enum kobject_action action,
                          struct attribute *attr);
# 27 "include/linux/kobject.h" 2






extern u64 hotplug_seqnum;

struct kobject {
        const char * k_name;
        char name[20];
        struct kref kref;
        struct list_head entry;
        struct kobject * parent;
        struct kset * kset;
        struct kobj_type * ktype;
        struct dentry * dentry;
};

extern int kobject_set_name(struct kobject *, const char *, ...)
        __attribute__((format(printf,2,3)));

static inline __attribute__((always_inline)) const char * kobject_name(const struct kobject * kobj)
{
        return kobj->k_name;
}

extern void kobject_init(struct kobject *);
extern void kobject_cleanup(struct kobject *);

extern int kobject_add(struct kobject *);
extern void kobject_del(struct kobject *);

extern int kobject_rename(struct kobject *, const char *new_name);

extern int kobject_register(struct kobject *);
extern void kobject_unregister(struct kobject *);

extern struct kobject * kobject_get(struct kobject *);
extern void kobject_put(struct kobject *);

extern char * kobject_get_path(struct kobject *, int);

struct kobj_type {
        void (*release)(struct kobject *);
        struct sysfs_ops * sysfs_ops;
        struct attribute ** default_attrs;
};
# 95 "include/linux/kobject.h"
struct kset_hotplug_ops {
        int (*filter)(struct kset *kset, struct kobject *kobj);
        const char *(*name)(struct kset *kset, struct kobject *kobj);
        int (*hotplug)(struct kset *kset, struct kobject *kobj, char **envp,
                        int num_envp, char *buffer, int buffer_size);
};

struct kset {
        struct subsystem * subsys;
        struct kobj_type * ktype;
        struct list_head list;
        spinlock_t list_lock;
        struct kobject kobj;
        struct kset_hotplug_ops * hotplug_ops;
};


extern void kset_init(struct kset * k);
extern int kset_add(struct kset * k);
extern int kset_register(struct kset * k);
extern void kset_unregister(struct kset * k);

static inline __attribute__((always_inline)) struct kset * to_kset(struct kobject * kobj)
{
        return kobj ? ({ const typeof( ((struct kset *)0)->kobj ) *__mptr = (kobj); (struct kset *)( (char *)__mptr - ((size_t) &((struct kset *)0)->kobj) );}) : ((void *)0);
}

static inline __attribute__((always_inline)) struct kset * kset_get(struct kset * k)
{
        return k ? to_kset(kobject_get(&k->kobj)) : ((void *)0);
}

static inline __attribute__((always_inline)) void kset_put(struct kset * k)
{
        kobject_put(&k->kobj);
}

static inline __attribute__((always_inline)) struct kobj_type * get_ktype(struct kobject * k)
{
        if (k->kset && k->kset->ktype)
                return k->kset->ktype;
        else
                return k->ktype;
}

extern struct kobject * kset_find_obj(struct kset *, const char *);
# 151 "include/linux/kobject.h"
struct subsystem {
        struct kset kset;
        struct rw_semaphore rwsem;
};
# 174 "include/linux/kobject.h"
extern struct subsystem kernel_subsys;
# 221 "include/linux/kobject.h"
extern void subsystem_init(struct subsystem *);
extern int subsystem_register(struct subsystem *);
extern void subsystem_unregister(struct subsystem *);

static inline __attribute__((always_inline)) struct subsystem * subsys_get(struct subsystem * s)
{
        return s ? ({ const typeof( ((struct subsystem *)0)->kset ) *__mptr = (kset_get(&s->kset)); (struct subsystem *)( (char *)__mptr - ((size_t) &((struct subsystem *)0)->kset) );}) : ((void *)0);
}

static inline __attribute__((always_inline)) void subsys_put(struct subsystem * s)
{
        kset_put(&s->kset);
}

struct subsys_attribute {
        struct attribute attr;
        ssize_t (*show)(struct subsystem *, char *);
        ssize_t (*store)(struct subsystem *, const char *, size_t);
};

extern int subsys_create_file(struct subsystem * , struct subsys_attribute *);
extern void subsys_remove_file(struct subsystem * , struct subsys_attribute *);


void kobject_hotplug(struct kobject *kobj, enum kobject_action action);
int add_hotplug_env_var(char **envp, int num_envp, int *cur_index,
                        char *buffer, int buffer_size, int *cur_len,
                        const char *format, ...)
        __attribute__((format (printf, 7, 8)));
# 20 "include/linux/module.h" 2
# 1 "include/linux/moduleparam.h" 1
# 29 "include/linux/moduleparam.h"
struct kernel_param;


typedef int (*param_set_fn)(const char *val, struct kernel_param *kp);

typedef int (*param_get_fn)(char *buffer, struct kernel_param *kp);

struct kernel_param {
        const char *name;
        unsigned int perm;
        param_set_fn set;
        param_get_fn get;
        void *arg;
};


struct kparam_string {
        unsigned int maxlen;
        char *string;
};


struct kparam_array
{
        unsigned int max;
        unsigned int *num;
        param_set_fn set;
        param_get_fn get;
        unsigned int elemsize;
        void *elem;
};
# 95 "include/linux/moduleparam.h"
extern int parse_args(const char *name,
                      char *args,
                      struct kernel_param *params,
                      unsigned num,
                      int (*unknown)(char *param, char *val));







extern int param_set_byte(const char *val, struct kernel_param *kp);
extern int param_get_byte(char *buffer, struct kernel_param *kp);


extern int param_set_short(const char *val, struct kernel_param *kp);
extern int param_get_short(char *buffer, struct kernel_param *kp);


extern int param_set_ushort(const char *val, struct kernel_param *kp);
extern int param_get_ushort(char *buffer, struct kernel_param *kp);


extern int param_set_int(const char *val, struct kernel_param *kp);
extern int param_get_int(char *buffer, struct kernel_param *kp);


extern int param_set_uint(const char *val, struct kernel_param *kp);
extern int param_get_uint(char *buffer, struct kernel_param *kp);


extern int param_set_long(const char *val, struct kernel_param *kp);
extern int param_get_long(char *buffer, struct kernel_param *kp);


extern int param_set_ulong(const char *val, struct kernel_param *kp);
extern int param_get_ulong(char *buffer, struct kernel_param *kp);


extern int param_set_charp(const char *val, struct kernel_param *kp);
extern int param_get_charp(char *buffer, struct kernel_param *kp);


extern int param_set_bool(const char *val, struct kernel_param *kp);
extern int param_get_bool(char *buffer, struct kernel_param *kp);


extern int param_set_invbool(const char *val, struct kernel_param *kp);
extern int param_get_invbool(char *buffer, struct kernel_param *kp);
# 159 "include/linux/moduleparam.h"
extern int param_array_set(const char *val, struct kernel_param *kp);
extern int param_array_get(char *buffer, struct kernel_param *kp);

extern int param_set_copystring(const char *val, struct kernel_param *kp);
extern int param_get_string(char *buffer, struct kernel_param *kp);

int param_array(const char *name,
                const char *val,
                unsigned int min, unsigned int max,
                void *elem, int elemsize,
                int (*set)(const char *, struct kernel_param *kp),
                int *num);



struct module;

extern int module_param_sysfs_setup(struct module *mod,
                                    struct kernel_param *kparam,
                                    unsigned int num_params);

extern void module_param_sysfs_remove(struct module *mod);
# 21 "include/linux/module.h" 2
# 1 "include/asm/local.h" 1





typedef struct
{
        volatile unsigned long counter;
} local_t;






static __inline__ __attribute__((always_inline)) void local_inc(local_t *v)
{
        __asm__ __volatile__(
                "incl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}

static __inline__ __attribute__((always_inline)) void local_dec(local_t *v)
{
        __asm__ __volatile__(
                "decl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}

static __inline__ __attribute__((always_inline)) void local_add(unsigned long i, local_t *v)
{
        __asm__ __volatile__(
                "addl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}

static __inline__ __attribute__((always_inline)) void local_sub(unsigned long i, local_t *v)
{
        __asm__ __volatile__(
                "subl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}
# 22 "include/linux/module.h" 2

# 1 "include/asm/module.h" 1




struct mod_arch_specific
{
};
# 24 "include/linux/module.h" 2
# 35 "include/linux/module.h"
struct kernel_symbol
{
        unsigned long value;
        const char *name;
};

struct modversion_info
{
        unsigned long crc;
        char name[(64 - sizeof(unsigned long))];
};

struct module;

struct module_attribute {
        struct attribute attr;
        ssize_t (*show)(struct module_attribute *, struct module *, char *);
        ssize_t (*store)(struct module_attribute *, struct module *,
                         const char *, size_t count);
        void (*setup)(struct module *, const char *);
        int (*test)(struct module *);
        void (*free)(struct module *);
};

struct module_kobject
{
        struct kobject kobj;
        struct module *mod;
};


extern int init_module(void);
extern void cleanup_module(void);


struct exception_table_entry;

const struct exception_table_entry *
search_extable(const struct exception_table_entry *first,
               const struct exception_table_entry *last,
               unsigned long value);
void sort_extable(struct exception_table_entry *start,
                  struct exception_table_entry *finish);
void sort_main_extable(void);

extern struct subsystem module_subsys;
# 159 "include/linux/module.h"
const struct exception_table_entry *search_exception_tables(unsigned long add);

struct notifier_block;




void *__symbol_get(const char *symbol);
void *__symbol_get_gpl(const char *symbol);
# 203 "include/linux/module.h"
struct module_ref
{
        local_t count;
} __attribute__((__aligned__((1 << (4)))));

enum module_state
{
        MODULE_STATE_LIVE,
        MODULE_STATE_COMING,
        MODULE_STATE_GOING,
};



struct module_sect_attr
{
        struct module_attribute mattr;
        char name[32];
        unsigned long address;
};

struct module_sect_attrs
{
        struct attribute_group grp;
        struct module_sect_attr attrs[0];
};

struct module_param_attrs;

struct module
{
        enum module_state state;


        struct list_head list;


        char name[(64 - sizeof(unsigned long))];


        struct module_kobject mkobj;
        struct module_param_attrs *param_attrs;
        const char *version;
        const char *srcversion;


        const struct kernel_symbol *syms;
        unsigned int num_syms;
        const unsigned long *crcs;


        const struct kernel_symbol *gpl_syms;
        unsigned int num_gpl_syms;
        const unsigned long *gpl_crcs;


        unsigned int num_exentries;
        const struct exception_table_entry *extable;


        int (*init)(void);


        void *module_init;


        void *module_core;


        unsigned long init_size, core_size;


        unsigned long init_text_size, core_text_size;


        struct mod_arch_specific arch;


        int unsafe;


        int license_gplok;
# 302 "include/linux/module.h"
        Elf32_Sym *symtab;
        unsigned long num_symtab;
        char *strtab;


        struct module_sect_attrs *sect_attrs;



        void *percpu;



        char *args;
};




static inline __attribute__((always_inline)) int module_is_live(struct module *mod)
{
        return mod->state != MODULE_STATE_GOING;
}


struct module *module_text_address(unsigned long addr);
struct module *__module_text_address(unsigned long addr);



struct module *module_get_kallsym(unsigned int symnum,
                                  unsigned long *value,
                                  char *type,
                                  char namebuf[128]);


unsigned long module_kallsyms_lookup_name(const char *name);

int is_exported(const char *name, const struct module *mod);

extern void __module_put_and_exit(struct module *mod, long code)
        __attribute__((noreturn));
# 391 "include/linux/module.h"
static inline __attribute__((always_inline)) int try_module_get(struct module *module)
{
        return !module || module_is_live(module);
}
static inline __attribute__((always_inline)) void module_put(struct module *module)
{
}
static inline __attribute__((always_inline)) void __module_get(struct module *module)
{
}
# 424 "include/linux/module.h"
const char *module_address_lookup(unsigned long addr,
                                  unsigned long *symbolsize,
                                  unsigned long *offset,
                                  char **modname);


const struct exception_table_entry *search_module_extables(unsigned long addr);

int register_module_notifier(struct notifier_block * nb);
int unregister_module_notifier(struct notifier_block * nb);

extern void print_modules(void);

struct device_driver;
void module_add_driver(struct module *, struct device_driver *);
void module_remove_driver(struct device_driver *);
# 547 "include/linux/module.h"
struct obsolete_modparm {
        char name[64];
        char type[64-sizeof(void *)];
        void *addr;
};

static inline __attribute__((always_inline)) void MODULE_PARM_(void) { }
# 568 "include/linux/module.h"
extern void __attribute__((deprecated)) inter_module_register(const char *,
                struct module *, const void *);
extern void __attribute__((deprecated)) inter_module_unregister(const char *);
extern const void * __attribute__((deprecated)) inter_module_get_request(const char *,
                const char *);
extern void __attribute__((deprecated)) inter_module_put(const char *);
# 52 "drivers/ide/ide-disk.c" 2




# 1 "include/linux/mm.h" 1
# 14 "include/linux/mm.h"
# 1 "include/linux/prio_tree.h" 1
# 14 "include/linux/prio_tree.h"
struct raw_prio_tree_node {
        struct prio_tree_node *left;
        struct prio_tree_node *right;
        struct prio_tree_node *parent;
};

struct prio_tree_node {
        struct prio_tree_node *left;
        struct prio_tree_node *right;
        struct prio_tree_node *parent;
        unsigned long start;
        unsigned long last;
};

struct prio_tree_root {
        struct prio_tree_node *prio_tree_node;
        unsigned short index_bits;
        unsigned short raw;




};

struct prio_tree_iter {
        struct prio_tree_node *cur;
        unsigned long mask;
        unsigned long value;
        int size_level;

        struct prio_tree_root *root;
        unsigned long r_index;
        unsigned long h_index;
};

static inline __attribute__((always_inline)) void prio_tree_iter_init(struct prio_tree_iter *iter,
                struct prio_tree_root *root, unsigned long r_index, unsigned long h_index)
{
        iter->root = root;
        iter->r_index = r_index;
        iter->h_index = h_index;
        iter->cur = ((void *)0);
}
# 84 "include/linux/prio_tree.h"
static inline __attribute__((always_inline)) int prio_tree_empty(const struct prio_tree_root *root)
{
        return root->prio_tree_node == ((void *)0);
}

static inline __attribute__((always_inline)) int prio_tree_root(const struct prio_tree_node *node)
{
        return node->parent == node;
}

static inline __attribute__((always_inline)) int prio_tree_left_empty(const struct prio_tree_node *node)
{
        return node->left == node;
}

static inline __attribute__((always_inline)) int prio_tree_right_empty(const struct prio_tree_node *node)
{
        return node->right == node;
}


struct prio_tree_node *prio_tree_replace(struct prio_tree_root *root,
                struct prio_tree_node *old, struct prio_tree_node *node);
struct prio_tree_node *prio_tree_insert(struct prio_tree_root *root,
                struct prio_tree_node *node);
void prio_tree_remove(struct prio_tree_root *root, struct prio_tree_node *node);
struct prio_tree_node *prio_tree_next(struct prio_tree_iter *iter);
# 15 "include/linux/mm.h" 2
# 1 "include/linux/fs.h" 1
# 10 "include/linux/fs.h"
# 1 "include/linux/limits.h" 1
# 11 "include/linux/fs.h" 2
# 1 "include/linux/ioctl.h" 1



# 1 "include/asm/ioctl.h" 1
# 56 "include/asm/ioctl.h"
extern unsigned int __invalid_size_argument_for_IOC;
# 5 "include/linux/ioctl.h" 2
# 12 "include/linux/fs.h" 2
# 1 "include/linux/rcuref.h" 1
# 36 "include/linux/rcuref.h"
# 1 "include/linux/interrupt.h" 1
# 11 "include/linux/interrupt.h"
# 1 "include/linux/hardirq.h" 1





# 1 "include/linux/smp_lock.h" 1
# 12 "include/linux/smp_lock.h"
extern int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) __reacquire_kernel_lock(void);
extern void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) __release_kernel_lock(void);
# 35 "include/linux/smp_lock.h"
static inline __attribute__((always_inline)) int reacquire_kernel_lock(struct task_struct *task)
{
        if (__builtin_expect(!!(task->lock_depth >= 0), 0))
                __reacquire_kernel_lock();
        return 0;
}

extern void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) lock_kernel(void) ;
extern void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) unlock_kernel(void) ;
# 7 "include/linux/hardirq.h" 2
# 1 "include/asm/hardirq.h" 1





# 1 "include/linux/irq.h" 1
# 21 "include/linux/irq.h"
# 1 "include/asm/irq.h" 1
# 16 "include/asm/irq.h"
# 1 "include/asm/mach-default/irq_vectors.h" 1
# 87 "include/asm/mach-default/irq_vectors.h"
# 1 "include/asm/mach-default/irq_vectors_limits.h" 1
# 88 "include/asm/mach-default/irq_vectors.h" 2
# 17 "include/asm/irq.h" 2


static __inline__ __attribute__((always_inline)) int irq_canonicalize(int irq)
{
        return ((irq == 2) ? 9 : irq);
}

extern void release_vm86_irqs(struct task_struct *);
# 40 "include/asm/irq.h"
extern int irqbalance_disable(char *str);
# 22 "include/linux/irq.h" 2
# 46 "include/linux/irq.h"
struct hw_interrupt_type {
        const char * typename;
        unsigned int (*startup)(unsigned int irq);
        void (*shutdown)(unsigned int irq);
        void (*enable)(unsigned int irq);
        void (*disable)(unsigned int irq);
        void (*ack)(unsigned int irq);
        void (*end)(unsigned int irq);
        void (*set_affinity)(unsigned int irq, cpumask_t dest);




};

typedef struct hw_interrupt_type hw_irq_controller;
# 70 "include/linux/irq.h"
typedef struct irq_desc {
        hw_irq_controller *handler;
        void *handler_data;
        struct irqaction *action;
        unsigned int status;
        unsigned int depth;
        unsigned int irq_count;
        unsigned int irqs_unhandled;
        spinlock_t lock;

        unsigned int move_irq;

} __attribute__((__aligned__((1 << (4))))) irq_desc_t;

extern irq_desc_t irq_desc [224];


static inline __attribute__((always_inline)) irq_desc_t *
irq_descp (int irq)
{
        return irq_desc + irq;
}

# 1 "include/asm/hw_irq.h" 1
# 16 "include/asm/hw_irq.h"
# 1 "include/linux/profile.h" 1
# 15 "include/linux/profile.h"
struct proc_dir_entry;
struct pt_regs;


void __attribute__ ((__section__ (".init.text"))) profile_init(void);
void profile_tick(int, struct pt_regs *);
void profile_hit(int, void *);

void create_prof_cpu_mask(struct proc_dir_entry *);




enum profile_type {
        PROFILE_TASK_EXIT,
        PROFILE_MUNMAP
};



struct notifier_block;
struct task_struct;
struct mm_struct;


void profile_task_exit(struct task_struct * task);




int profile_handoff_task(struct task_struct * task);


void profile_munmap(unsigned long addr);

int task_handoff_register(struct notifier_block * n);
int task_handoff_unregister(struct notifier_block * n);

int profile_event_register(enum profile_type, struct notifier_block * n);
int profile_event_unregister(enum profile_type, struct notifier_block * n);

int register_timer_hook(int (*hook)(struct pt_regs *));
void unregister_timer_hook(int (*hook)(struct pt_regs *));


extern int (*timer_hook)(struct pt_regs *);

struct pt_regs;
# 17 "include/asm/hw_irq.h" 2


# 1 "include/asm/sections.h" 1




# 1 "include/asm-generic/sections.h" 1





extern char _text[], _stext[], _etext[];
extern char _data[], _sdata[], _edata[];
extern char __bss_start[], __bss_stop[];
extern char __init_begin[], __init_end[];
extern char _sinittext[], _einittext[];
extern char _sextratext[] __attribute__((weak));
extern char _eextratext[] __attribute__((weak));
extern char _end[];
extern char __per_cpu_start[], __per_cpu_end[];
extern char __kprobes_text_start[], __kprobes_text_end[];
# 6 "include/asm/sections.h" 2
# 20 "include/asm/hw_irq.h" 2

struct hw_interrupt_type;
# 30 "include/asm/hw_irq.h"
extern u8 irq_vector[(32 * 8)];



extern void (*interrupt[224])(void);


__attribute__((regparm(3))) void reschedule_interrupt(void);
__attribute__((regparm(3))) void invalidate_interrupt(void);
__attribute__((regparm(3))) void call_function_interrupt(void);



__attribute__((regparm(3))) void apic_timer_interrupt(void);
__attribute__((regparm(3))) void error_interrupt(void);
__attribute__((regparm(3))) void spurious_interrupt(void);
__attribute__((regparm(3))) void thermal_interrupt(struct pt_regs *);



void disable_8259A_irq(unsigned int irq);
void enable_8259A_irq(unsigned int irq);
int i8259A_irq_pending(unsigned int irq);
void make_8259A_irq(unsigned int irq);
void init_8259A(int aeoi);
void send_IPI_self(int vector) __attribute__((regparm(3)));
void init_VISWS_APIC_irqs(void);
void setup_IO_APIC(void);
void disable_IO_APIC(void);
void print_IO_APIC(void);
int IO_APIC_get_PCI_irq_vector(int bus, int slot, int fn);
void send_IPI(int dest, int vector);
void setup_ioapic_dest(void);

extern unsigned long io_apic_irqs;

extern atomic_t irq_err_count;
extern atomic_t irq_mis_count;




static inline __attribute__((always_inline)) void hw_resend_irq(struct hw_interrupt_type *h, unsigned int i)
{
        if ((((i) >= 16) || ((1<<(i)) & io_apic_irqs)))
                send_IPI_self((irq_vector[i]));
}
# 94 "include/linux/irq.h" 2

extern int setup_irq(unsigned int irq, struct irqaction * new);


extern cpumask_t irq_affinity[224];


static inline __attribute__((always_inline)) void set_native_irq_info(int irq, cpumask_t mask)
{
        irq_affinity[irq] = mask;
}
# 114 "include/linux/irq.h"
extern cpumask_t pending_irq_cpumask[224];

static inline __attribute__((always_inline)) void set_pending_irq(unsigned int irq, cpumask_t mask)
{
        irq_desc_t *desc = irq_desc + irq;
        unsigned long flags;

        flags = _spin_lock_irqsave(&desc->lock);
        desc->move_irq = 1;
        pending_irq_cpumask[irq] = mask;
        _spin_unlock_irqrestore(&desc->lock, flags);
}

static inline __attribute__((always_inline)) void
move_native_irq(int irq)
{
        cpumask_t tmp;
        irq_desc_t *desc = irq_descp(irq);

        if (__builtin_expect(!!(!desc->move_irq), 1))
                return;

        desc->move_irq = 0;

        if (__builtin_expect(!!(__cpus_empty(&(pending_irq_cpumask[irq]), 8)), 1))
                return;

        if (!desc->handler->set_affinity)
                return;


        __cpus_and(&(tmp), &(pending_irq_cpumask[irq]), &(cpu_online_map), 8);
# 156 "include/linux/irq.h"
        if (__builtin_expect(!!(!__cpus_empty(&(tmp), 8)), 0)) {
                desc->handler->disable(irq);
                desc->handler->set_affinity(irq,tmp);
                desc->handler->enable(irq);
        }
        __cpus_clear(&(pending_irq_cpumask[irq]), 8);
}
# 182 "include/linux/irq.h"
static inline __attribute__((always_inline)) void move_irq(int irq)
{
        move_native_irq(irq);
}

static inline __attribute__((always_inline)) void set_irq_info(int irq, cpumask_t mask)
{
        set_native_irq_info(irq, mask);
}
# 212 "include/linux/irq.h"
extern int no_irq_affinity;
extern int noirqdebug_setup(char *str);

extern __attribute__((regparm(3))) int handle_IRQ_event(unsigned int irq, struct pt_regs *regs,
                                        struct irqaction *action);
extern __attribute__((regparm(3))) unsigned int __do_IRQ(unsigned int irq, struct pt_regs *regs);
extern void note_interrupt(unsigned int irq, irq_desc_t *desc,
                                        int action_ret, struct pt_regs *regs);
extern int can_request_irq(unsigned int irq, unsigned long irqflags);

extern void init_irq_proc(void);


extern hw_irq_controller no_irq_type;
# 7 "include/asm/hardirq.h" 2

typedef struct {
        unsigned int __softirq_pending;
        unsigned long idle_timestamp;
        unsigned int __nmi_count;
        unsigned int apic_timer_irqs;
} __attribute__((__aligned__((1 << (4))))) irq_cpustat_t;

extern __typeof__(irq_cpustat_t) per_cpu__irq_stat;
extern irq_cpustat_t irq_stat[];




void ack_bad_irq(unsigned int irq);
# 1 "include/linux/irq_cpustat.h" 1
# 23 "include/asm/hardirq.h" 2
# 8 "include/linux/hardirq.h" 2
# 85 "include/linux/hardirq.h"
extern void synchronize_irq(unsigned int irq);
# 94 "include/linux/hardirq.h"
static inline __attribute__((always_inline)) void account_user_vtime(struct task_struct *tsk)
{
}

static inline __attribute__((always_inline)) void account_system_vtime(struct task_struct *tsk)
{
}
# 109 "include/linux/hardirq.h"
extern void irq_exit(void);
# 12 "include/linux/interrupt.h" 2
# 30 "include/linux/interrupt.h"
typedef int irqreturn_t;





struct irqaction {
        irqreturn_t (*handler)(int, void *, struct pt_regs *);
        unsigned long flags;
        cpumask_t mask;
        const char *name;
        void *dev_id;
        struct irqaction *next;
        int irq;
        struct proc_dir_entry *dir;
};

extern irqreturn_t no_action(int cpl, void *dev_id, struct pt_regs *regs);
extern int request_irq(unsigned int,
                       irqreturn_t (*handler)(int, void *, struct pt_regs *),
                       unsigned long, const char *, void *);
extern void free_irq(unsigned int, void *);



extern void disable_irq_nosync(unsigned int irq);
extern void disable_irq(unsigned int irq);
extern void enable_irq(unsigned int irq);
# 100 "include/linux/interrupt.h"
extern void local_bh_enable(void);







enum
{
        HI_SOFTIRQ=0,
        TIMER_SOFTIRQ,
        NET_TX_SOFTIRQ,
        NET_RX_SOFTIRQ,
        SCSI_SOFTIRQ,
        TASKLET_SOFTIRQ
};





struct softirq_action
{
        void (*action)(struct softirq_action *);
        void *data;
};

 __attribute__((regparm(0))) void do_softirq(void);
extern void open_softirq(int nr, void (*action)(struct softirq_action*), void *data);
extern void softirq_init(void);

extern void raise_softirq_irqoff(unsigned int nr) __attribute__((regparm(3)));
extern void raise_softirq(unsigned int nr) __attribute__((regparm(3)));
# 156 "include/linux/interrupt.h"
struct tasklet_struct
{
        struct tasklet_struct *next;
        unsigned long state;
        atomic_t count;
        void (*func)(unsigned long);
        unsigned long data;
};
# 172 "include/linux/interrupt.h"
enum
{
        TASKLET_STATE_SCHED,
        TASKLET_STATE_RUN
};


static inline __attribute__((always_inline)) int tasklet_trylock(struct tasklet_struct *t)
{
        return !test_and_set_bit(TASKLET_STATE_RUN, &(t)->state);
}

static inline __attribute__((always_inline)) void tasklet_unlock(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        clear_bit(TASKLET_STATE_RUN, &(t)->state);
}

static inline __attribute__((always_inline)) void tasklet_unlock_wait(struct tasklet_struct *t)
{
        while ((__builtin_constant_p(TASKLET_STATE_RUN) ? constant_test_bit((TASKLET_STATE_RUN),(&(t)->state)) : variable_test_bit((TASKLET_STATE_RUN),(&(t)->state)))) { __asm__ __volatile__("": : :"memory"); }
}






extern void __tasklet_schedule(struct tasklet_struct *t) __attribute__((regparm(3)));

static inline __attribute__((always_inline)) void tasklet_schedule(struct tasklet_struct *t)
{
        if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
                __tasklet_schedule(t);
}

extern void __tasklet_hi_schedule(struct tasklet_struct *t) __attribute__((regparm(3)));

static inline __attribute__((always_inline)) void tasklet_hi_schedule(struct tasklet_struct *t)
{
        if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
                __tasklet_hi_schedule(t);
}


static inline __attribute__((always_inline)) void tasklet_disable_nosync(struct tasklet_struct *t)
{
        atomic_inc(&t->count);
        __asm__ __volatile__("": : :"memory");
}

static inline __attribute__((always_inline)) void tasklet_disable(struct tasklet_struct *t)
{
        tasklet_disable_nosync(t);
        tasklet_unlock_wait(t);
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "mfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
}

static inline __attribute__((always_inline)) void tasklet_enable(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        atomic_dec(&t->count);
}

static inline __attribute__((always_inline)) void tasklet_hi_enable(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        atomic_dec(&t->count);
}

extern void tasklet_kill(struct tasklet_struct *t);
extern void tasklet_kill_immediate(struct tasklet_struct *t, unsigned int cpu);
extern void tasklet_init(struct tasklet_struct *t,
                         void (*func)(unsigned long), unsigned long data);
# 289 "include/linux/interrupt.h"
extern unsigned long probe_irq_on(void);
extern int probe_irq_off(unsigned long);
extern unsigned int probe_irq_mask(unsigned long);
# 37 "include/linux/rcuref.h" 2
# 119 "include/linux/rcuref.h"
extern spinlock_t __rcuref_hash[];
# 142 "include/linux/rcuref.h"
static inline __attribute__((always_inline)) void rcuref_inc(atomic_t *rcuref)
{
        unsigned long flags;
        flags = _spin_lock_irqsave((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]));
        rcuref->counter += 1;
        _spin_unlock_irqrestore((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]), flags);
}
# 158 "include/linux/rcuref.h"
static inline __attribute__((always_inline)) void rcuref_dec(atomic_t *rcuref)
{
        unsigned long flags;
        flags = _spin_lock_irqsave((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]));
        rcuref->counter -= 1;
        _spin_unlock_irqrestore((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]), flags);
}
# 179 "include/linux/rcuref.h"
static inline __attribute__((always_inline)) int rcuref_dec_and_test(atomic_t *rcuref)
{
        unsigned long flags;
        flags = _spin_lock_irqsave((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]));
        rcuref->counter--;
        if (!rcuref->counter) {
                _spin_unlock_irqrestore((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]), flags);
                return 1;
        } else {
                _spin_unlock_irqrestore((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]), flags);
                return 0;
        }
}
# 203 "include/linux/rcuref.h"
static inline __attribute__((always_inline)) int rcuref_inc_lf(atomic_t *rcuref)
{
        int ret;
        unsigned long flags;
        flags = _spin_lock_irqsave((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]));
        if (rcuref->counter)
                ret = rcuref->counter++;
        else
                ret = 0;
        _spin_unlock_irqrestore((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]), flags);
        return ret;
}
# 13 "include/linux/fs.h" 2
# 33 "include/linux/fs.h"
struct files_stat_struct {
        int nr_files;
        int nr_free_files;
        int max_files;
};
extern struct files_stat_struct files_stat;

struct inodes_stat_t {
        int nr_inodes;
        int nr_unused;
        int dummy[5];
};
extern struct inodes_stat_t inodes_stat;

extern int leases_enable, lease_break_time;


extern int dir_notify_enable;
# 209 "include/linux/fs.h"
# 1 "include/linux/kdev_t.h" 1
# 21 "include/linux/kdev_t.h"
static inline __attribute__((always_inline)) int old_valid_dev(dev_t dev)
{
        return ((unsigned int) ((dev) >> 20)) < 256 && ((unsigned int) ((dev) & ((1U << 20) - 1))) < 256;
}

static inline __attribute__((always_inline)) u16 old_encode_dev(dev_t dev)
{
        return (((unsigned int) ((dev) >> 20)) << 8) | ((unsigned int) ((dev) & ((1U << 20) - 1)));
}

static inline __attribute__((always_inline)) dev_t old_decode_dev(u16 val)
{
        return ((((val >> 8) & 255) << 20) | (val & 255));
}

static inline __attribute__((always_inline)) int new_valid_dev(dev_t dev)
{
        return 1;
}

static inline __attribute__((always_inline)) u32 new_encode_dev(dev_t dev)
{
        unsigned major = ((unsigned int) ((dev) >> 20));
        unsigned minor = ((unsigned int) ((dev) & ((1U << 20) - 1)));
        return (minor & 0xff) | (major << 8) | ((minor & ~0xff) << 12);
}

static inline __attribute__((always_inline)) dev_t new_decode_dev(u32 dev)
{
        unsigned major = (dev & 0xfff00) >> 8;
        unsigned minor = (dev & 0xff) | ((dev >> 12) & 0xfff00);
        return (((major) << 20) | (minor));
}

static inline __attribute__((always_inline)) int huge_valid_dev(dev_t dev)
{
        return 1;
}

static inline __attribute__((always_inline)) u64 huge_encode_dev(dev_t dev)
{
        return new_encode_dev(dev);
}

static inline __attribute__((always_inline)) dev_t huge_decode_dev(u64 dev)
{
        return new_decode_dev(dev);
}

static inline __attribute__((always_inline)) int sysv_valid_dev(dev_t dev)
{
        return ((unsigned int) ((dev) >> 20)) < (1<<14) && ((unsigned int) ((dev) & ((1U << 20) - 1))) < (1<<18);
}

static inline __attribute__((always_inline)) u32 sysv_encode_dev(dev_t dev)
{
        return ((unsigned int) ((dev) & ((1U << 20) - 1))) | (((unsigned int) ((dev) >> 20)) << 18);
}

static inline __attribute__((always_inline)) unsigned sysv_major(u32 dev)
{
        return (dev >> 18) & 0x3fff;
}

static inline __attribute__((always_inline)) unsigned sysv_minor(u32 dev)
{
        return dev & 0x3ffff;
}
# 210 "include/linux/fs.h" 2
# 1 "include/linux/dcache.h" 1
# 10 "include/linux/dcache.h"
# 1 "include/linux/rcupdate.h" 1
# 50 "include/linux/rcupdate.h"
struct rcu_head {
        struct rcu_head *next;
        void (*func)(struct rcu_head *head);
};
# 64 "include/linux/rcupdate.h"
struct rcu_ctrlblk {
        long cur;
        long completed;
        int next_pending;
} __attribute__((__aligned__(1 << (7))));


static inline __attribute__((always_inline)) int rcu_batch_before(long a, long b)
{
        return (a - b) < 0;
}


static inline __attribute__((always_inline)) int rcu_batch_after(long a, long b)
{
        return (a - b) > 0;
}






struct rcu_data {

        long quiescbatch;
        int passed_quiesc;
        int qs_pending;


        long batch;
        struct rcu_head *nxtlist;
        struct rcu_head **nxttail;
        long count;
        struct rcu_head *curlist;
        struct rcu_head **curtail;
        struct rcu_head *donelist;
        struct rcu_head **donetail;
        int cpu;
};

extern __typeof__(struct rcu_data) per_cpu__rcu_data;
extern __typeof__(struct rcu_data) per_cpu__rcu_bh_data;
extern struct rcu_ctrlblk rcu_ctrlblk;
extern struct rcu_ctrlblk rcu_bh_ctrlblk;







static inline __attribute__((always_inline)) void rcu_qsctr_inc(int cpu)
{
        struct rcu_data *rdp = &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_data)); (typeof(&per_cpu__rcu_data)) (__ptr + (__per_cpu_offset[cpu])); }));
        rdp->passed_quiesc = 1;
}
static inline __attribute__((always_inline)) void rcu_bh_qsctr_inc(int cpu)
{
        struct rcu_data *rdp = &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_bh_data)); (typeof(&per_cpu__rcu_bh_data)) (__ptr + (__per_cpu_offset[cpu])); }));
        rdp->passed_quiesc = 1;
}

static inline __attribute__((always_inline)) int __rcu_pending(struct rcu_ctrlblk *rcp,
                                                struct rcu_data *rdp)
{



        if (rdp->curlist && !rcu_batch_before(rcp->completed, rdp->batch))
                return 1;


        if (!rdp->curlist && rdp->nxtlist)
                return 1;


        if (rdp->donelist)
                return 1;


        if (rdp->quiescbatch != rcp->cur || rdp->qs_pending)
                return 1;


        return 0;
}

static inline __attribute__((always_inline)) int rcu_pending(int cpu)
{
        return __rcu_pending(&rcu_ctrlblk, &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_data)); (typeof(&per_cpu__rcu_data)) (__ptr + (__per_cpu_offset[cpu])); }))) ||
                __rcu_pending(&rcu_bh_ctrlblk, &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_bh_data)); (typeof(&per_cpu__rcu_bh_data)) (__ptr + (__per_cpu_offset[cpu])); })));
}
# 275 "include/linux/rcupdate.h"
extern void rcu_init(void);
extern void rcu_check_callbacks(int cpu, int user);
extern void rcu_restart_cpu(int cpu);


extern void call_rcu(struct rcu_head *head, void (*func)(struct rcu_head *head)) __attribute__((regparm(3)));

extern void call_rcu_bh(struct rcu_head *head, void (*func)(struct rcu_head *head)) __attribute__((regparm(3)));

extern void synchronize_kernel(void);
extern void synchronize_rcu(void);
void synchronize_idle(void);
# 11 "include/linux/dcache.h" 2


struct nameidata;
struct vfsmount;
# 34 "include/linux/dcache.h"
struct qstr {
        unsigned int hash;
        unsigned int len;
        const unsigned char *name;
};

struct dentry_stat_t {
        int nr_dentry;
        int nr_unused;
        int age_limit;
        int want_pages;
        int dummy[2];
};
extern struct dentry_stat_t dentry_stat;






static inline __attribute__((always_inline)) unsigned long
partial_name_hash(unsigned long c, unsigned long prevhash)
{
        return (prevhash + (c << 4) + (c >> 4)) * 11;
}





static inline __attribute__((always_inline)) unsigned long end_name_hash(unsigned long hash)
{
        return (unsigned int) hash;
}


static inline __attribute__((always_inline)) unsigned int
full_name_hash(const unsigned char *name, unsigned int len)
{
        unsigned long hash = 0;
        while (len--)
                hash = partial_name_hash(*name++, hash);
        return end_name_hash(hash);
}

struct dcookie_struct;



struct dentry {
        atomic_t d_count;
        unsigned int d_flags;
        spinlock_t d_lock;
        struct inode *d_inode;





        struct hlist_node d_hash;
        struct dentry *d_parent;
        struct qstr d_name;

        struct list_head d_lru;
        struct list_head d_child;
        struct list_head d_subdirs;
        struct list_head d_alias;
        unsigned long d_time;
        struct dentry_operations *d_op;
        struct super_block *d_sb;
        void *d_fsdata;
        struct rcu_head d_rcu;
        struct dcookie_struct *d_cookie;
        int d_mounted;
        unsigned char d_iname[36];
};

struct dentry_operations {
        int (*d_revalidate)(struct dentry *, struct nameidata *);
        int (*d_hash) (struct dentry *, struct qstr *);
        int (*d_compare) (struct dentry *, struct qstr *, struct qstr *);
        int (*d_delete)(struct dentry *);
        void (*d_release)(struct dentry *);
        void (*d_iput)(struct dentry *, struct inode *);
};
# 159 "include/linux/dcache.h"
extern spinlock_t dcache_lock;
# 177 "include/linux/dcache.h"
static inline __attribute__((always_inline)) void __d_drop(struct dentry *dentry)
{
        if (!(dentry->d_flags & 0x0010)) {
                dentry->d_flags |= 0x0010;
                hlist_del_rcu(&dentry->d_hash);
        }
}

static inline __attribute__((always_inline)) void d_drop(struct dentry *dentry)
{
        _spin_lock(&dcache_lock);
        _spin_lock(&dentry->d_lock);
        __d_drop(dentry);
        _spin_unlock(&dentry->d_lock);
        _spin_unlock(&dcache_lock);
}

static inline __attribute__((always_inline)) int dname_external(struct dentry *dentry)
{
        return dentry->d_name.name != dentry->d_iname;
}




extern void d_instantiate(struct dentry *, struct inode *);
extern struct dentry * d_instantiate_unique(struct dentry *, struct inode *);
extern void d_delete(struct dentry *);


extern struct dentry * d_alloc(struct dentry *, const struct qstr *);
extern struct dentry * d_alloc_anon(struct inode *);
extern struct dentry * d_splice_alias(struct inode *, struct dentry *);
extern void shrink_dcache_sb(struct super_block *);
extern void shrink_dcache_parent(struct dentry *);
extern void shrink_dcache_anon(struct hlist_head *);
extern int d_invalidate(struct dentry *);


extern struct dentry * d_alloc_root(struct inode *);


extern void d_genocide(struct dentry *);

extern struct dentry *d_find_alias(struct inode *);
extern void d_prune_aliases(struct inode *);


extern int have_submounts(struct dentry *);




extern void d_rehash(struct dentry *);
# 241 "include/linux/dcache.h"
static inline __attribute__((always_inline)) void d_add(struct dentry *entry, struct inode *inode)
{
        d_instantiate(entry, inode);
        d_rehash(entry);
}
# 255 "include/linux/dcache.h"
static inline __attribute__((always_inline)) struct dentry *d_add_unique(struct dentry *entry, struct inode *inode)
{
        struct dentry *res;

        res = d_instantiate_unique(entry, inode);
        d_rehash(res != ((void *)0) ? res : entry);
        return res;
}


extern void d_move(struct dentry *, struct dentry *);


extern struct dentry * d_lookup(struct dentry *, struct qstr *);
extern struct dentry * __d_lookup(struct dentry *, struct qstr *);


extern int d_validate(struct dentry *, struct dentry *);

extern char * d_path(struct dentry *, struct vfsmount *, char *, int);
# 291 "include/linux/dcache.h"
static inline __attribute__((always_inline)) struct dentry *dget(struct dentry *dentry)
{
        if (dentry) {
                do { if (__builtin_expect(!!((!((&dentry->d_count)->counter))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (294), "i" ("include/linux/dcache.h")); } while(0);
                atomic_inc(&dentry->d_count);
        }
        return dentry;
}

extern struct dentry * dget_locked(struct dentry *);
# 309 "include/linux/dcache.h"
static inline __attribute__((always_inline)) int d_unhashed(struct dentry *dentry)
{
        return (dentry->d_flags & 0x0010);
}

static inline __attribute__((always_inline)) struct dentry *dget_parent(struct dentry *dentry)
{
        struct dentry *ret;

        _spin_lock(&dentry->d_lock);
        ret = dget(dentry->d_parent);
        _spin_unlock(&dentry->d_lock);
        return ret;
}

extern void dput(struct dentry *);

static inline __attribute__((always_inline)) int d_mountpoint(struct dentry *dentry)
{
        return dentry->d_mounted;
}

extern struct vfsmount *lookup_mnt(struct vfsmount *, struct dentry *);
extern struct dentry *lookup_create(struct nameidata *nd, int is_dir);

extern int sysctl_vfs_cache_pressure;
# 211 "include/linux/fs.h" 2




# 1 "include/linux/radix-tree.h" 1
# 25 "include/linux/radix-tree.h"
struct radix_tree_root {
        unsigned int height;
        unsigned int gfp_mask;
        struct radix_tree_node *rnode;
};
# 47 "include/linux/radix-tree.h"
int radix_tree_insert(struct radix_tree_root *, unsigned long, void *);
void *radix_tree_lookup(struct radix_tree_root *, unsigned long);
void *radix_tree_delete(struct radix_tree_root *, unsigned long);
unsigned int
radix_tree_gang_lookup(struct radix_tree_root *root, void **results,
                        unsigned long first_index, unsigned int max_items);
int radix_tree_preload(gfp_t gfp_mask);
void radix_tree_init(void);
void *radix_tree_tag_set(struct radix_tree_root *root,
                        unsigned long index, int tag);
void *radix_tree_tag_clear(struct radix_tree_root *root,
                        unsigned long index, int tag);
int radix_tree_tag_get(struct radix_tree_root *root,
                        unsigned long index, int tag);
unsigned int
radix_tree_gang_lookup_tag(struct radix_tree_root *root, void **results,
                unsigned long first_index, unsigned int max_items, int tag);
int radix_tree_tagged(struct radix_tree_root *root, int tag);

static inline __attribute__((always_inline)) void radix_tree_preload_end(void)
{
        do { } while (0);
}
# 216 "include/linux/fs.h" 2
# 224 "include/linux/fs.h"
struct iovec;
struct nameidata;
struct kiocb;
struct pipe_inode_info;
struct poll_table_struct;
struct kstatfs;
struct vm_area_struct;
struct vfsmount;


extern void update_atime (struct inode *);

extern void __attribute__ ((__section__ (".init.text"))) inode_init(unsigned long);
extern void __attribute__ ((__section__ (".init.text"))) inode_init_early(void);
extern void __attribute__ ((__section__ (".init.text"))) mnt_init(unsigned long);
extern void __attribute__ ((__section__ (".init.text"))) files_init(unsigned long);

struct buffer_head;
typedef int (get_block_t)(struct inode *inode, sector_t iblock,
                        struct buffer_head *bh_result, int create);
typedef int (get_blocks_t)(struct inode *inode, sector_t iblock,
                        unsigned long max_blocks,
                        struct buffer_head *bh_result, int create);
typedef void (dio_iodone_t)(struct kiocb *iocb, loff_t offset,
                        ssize_t bytes, void *private);
# 277 "include/linux/fs.h"
struct iattr {
        unsigned int ia_valid;
        umode_t ia_mode;
        uid_t ia_uid;
        gid_t ia_gid;
        loff_t ia_size;
        struct timespec ia_atime;
        struct timespec ia_mtime;
        struct timespec ia_ctime;
};




# 1 "include/linux/quota.h" 1
# 45 "include/linux/quota.h"
typedef __kernel_uid32_t qid_t;
typedef __u64 qsize_t;

extern spinlock_t dq_data_lock;
# 106 "include/linux/quota.h"
struct if_dqblk {
        __u64 dqb_bhardlimit;
        __u64 dqb_bsoftlimit;
        __u64 dqb_curspace;
        __u64 dqb_ihardlimit;
        __u64 dqb_isoftlimit;
        __u64 dqb_curinodes;
        __u64 dqb_btime;
        __u64 dqb_itime;
        __u32 dqb_valid;
};
# 127 "include/linux/quota.h"
struct if_dqinfo {
        __u64 dqi_bgrace;
        __u64 dqi_igrace;
        __u32 dqi_flags;
        __u32 dqi_valid;
};



# 1 "include/linux/dqblk_xfs.h" 1
# 52 "include/linux/dqblk_xfs.h"
typedef struct fs_disk_quota {
        __s8 d_version;
        __s8 d_flags;
        __u16 d_fieldmask;
        __u32 d_id;
        __u64 d_blk_hardlimit;
        __u64 d_blk_softlimit;
        __u64 d_ino_hardlimit;
        __u64 d_ino_softlimit;
        __u64 d_bcount;
        __u64 d_icount;
        __s32 d_itimer;

        __s32 d_btimer;
        __u16 d_iwarns;
        __u16 d_bwarns;
        __s32 d_padding2;
        __u64 d_rtb_hardlimit;
        __u64 d_rtb_softlimit;
        __u64 d_rtbcount;
        __s32 d_rtbtimer;
        __u16 d_rtbwarns;
        __s16 d_padding3;
        char d_padding4[8];
} fs_disk_quota_t;
# 139 "include/linux/dqblk_xfs.h"
typedef struct fs_qfilestat {
        __u64 qfs_ino;
        __u64 qfs_nblks;
        __u32 qfs_nextents;
} fs_qfilestat_t;

typedef struct fs_quota_stat {
        __s8 qs_version;
        __u16 qs_flags;
        __s8 qs_pad;
        fs_qfilestat_t qs_uquota;
        fs_qfilestat_t qs_gquota;
        __u32 qs_incoredqs;
        __s32 qs_btimelimit;
        __s32 qs_itimelimit;
        __s32 qs_rtbtimelimit;
        __u16 qs_bwarnlimit;
        __u16 qs_iwarnlimit;
} fs_quota_stat_t;
# 137 "include/linux/quota.h" 2
# 1 "include/linux/dqblk_v1.h" 1
# 21 "include/linux/dqblk_v1.h"
struct v1_mem_dqinfo {
};
# 138 "include/linux/quota.h" 2
# 1 "include/linux/dqblk_v2.h" 1
# 20 "include/linux/dqblk_v2.h"
struct v2_mem_dqinfo {
        unsigned int dqi_blocks;
        unsigned int dqi_free_blk;
        unsigned int dqi_free_entry;
};
# 139 "include/linux/quota.h" 2
# 150 "include/linux/quota.h"
struct mem_dqblk {
        __u32 dqb_bhardlimit;
        __u32 dqb_bsoftlimit;
        qsize_t dqb_curspace;
        __u32 dqb_ihardlimit;
        __u32 dqb_isoftlimit;
        __u32 dqb_curinodes;
        time_t dqb_btime;
        time_t dqb_itime;
};




struct quota_format_type;

struct mem_dqinfo {
        struct quota_format_type *dqi_format;
        struct list_head dqi_dirty_list;
        unsigned long dqi_flags;
        unsigned int dqi_bgrace;
        unsigned int dqi_igrace;
        union {
                struct v1_mem_dqinfo v1_i;
                struct v2_mem_dqinfo v2_i;
        } u;
};

struct super_block;





extern void mark_info_dirty(struct super_block *sb, int type);







struct dqstats {
        int lookups;
        int drops;
        int reads;
        int writes;
        int cache_hits;
        int allocated_dquots;
        int free_dquots;
        int syncs;
};

extern struct dqstats dqstats;
# 213 "include/linux/quota.h"
struct dquot {
        struct hlist_node dq_hash;
        struct list_head dq_inuse;
        struct list_head dq_free;
        struct list_head dq_dirty;
        struct semaphore dq_lock;
        atomic_t dq_count;
        wait_queue_head_t dq_wait_unused;
        struct super_block *dq_sb;
        unsigned int dq_id;
        loff_t dq_off;
        unsigned long dq_flags;
        short dq_type;
        struct mem_dqblk dq_dqb;
};







struct quota_format_ops {
        int (*check_quota_file)(struct super_block *sb, int type);
        int (*read_file_info)(struct super_block *sb, int type);
        int (*write_file_info)(struct super_block *sb, int type);
        int (*free_file_info)(struct super_block *sb, int type);
        int (*read_dqblk)(struct dquot *dquot);
        int (*commit_dqblk)(struct dquot *dquot);
        int (*release_dqblk)(struct dquot *dquot);
};


struct dquot_operations {
        int (*initialize) (struct inode *, int);
        int (*drop) (struct inode *);
        int (*alloc_space) (struct inode *, qsize_t, int);
        int (*alloc_inode) (const struct inode *, unsigned long);
        int (*free_space) (struct inode *, qsize_t);
        int (*free_inode) (const struct inode *, unsigned long);
        int (*transfer) (struct inode *, struct iattr *);
        int (*write_dquot) (struct dquot *);
        int (*acquire_dquot) (struct dquot *);
        int (*release_dquot) (struct dquot *);
        int (*mark_dirty) (struct dquot *);
        int (*write_info) (struct super_block *, int);
};


struct quotactl_ops {
        int (*quota_on)(struct super_block *, int, int, char *);
        int (*quota_off)(struct super_block *, int);
        int (*quota_sync)(struct super_block *, int);
        int (*get_info)(struct super_block *, int, struct if_dqinfo *);
        int (*set_info)(struct super_block *, int, struct if_dqinfo *);
        int (*get_dqblk)(struct super_block *, int, qid_t, struct if_dqblk *);
        int (*set_dqblk)(struct super_block *, int, qid_t, struct if_dqblk *);
        int (*get_xstate)(struct super_block *, struct fs_quota_stat *);
        int (*set_xstate)(struct super_block *, unsigned int, int);
        int (*get_xquota)(struct super_block *, int, qid_t, struct fs_disk_quota *);
        int (*set_xquota)(struct super_block *, int, qid_t, struct fs_disk_quota *);
};

struct quota_format_type {
        int qf_fmt_id;
        struct quota_format_ops *qf_ops;
        struct module *qf_owner;
        struct quota_format_type *qf_next;
};




struct quota_info {
        unsigned int flags;
        struct semaphore dqio_sem;
        struct semaphore dqonoff_sem;
        struct rw_semaphore dqptr_sem;
        struct inode *files[2];
        struct vfsmount *mnt[2];
        struct mem_dqinfo info[2];
        struct quota_format_ops *ops[2];
};


int mark_dquot_dirty(struct dquot *dquot);
# 308 "include/linux/quota.h"
int register_quota_format(struct quota_format_type *fmt);
void unregister_quota_format(struct quota_format_type *fmt);

struct quota_module_name {
        int qm_fmt_id;
        char *qm_mod_name;
};
# 292 "include/linux/fs.h" 2




struct page;
struct address_space;
struct writeback_control;

struct address_space_operations {
        int (*writepage)(struct page *page, struct writeback_control *wbc);
        int (*readpage)(struct file *, struct page *);
        int (*sync_page)(struct page *);


        int (*writepages)(struct address_space *, struct writeback_control *);


        int (*set_page_dirty)(struct page *page);

        int (*readpages)(struct file *filp, struct address_space *mapping,
                        struct list_head *pages, unsigned nr_pages);





        int (*prepare_write)(struct file *, struct page *, unsigned, unsigned);
        int (*commit_write)(struct file *, struct page *, unsigned, unsigned);

        sector_t (*bmap)(struct address_space *, sector_t);
        int (*invalidatepage) (struct page *, unsigned long);
        int (*releasepage) (struct page *, int);
        ssize_t (*direct_IO)(int, struct kiocb *, const struct iovec *iov,
                        loff_t offset, unsigned long nr_segs);
        struct page* (*get_xip_page)(struct address_space *, sector_t,
                        int);
};

struct backing_dev_info;
struct address_space {
        struct inode *host;
        struct radix_tree_root page_tree;
        rwlock_t tree_lock;
        unsigned int i_mmap_writable;
        struct prio_tree_root i_mmap;
        struct list_head i_mmap_nonlinear;
        spinlock_t i_mmap_lock;
        unsigned int truncate_count;
        unsigned long nrpages;
        unsigned long writeback_index;
        struct address_space_operations *a_ops;
        unsigned long flags;
        struct backing_dev_info *backing_dev_info;
        spinlock_t private_lock;
        struct list_head private_list;
        struct address_space *assoc_mapping;
} __attribute__((aligned(sizeof(long))));






struct block_device {
        dev_t bd_dev;
        struct inode * bd_inode;
        int bd_openers;
        struct semaphore bd_sem;
        struct semaphore bd_mount_sem;
        struct list_head bd_inodes;
        void * bd_holder;
        int bd_holders;
        struct block_device * bd_contains;
        unsigned bd_block_size;
        struct hd_struct * bd_part;

        unsigned bd_part_count;
        int bd_invalidated;
        struct gendisk * bd_disk;
        struct list_head bd_list;
        struct backing_dev_info *bd_inode_backing_dev_info;






        unsigned long bd_private;
};
# 389 "include/linux/fs.h"
int mapping_tagged(struct address_space *mapping, int tag);




static inline __attribute__((always_inline)) int mapping_mapped(struct address_space *mapping)
{
        return !prio_tree_empty(&mapping->i_mmap) ||
                !list_empty(&mapping->i_mmap_nonlinear);
}







static inline __attribute__((always_inline)) int mapping_writably_mapped(struct address_space *mapping)
{
        return mapping->i_mmap_writable != 0;
}
# 422 "include/linux/fs.h"
struct inode {
        struct hlist_node i_hash;
        struct list_head i_list;
        struct list_head i_sb_list;
        struct list_head i_dentry;
        unsigned long i_ino;
        atomic_t i_count;
        umode_t i_mode;
        unsigned int i_nlink;
        uid_t i_uid;
        gid_t i_gid;
        dev_t i_rdev;
        loff_t i_size;
        struct timespec i_atime;
        struct timespec i_mtime;
        struct timespec i_ctime;
        unsigned int i_blkbits;
        unsigned long i_blksize;
        unsigned long i_version;
        unsigned long i_blocks;
        unsigned short i_bytes;
        spinlock_t i_lock;
        struct semaphore i_sem;
        struct rw_semaphore i_alloc_sem;
        struct inode_operations *i_op;
        struct file_operations *i_fop;
        struct super_block *i_sb;
        struct file_lock *i_flock;
        struct address_space *i_mapping;
        struct address_space i_data;

        struct dquot *i_dquot[2];


        struct list_head i_devices;
        struct pipe_inode_info *i_pipe;
        struct block_device *i_bdev;
        struct cdev *i_cdev;
        int i_cindex;

        __u32 i_generation;


        unsigned long i_dnotify_mask;
        struct dnotify_struct *i_dnotify;



        struct list_head inotify_watches;
        struct semaphore inotify_sem;


        unsigned long i_state;
        unsigned long dirtied_when;

        unsigned int i_flags;

        atomic_t i_writecount;
        void *i_security;
        union {
                void *generic_ip;
        } u;

        seqcount_t i_size_seqcount;

};
# 499 "include/linux/fs.h"
static inline __attribute__((always_inline)) loff_t i_size_read(struct inode *inode)
{

        loff_t i_size;
        unsigned int seq;

        do {
                seq = read_seqcount_begin(&inode->i_size_seqcount);
                i_size = inode->i_size;
        } while (read_seqcount_retry(&inode->i_size_seqcount, seq));
        return i_size;
# 520 "include/linux/fs.h"
}


static inline __attribute__((always_inline)) void i_size_write(struct inode *inode, loff_t i_size)
{

        write_seqcount_begin(&inode->i_size_seqcount);
        inode->i_size = i_size;
        write_seqcount_end(&inode->i_size_seqcount);







}

static inline __attribute__((always_inline)) unsigned iminor(struct inode *inode)
{
        return ((unsigned int) ((inode->i_rdev) & ((1U << 20) - 1)));
}

static inline __attribute__((always_inline)) unsigned imajor(struct inode *inode)
{
        return ((unsigned int) ((inode->i_rdev) >> 20));
}

extern struct block_device *I_BDEV(struct inode *inode);

struct fown_struct {
        rwlock_t lock;
        int pid;
        uid_t uid, euid;
        void *security;
        int signum;
};




struct file_ra_state {
        unsigned long start;
        unsigned long size;
        unsigned long flags;
        unsigned long cache_hit;
        unsigned long prev_page;
        unsigned long ahead_start;
        unsigned long ahead_size;
        unsigned long ra_pages;
        unsigned long mmap_hit;
        unsigned long mmap_miss;
};



struct file {
        struct list_head f_list;
        struct dentry *f_dentry;
        struct vfsmount *f_vfsmnt;
        struct file_operations *f_op;
        atomic_t f_count;
        unsigned int f_flags;
        mode_t f_mode;
        loff_t f_pos;
        struct fown_struct f_owner;
        unsigned int f_uid, f_gid;
        struct file_ra_state f_ra;

        unsigned long f_version;
        void *f_security;


        void *private_data;



        struct list_head f_ep_links;
        spinlock_t f_ep_lock;

        struct address_space *f_mapping;
        struct rcu_head f_rcuhead;
};
extern spinlock_t files_lock;
# 634 "include/linux/fs.h"
typedef struct files_struct *fl_owner_t;

struct file_lock_operations {
        void (*fl_insert)(struct file_lock *);
        void (*fl_remove)(struct file_lock *);
        void (*fl_copy_lock)(struct file_lock *, struct file_lock *);
        void (*fl_release_private)(struct file_lock *);
};

struct lock_manager_operations {
        int (*fl_compare_owner)(struct file_lock *, struct file_lock *);
        void (*fl_notify)(struct file_lock *);
        void (*fl_copy_lock)(struct file_lock *, struct file_lock *);
        void (*fl_release_private)(struct file_lock *);
        void (*fl_break)(struct file_lock *);
        int (*fl_mylease)(struct file_lock *, struct file_lock *);
        int (*fl_change)(struct file_lock **, int);
};


# 1 "include/linux/nfs_fs_i.h" 1





# 1 "include/linux/nfs.h" 1
# 10 "include/linux/nfs.h"
# 1 "include/linux/sunrpc/msg_prot.h" 1
# 15 "include/linux/sunrpc/msg_prot.h"
typedef u32 rpc_authflavor_t;

enum rpc_auth_flavors {
        RPC_AUTH_NULL = 0,
        RPC_AUTH_UNIX = 1,
        RPC_AUTH_SHORT = 2,
        RPC_AUTH_DES = 3,
        RPC_AUTH_KRB = 4,
        RPC_AUTH_GSS = 6,
        RPC_AUTH_MAXFLAVOR = 8,

        RPC_AUTH_GSS_KRB5 = 390003,
        RPC_AUTH_GSS_KRB5I = 390004,
        RPC_AUTH_GSS_KRB5P = 390005,
        RPC_AUTH_GSS_LKEY = 390006,
        RPC_AUTH_GSS_LKEYI = 390007,
        RPC_AUTH_GSS_LKEYP = 390008,
        RPC_AUTH_GSS_SPKM = 390009,
        RPC_AUTH_GSS_SPKMI = 390010,
        RPC_AUTH_GSS_SPKMP = 390011,
};

enum rpc_msg_type {
        RPC_CALL = 0,
        RPC_REPLY = 1
};

enum rpc_reply_stat {
        RPC_MSG_ACCEPTED = 0,
        RPC_MSG_DENIED = 1
};

enum rpc_accept_stat {
        RPC_SUCCESS = 0,
        RPC_PROG_UNAVAIL = 1,
        RPC_PROG_MISMATCH = 2,
        RPC_PROC_UNAVAIL = 3,
        RPC_GARBAGE_ARGS = 4,
        RPC_SYSTEM_ERR = 5
};

enum rpc_reject_stat {
        RPC_MISMATCH = 0,
        RPC_AUTH_ERROR = 1
};

enum rpc_auth_stat {
        RPC_AUTH_OK = 0,
        RPC_AUTH_BADCRED = 1,
        RPC_AUTH_REJECTEDCRED = 2,
        RPC_AUTH_BADVERF = 3,
        RPC_AUTH_REJECTEDVERF = 4,
        RPC_AUTH_TOOWEAK = 5,

        RPCSEC_GSS_CREDPROBLEM = 13,
        RPCSEC_GSS_CTXPROBLEM = 14
};
# 11 "include/linux/nfs.h" 2
# 42 "include/linux/nfs.h"
 enum nfs_stat {
        NFS_OK = 0,
        NFSERR_PERM = 1,
        NFSERR_NOENT = 2,
        NFSERR_IO = 5,
        NFSERR_NXIO = 6,
        NFSERR_EAGAIN = 11,
        NFSERR_ACCES = 13,
        NFSERR_EXIST = 17,
        NFSERR_XDEV = 18,
        NFSERR_NODEV = 19,
        NFSERR_NOTDIR = 20,
        NFSERR_ISDIR = 21,
        NFSERR_INVAL = 22,
        NFSERR_FBIG = 27,
        NFSERR_NOSPC = 28,
        NFSERR_ROFS = 30,
        NFSERR_MLINK = 31,
        NFSERR_OPNOTSUPP = 45,
        NFSERR_NAMETOOLONG = 63,
        NFSERR_NOTEMPTY = 66,
        NFSERR_DQUOT = 69,
        NFSERR_STALE = 70,
        NFSERR_REMOTE = 71,
        NFSERR_WFLUSH = 99,
        NFSERR_BADHANDLE = 10001,
        NFSERR_NOT_SYNC = 10002,
        NFSERR_BAD_COOKIE = 10003,
        NFSERR_NOTSUPP = 10004,
        NFSERR_TOOSMALL = 10005,
        NFSERR_SERVERFAULT = 10006,
        NFSERR_BADTYPE = 10007,
        NFSERR_JUKEBOX = 10008,
        NFSERR_SAME = 10009,
        NFSERR_DENIED = 10010,
        NFSERR_EXPIRED = 10011,
        NFSERR_LOCKED = 10012,
        NFSERR_GRACE = 10013,
        NFSERR_FHEXPIRED = 10014,
        NFSERR_SHARE_DENIED = 10015,
        NFSERR_WRONGSEC = 10016,
        NFSERR_CLID_INUSE = 10017,
        NFSERR_RESOURCE = 10018,
        NFSERR_MOVED = 10019,
        NFSERR_NOFILEHANDLE = 10020,
        NFSERR_MINOR_VERS_MISMATCH = 10021,
        NFSERR_STALE_CLIENTID = 10022,
        NFSERR_STALE_STATEID = 10023,
        NFSERR_OLD_STATEID = 10024,
        NFSERR_BAD_STATEID = 10025,
        NFSERR_BAD_SEQID = 10026,
        NFSERR_NOT_SAME = 10027,
        NFSERR_LOCK_RANGE = 10028,
        NFSERR_SYMLINK = 10029,
        NFSERR_RESTOREFH = 10030,
        NFSERR_LEASE_MOVED = 10031,
        NFSERR_ATTRNOTSUPP = 10032,
        NFSERR_NO_GRACE = 10033,
        NFSERR_RECLAIM_BAD = 10034,
        NFSERR_RECLAIM_CONFLICT = 10035,
        NFSERR_BAD_XDR = 10036,
        NFSERR_LOCKS_HELD = 10037,
        NFSERR_OPENMODE = 10038,
        NFSERR_BADOWNER = 10039,
        NFSERR_BADCHAR = 10040,
        NFSERR_BADNAME = 10041,
        NFSERR_BAD_RANGE = 10042,
        NFSERR_LOCK_NOTSUPP = 10043,
        NFSERR_OP_ILLEGAL = 10044,
        NFSERR_DEADLOCK = 10045,
        NFSERR_FILE_OPEN = 10046,
        NFSERR_ADMIN_REVOKED = 10047,
        NFSERR_CB_PATH_DOWN = 10048,
        NFSERR_REPLAY_ME = 10049
};



enum nfs_ftype {
        NFNON = 0,
        NFREG = 1,
        NFDIR = 2,
        NFBLK = 3,
        NFCHR = 4,
        NFLNK = 5,
        NFSOCK = 6,
        NFBAD = 7,
        NFFIFO = 8
};






struct nfs_fh {
        unsigned short size;
        unsigned char data[128];
};





static inline __attribute__((always_inline)) int nfs_compare_fh(const struct nfs_fh *a, const struct nfs_fh *b)
{
        return a->size != b->size || __builtin_memcmp(a->data, b->data, a->size) != 0;
}

static inline __attribute__((always_inline)) void nfs_copy_fh(struct nfs_fh *target, const struct nfs_fh *source)
{
        target->size = source->size;
        (__builtin_constant_p(source->size) ? __constant_memcpy((target->data),(source->data),(source->size)) : __memcpy((target->data),(source->data),(source->size)));
}
# 165 "include/linux/nfs.h"
enum nfs3_stable_how {
        NFS_UNSTABLE = 0,
        NFS_DATA_SYNC = 1,
        NFS_FILE_SYNC = 2
};
# 7 "include/linux/nfs_fs_i.h" 2

struct nlm_lockowner;




struct nfs_lock_info {
        u32 state;
        u32 flags;
        struct nlm_lockowner *owner;
};

struct nfs4_lock_state;
struct nfs4_lock_info {
        struct nfs4_lock_state *owner;
};
# 655 "include/linux/fs.h" 2

struct file_lock {
        struct file_lock *fl_next;
        struct list_head fl_link;
        struct list_head fl_block;
        fl_owner_t fl_owner;
        unsigned int fl_pid;
        wait_queue_head_t fl_wait;
        struct file *fl_file;
        unsigned char fl_flags;
        unsigned char fl_type;
        loff_t fl_start;
        loff_t fl_end;

        struct fasync_struct * fl_fasync;
        unsigned long fl_break_time;

        struct file_lock_operations *fl_ops;
        struct lock_manager_operations *fl_lmops;
        union {
                struct nfs_lock_info nfs_fl;
                struct nfs4_lock_info nfs4_fl;
        } fl_u;
};
# 687 "include/linux/fs.h"
extern struct list_head file_lock_list;

# 1 "include/linux/fcntl.h" 1



# 1 "include/asm/fcntl.h" 1
# 1 "include/asm-generic/fcntl.h" 1
# 115 "include/asm-generic/fcntl.h"
struct flock {
        short l_type;
        short l_whence;
        off_t l_start;
        off_t l_len;
        pid_t l_pid;
       
};
# 138 "include/asm-generic/fcntl.h"
struct flock64 {
        short l_type;
        short l_whence;
        loff_t l_start;
        loff_t l_len;
        pid_t l_pid;
       
};
# 2 "include/asm/fcntl.h" 2
# 5 "include/linux/fcntl.h" 2
# 690 "include/linux/fs.h" 2

extern int fcntl_getlk(struct file *, struct flock *);
extern int fcntl_setlk(unsigned int, struct file *, unsigned int,
                        struct flock *);


extern int fcntl_getlk64(struct file *, struct flock64 *);
extern int fcntl_setlk64(unsigned int, struct file *, unsigned int,
                        struct flock64 *);


extern void send_sigio(struct fown_struct *fown, int fd, int band);
extern int fcntl_setlease(unsigned int fd, struct file *filp, long arg);
extern int fcntl_getlease(struct file *filp);


extern void locks_init_lock(struct file_lock *);
extern void locks_copy_lock(struct file_lock *, struct file_lock *);
extern void locks_remove_posix(struct file *, fl_owner_t);
extern void locks_remove_flock(struct file *);
extern struct file_lock *posix_test_lock(struct file *, struct file_lock *);
extern int posix_lock_file(struct file *, struct file_lock *);
extern int posix_lock_file_wait(struct file *, struct file_lock *);
extern void posix_block_lock(struct file_lock *, struct file_lock *);
extern void posix_unblock_lock(struct file *, struct file_lock *);
extern int posix_locks_deadlock(struct file_lock *, struct file_lock *);
extern int flock_lock_file_wait(struct file *filp, struct file_lock *fl);
extern int __break_lease(struct inode *inode, unsigned int flags);
extern void lease_get_mtime(struct inode *, struct timespec *time);
extern int setlease(struct file *, long, struct file_lock **);
extern int lease_modify(struct file_lock **, int);
extern int lock_may_read(struct inode *, loff_t start, unsigned long count);
extern int lock_may_write(struct inode *, loff_t start, unsigned long count);
extern void steal_locks(fl_owner_t from);

struct fasync_struct {
        int magic;
        int fa_fd;
        struct fasync_struct *fa_next;
        struct file *fa_file;
};




extern int fasync_helper(int, struct file *, int, struct fasync_struct **);

extern void kill_fasync(struct fasync_struct **, int, int);

extern void __kill_fasync(struct fasync_struct *, int, int);

extern int f_setown(struct file *filp, unsigned long arg, int force);
extern void f_delown(struct file *filp);
extern int send_sigurg(struct fown_struct *fown);
# 753 "include/linux/fs.h"
extern struct list_head super_blocks;
extern spinlock_t sb_lock;



struct super_block {
        struct list_head s_list;
        dev_t s_dev;
        unsigned long s_blocksize;
        unsigned long s_old_blocksize;
        unsigned char s_blocksize_bits;
        unsigned char s_dirt;
        unsigned long long s_maxbytes;
        struct file_system_type *s_type;
        struct super_operations *s_op;
        struct dquot_operations *dq_op;
        struct quotactl_ops *s_qcop;
        struct export_operations *s_export_op;
        unsigned long s_flags;
        unsigned long s_magic;
        struct dentry *s_root;
        struct rw_semaphore s_umount;
        struct semaphore s_lock;
        int s_count;
        int s_syncing;
        int s_need_sync_fs;
        atomic_t s_active;
        void *s_security;
        struct xattr_handler **s_xattr;

        struct list_head s_inodes;
        struct list_head s_dirty;
        struct list_head s_io;
        struct hlist_head s_anon;
        struct list_head s_files;

        struct block_device *s_bdev;
        struct list_head s_instances;
        struct quota_info s_dquot;

        int s_frozen;
        wait_queue_head_t s_wait_unfrozen;

        char s_id[32];

        void *s_fs_info;





        struct semaphore s_vfs_rename_sem;



        u32 s_time_gran;
};

extern struct timespec current_fs_time(struct super_block *sb);




enum {
        SB_UNFROZEN = 0,
        SB_FREEZE_WRITE = 1,
        SB_FREEZE_TRANS = 2,
};




static inline __attribute__((always_inline)) void get_fs_excl(void)
{
        atomic_inc(&get_current()->fs_excl);
}

static inline __attribute__((always_inline)) void put_fs_excl(void)
{
        atomic_dec(&get_current()->fs_excl);
}

static inline __attribute__((always_inline)) int has_fs_excl(void)
{
        return ((&get_current()->fs_excl)->counter);
}





static inline __attribute__((always_inline)) void lock_super(struct super_block * sb)
{
        get_fs_excl();
        down(&sb->s_lock);
}

static inline __attribute__((always_inline)) void unlock_super(struct super_block * sb)
{
        put_fs_excl();
        up(&sb->s_lock);
}




extern int vfs_create(struct inode *, struct dentry *, int, struct nameidata *);
extern int vfs_mkdir(struct inode *, struct dentry *, int);
extern int vfs_mknod(struct inode *, struct dentry *, int, dev_t);
extern int vfs_symlink(struct inode *, struct dentry *, const char *, int);
extern int vfs_link(struct dentry *, struct inode *, struct dentry *);
extern int vfs_rmdir(struct inode *, struct dentry *);
extern int vfs_unlink(struct inode *, struct dentry *);
extern int vfs_rename(struct inode *, struct dentry *, struct inode *, struct dentry *);




extern void dentry_unhash(struct dentry *dentry);
# 892 "include/linux/fs.h"
int generic_osync_inode(struct inode *, struct address_space *, int);







typedef int (*filldir_t)(void *, const char *, int, loff_t, ino_t, unsigned);

struct block_device_operations {
        int (*open) (struct inode *, struct file *);
        int (*release) (struct inode *, struct file *);
        int (*ioctl) (struct inode *, struct file *, unsigned, unsigned long);
        long (*unlocked_ioctl) (struct file *, unsigned, unsigned long);
        long (*compat_ioctl) (struct file *, unsigned, unsigned long);
        int (*direct_access) (struct block_device *, sector_t, unsigned long *);
        int (*media_changed) (struct gendisk *);
        int (*revalidate_disk) (struct gendisk *);
        struct module *owner;
};
# 923 "include/linux/fs.h"
typedef struct {
        size_t written;
        size_t count;
        union {
                char * buf;
                void *data;
        } arg;
        int error;
} read_descriptor_t;

typedef int (*read_actor_t)(read_descriptor_t *, struct page *, unsigned long, unsigned long);
# 946 "include/linux/fs.h"
struct file_operations {
        struct module *owner;
        loff_t (*llseek) (struct file *, loff_t, int);
        ssize_t (*read) (struct file *, char *, size_t, loff_t *);
        ssize_t (*aio_read) (struct kiocb *, char *, size_t, loff_t);
        ssize_t (*write) (struct file *, const char *, size_t, loff_t *);
        ssize_t (*aio_write) (struct kiocb *, const char *, size_t, loff_t);
        int (*readdir) (struct file *, void *, filldir_t);
        unsigned int (*poll) (struct file *, struct poll_table_struct *);
        int (*ioctl) (struct inode *, struct file *, unsigned int, unsigned long);
        long (*unlocked_ioctl) (struct file *, unsigned int, unsigned long);
        long (*compat_ioctl) (struct file *, unsigned int, unsigned long);
        int (*mmap) (struct file *, struct vm_area_struct *);
        int (*open) (struct inode *, struct file *);
        int (*flush) (struct file *);
        int (*release) (struct inode *, struct file *);
        int (*fsync) (struct file *, struct dentry *, int datasync);
        int (*aio_fsync) (struct kiocb *, int datasync);
        int (*fasync) (int, struct file *, int);
        int (*lock) (struct file *, int, struct file_lock *);
        ssize_t (*readv) (struct file *, const struct iovec *, unsigned long, loff_t *);
        ssize_t (*writev) (struct file *, const struct iovec *, unsigned long, loff_t *);
        ssize_t (*sendfile) (struct file *, loff_t *, size_t, read_actor_t, void *);
        ssize_t (*sendpage) (struct file *, struct page *, int, size_t, loff_t *, int);
        unsigned long (*get_unmapped_area)(struct file *, unsigned long, unsigned long, unsigned long, unsigned long);
        int (*check_flags)(int);
        int (*dir_notify)(struct file *filp, unsigned long arg);
        int (*flock) (struct file *, int, struct file_lock *);
};

struct inode_operations {
        int (*create) (struct inode *,struct dentry *,int, struct nameidata *);
        struct dentry * (*lookup) (struct inode *,struct dentry *, struct nameidata *);
        int (*link) (struct dentry *,struct inode *,struct dentry *);
        int (*unlink) (struct inode *,struct dentry *);
        int (*symlink) (struct inode *,struct dentry *,const char *);
        int (*mkdir) (struct inode *,struct dentry *,int);
        int (*rmdir) (struct inode *,struct dentry *);
        int (*mknod) (struct inode *,struct dentry *,int,dev_t);
        int (*rename) (struct inode *, struct dentry *,
                        struct inode *, struct dentry *);
        int (*readlink) (struct dentry *, char *,int);
        void * (*follow_link) (struct dentry *, struct nameidata *);
        void (*put_link) (struct dentry *, struct nameidata *, void *);
        void (*truncate) (struct inode *);
        int (*permission) (struct inode *, int, struct nameidata *);
        int (*setattr) (struct dentry *, struct iattr *);
        int (*getattr) (struct vfsmount *mnt, struct dentry *, struct kstat *);
        int (*setxattr) (struct dentry *, const char *,const void *,size_t,int);
        ssize_t (*getxattr) (struct dentry *, const char *, void *, size_t);
        ssize_t (*listxattr) (struct dentry *, char *, size_t);
        int (*removexattr) (struct dentry *, const char *);
};

struct seq_file;

extern ssize_t vfs_read(struct file *, char *, size_t, loff_t *);
extern ssize_t vfs_write(struct file *, const char *, size_t, loff_t *);
extern ssize_t vfs_readv(struct file *, const struct iovec *,
                unsigned long, loff_t *);
extern ssize_t vfs_writev(struct file *, const struct iovec *,
                unsigned long, loff_t *);





struct super_operations {
        struct inode *(*alloc_inode)(struct super_block *sb);
        void (*destroy_inode)(struct inode *);

        void (*read_inode) (struct inode *);

        void (*dirty_inode) (struct inode *);
        int (*write_inode) (struct inode *, int);
        void (*put_inode) (struct inode *);
        void (*drop_inode) (struct inode *);
        void (*delete_inode) (struct inode *);
        void (*put_super) (struct super_block *);
        void (*write_super) (struct super_block *);
        int (*sync_fs)(struct super_block *sb, int wait);
        void (*write_super_lockfs) (struct super_block *);
        void (*unlockfs) (struct super_block *);
        int (*statfs) (struct super_block *, struct kstatfs *);
        int (*remount_fs) (struct super_block *, int *, char *);
        void (*clear_inode) (struct inode *);
        void (*umount_begin) (struct super_block *);

        int (*show_options)(struct seq_file *, struct vfsmount *);

        ssize_t (*quota_read)(struct super_block *, int, char *, size_t, loff_t);
        ssize_t (*quota_write)(struct super_block *, int, const char *, size_t, loff_t);
};
# 1053 "include/linux/fs.h"
extern void __mark_inode_dirty(struct inode *, int);
static inline __attribute__((always_inline)) void mark_inode_dirty(struct inode *inode)
{
        __mark_inode_dirty(inode, (1 | 2 | 4));
}

static inline __attribute__((always_inline)) void mark_inode_dirty_sync(struct inode *inode)
{
        __mark_inode_dirty(inode, 1);
}

static inline __attribute__((always_inline)) void touch_atime(struct vfsmount *mnt, struct dentry *dentry)
{

        update_atime(dentry->d_inode);
}

static inline __attribute__((always_inline)) void file_accessed(struct file *file)
{
        if (!(file->f_flags & 01000000))
                touch_atime(file->f_vfsmnt, file->f_dentry);
}

int sync_inode(struct inode *inode, struct writeback_control *wbc);
# 1162 "include/linux/fs.h"
struct export_operations {
        struct dentry *(*decode_fh)(struct super_block *sb, __u32 *fh, int fh_len, int fh_type,
                         int (*acceptable)(void *context, struct dentry *de),
                         void *context);
        int (*encode_fh)(struct dentry *de, __u32 *fh, int *max_len,
                         int connectable);


        int (*get_name)(struct dentry *parent, char *name,
                        struct dentry *child);
        struct dentry * (*get_parent)(struct dentry *child);
        struct dentry * (*get_dentry)(struct super_block *sb, void *inump);


        struct dentry * (*find_exported_dentry)(
                struct super_block *sb, void *obj, void *parent,
                int (*acceptable)(void *context, struct dentry *de),
                void *context);


};

extern struct dentry *
find_exported_dentry(struct super_block *sb, void *obj, void *parent,
                     int (*acceptable)(void *context, struct dentry *de),
                     void *context);

struct file_system_type {
        const char *name;
        int fs_flags;
        struct super_block *(*get_sb) (struct file_system_type *, int,
                                       const char *, void *);
        void (*kill_sb) (struct super_block *);
        struct module *owner;
        struct file_system_type * next;
        struct list_head fs_supers;
};

struct super_block *get_sb_bdev(struct file_system_type *fs_type,
        int flags, const char *dev_name, void *data,
        int (*fill_super)(struct super_block *, void *, int));
struct super_block *get_sb_single(struct file_system_type *fs_type,
        int flags, void *data,
        int (*fill_super)(struct super_block *, void *, int));
struct super_block *get_sb_nodev(struct file_system_type *fs_type,
        int flags, void *data,
        int (*fill_super)(struct super_block *, void *, int));
void generic_shutdown_super(struct super_block *sb);
void kill_block_super(struct super_block *sb);
void kill_anon_super(struct super_block *sb);
void kill_litter_super(struct super_block *sb);
void deactivate_super(struct super_block *sb);
int set_anon_super(struct super_block *s, void *data);
struct super_block *sget(struct file_system_type *type,
                        int (*test)(struct super_block *,void *),
                        int (*set)(struct super_block *,void *),
                        void *data);
struct super_block *get_sb_pseudo(struct file_system_type *, char *,
                        struct super_operations *ops, unsigned long);
int __put_super(struct super_block *sb);
int __put_super_and_need_restart(struct super_block *sb);
void unnamed_dev_init(void);







extern int register_filesystem(struct file_system_type *);
extern int unregister_filesystem(struct file_system_type *);
extern struct vfsmount *kern_mount(struct file_system_type *);
extern int may_umount_tree(struct vfsmount *);
extern int may_umount(struct vfsmount *);
extern long do_mount(char *, char *, char *, unsigned long, void *);

extern int vfs_statfs(struct super_block *, struct kstatfs *);




extern int locks_mandatory_locked(struct inode *);
extern int locks_mandatory_area(int, struct inode *, struct file *, loff_t, size_t);
# 1253 "include/linux/fs.h"
static inline __attribute__((always_inline)) int locks_verify_locked(struct inode *inode)
{
        if ((((inode)->i_sb->s_flags & (64)) && ((inode)->i_mode & (0002000 | 00010)) == 0002000))
                return locks_mandatory_locked(inode);
        return 0;
}

extern int rw_verify_area(int, struct file *, loff_t *, size_t);

static inline __attribute__((always_inline)) int locks_verify_truncate(struct inode *inode,
                                    struct file *filp,
                                    loff_t size)
{
        if (inode->i_flock && (((inode)->i_sb->s_flags & (64)) && ((inode)->i_mode & (0002000 | 00010)) == 0002000))
                return locks_mandatory_area(
                        2, inode, filp,
                        size < inode->i_size ? size : inode->i_size,
                        (size < inode->i_size ? inode->i_size - size
                         : size - inode->i_size)
                );
        return 0;
}

static inline __attribute__((always_inline)) int break_lease(struct inode *inode, unsigned int mode)
{
        if (inode->i_flock)
                return __break_lease(inode, mode);
        return 0;
}



extern int do_truncate(struct dentry *, loff_t start);
extern long do_sys_open(const char *filename, int flags, int mode);
extern struct file *filp_open(const char *, int, int);
extern struct file * dentry_open(struct dentry *, struct vfsmount *, int);
extern int filp_close(struct file *, fl_owner_t id);
extern char * getname(const char *);


extern void __attribute__ ((__section__ (".init.text"))) vfs_caches_init_early(void);
extern void __attribute__ ((__section__ (".init.text"))) vfs_caches_init(unsigned long);
# 1304 "include/linux/fs.h"
extern int register_blkdev(unsigned int, const char *);
extern int unregister_blkdev(unsigned int, const char *);
extern struct block_device *bdget(dev_t);
extern void bd_set_size(struct block_device *, loff_t size);
extern void bd_forget(struct inode *inode);
extern void bdput(struct block_device *);
extern struct block_device *open_by_devnum(dev_t, unsigned);
extern struct file_operations def_blk_fops;
extern struct address_space_operations def_blk_aops;
extern struct file_operations def_chr_fops;
extern struct file_operations bad_sock_fops;
extern struct file_operations def_fifo_fops;
extern int ioctl_by_bdev(struct block_device *, unsigned, unsigned long);
extern int blkdev_ioctl(struct inode *, struct file *, unsigned, unsigned long);
extern long compat_blkdev_ioctl(struct file *, unsigned, unsigned long);
extern int blkdev_get(struct block_device *, mode_t, unsigned);
extern int blkdev_put(struct block_device *);
extern int bd_claim(struct block_device *, void *);
extern void bd_release(struct block_device *);


extern int alloc_chrdev_region(dev_t *, unsigned, unsigned, const char *);
extern int register_chrdev_region(dev_t, unsigned, const char *);
extern int register_chrdev(unsigned int, const char *,
                           struct file_operations *);
extern int unregister_chrdev(unsigned int, const char *);
extern void unregister_chrdev_region(dev_t, unsigned);
extern int chrdev_open(struct inode *, struct file *);



extern const char *__bdevname(dev_t, char *buffer);
extern const char *bdevname(struct block_device *bdev, char *buffer);
extern struct block_device *lookup_bdev(const char *);
extern struct block_device *open_bdev_excl(const char *, int, void *);
extern void close_bdev_excl(struct block_device *);

extern void init_special_inode(struct inode *, umode_t, dev_t);


extern void make_bad_inode(struct inode *);
extern int is_bad_inode(struct inode *);

extern struct file_operations read_fifo_fops;
extern struct file_operations write_fifo_fops;
extern struct file_operations rdwr_fifo_fops;
extern struct file_operations read_pipe_fops;
extern struct file_operations write_pipe_fops;
extern struct file_operations rdwr_pipe_fops;

extern int fs_may_remount_ro(struct super_block *);
# 1366 "include/linux/fs.h"
extern int check_disk_change(struct block_device *);
extern int invalidate_inodes(struct super_block *);
extern int __invalidate_device(struct block_device *);
extern int invalidate_partition(struct gendisk *, int);
unsigned long invalidate_mapping_pages(struct address_space *mapping,
                                        unsigned long start, unsigned long end);
unsigned long invalidate_inode_pages(struct address_space *mapping);
static inline __attribute__((always_inline)) void invalidate_remote_inode(struct inode *inode)
{
        if ((((inode->i_mode) & 00170000) == 0100000) || (((inode->i_mode) & 00170000) == 0040000) ||
            (((inode->i_mode) & 00170000) == 0120000))
                invalidate_inode_pages(inode->i_mapping);
}
extern int invalidate_inode_pages2(struct address_space *mapping);
extern int invalidate_inode_pages2_range(struct address_space *mapping,
                                         unsigned long start, unsigned long end);
extern int write_inode_now(struct inode *, int);
extern int filemap_fdatawrite(struct address_space *);
extern int filemap_flush(struct address_space *);
extern int filemap_fdatawait(struct address_space *);
extern int filemap_write_and_wait(struct address_space *mapping);
extern int filemap_write_and_wait_range(struct address_space *mapping,
                                        loff_t lstart, loff_t lend);
extern void sync_supers(void);
extern void sync_filesystems(int wait);
extern void emergency_sync(void);
extern void emergency_remount(void);
extern int do_remount_sb(struct super_block *sb, int flags,
                         void *data, int force);
extern sector_t bmap(struct inode *, sector_t);
extern int notify_change(struct dentry *, struct iattr *);
extern int permission(struct inode *, int, struct nameidata *);
extern int generic_permission(struct inode *, int,
                int (*check_acl)(struct inode *, int));

extern int get_write_access(struct inode *);
extern int deny_write_access(struct file *);
static inline __attribute__((always_inline)) void put_write_access(struct inode * inode)
{
        atomic_dec(&inode->i_writecount);
}
static inline __attribute__((always_inline)) void allow_write_access(struct file *file)
{
        if (file)
                atomic_inc(&file->f_dentry->d_inode->i_writecount);
}
extern int do_pipe(int *);

extern int open_namei(const char *, int, int, struct nameidata *);
extern int may_open(struct nameidata *, int, int);

extern int kernel_read(struct file *, unsigned long, char *, unsigned long);
extern struct file * open_exec(const char *);


extern int is_subdir(struct dentry *, struct dentry *);
extern ino_t find_inode_number(struct dentry *, struct qstr *);

# 1 "include/linux/err.h" 1
# 18 "include/linux/err.h"
static inline __attribute__((always_inline)) void *ERR_PTR(long error)
{
        return (void *) error;
}

static inline __attribute__((always_inline)) long PTR_ERR(const void *ptr)
{
        return (long) ptr;
}

static inline __attribute__((always_inline)) long IS_ERR(const void *ptr)
{
        return __builtin_expect(!!(((unsigned long)ptr) > (unsigned long)-1000L), 0);
}
# 1425 "include/linux/fs.h" 2


extern loff_t default_llseek(struct file *file, loff_t offset, int origin);

extern loff_t vfs_llseek(struct file *file, loff_t offset, int origin);

extern void inode_init_once(struct inode *);
extern void iput(struct inode *);
extern struct inode * igrab(struct inode *);
extern ino_t iunique(struct super_block *, ino_t);
extern int inode_needs_sync(struct inode *inode);
extern void generic_delete_inode(struct inode *inode);
extern void generic_drop_inode(struct inode *inode);

extern struct inode *ilookup5_nowait(struct super_block *sb,
                unsigned long hashval, int (*test)(struct inode *, void *),
                void *data);
extern struct inode *ilookup5(struct super_block *sb, unsigned long hashval,
                int (*test)(struct inode *, void *), void *data);
extern struct inode *ilookup(struct super_block *sb, unsigned long ino);

extern struct inode * iget5_locked(struct super_block *, unsigned long, int (*test)(struct inode *, void *), int (*set)(struct inode *, void *), void *);
extern struct inode * iget_locked(struct super_block *, unsigned long);
extern void unlock_new_inode(struct inode *);

static inline __attribute__((always_inline)) struct inode *iget(struct super_block *sb, unsigned long ino)
{
        struct inode *inode = iget_locked(sb, ino);

        if (inode && (inode->i_state & 64)) {
                sb->s_op->read_inode(inode);
                unlock_new_inode(inode);
        }

        return inode;
}

extern void __iget(struct inode * inode);
extern void clear_inode(struct inode *);
extern void destroy_inode(struct inode *);
extern struct inode *new_inode(struct super_block *);
extern int remove_suid(struct dentry *);
extern void remove_dquot_ref(struct super_block *, int, struct list_head *);
extern struct semaphore iprune_sem;

extern void __insert_inode_hash(struct inode *, unsigned long hashval);
extern void remove_inode_hash(struct inode *);
static inline __attribute__((always_inline)) void insert_inode_hash(struct inode *inode) {
        __insert_inode_hash(inode, inode->i_ino);
}

extern struct file * get_empty_filp(void);
extern void file_move(struct file *f, struct list_head *list);
extern void file_kill(struct file *f);
struct bio;
extern void submit_bio(int, struct bio *);
extern int bdev_read_only(struct block_device *);
extern int set_blocksize(struct block_device *, int);
extern int sb_set_blocksize(struct super_block *, int);
extern int sb_min_blocksize(struct super_block *, int);

extern int generic_file_mmap(struct file *, struct vm_area_struct *);
extern int generic_file_readonly_mmap(struct file *, struct vm_area_struct *);
extern int file_read_actor(read_descriptor_t * desc, struct page *page, unsigned long offset, unsigned long size);
extern int file_send_actor(read_descriptor_t * desc, struct page *page, unsigned long offset, unsigned long size);
extern ssize_t generic_file_read(struct file *, char *, size_t, loff_t *);
int generic_write_checks(struct file *file, loff_t *pos, size_t *count, int isblk);
extern ssize_t generic_file_write(struct file *, const char *, size_t, loff_t *);
extern ssize_t generic_file_aio_read(struct kiocb *, char *, size_t, loff_t);
extern ssize_t __generic_file_aio_read(struct kiocb *, const struct iovec *, unsigned long, loff_t *);
extern ssize_t generic_file_aio_write(struct kiocb *, const char *, size_t, loff_t);
extern ssize_t generic_file_aio_write_nolock(struct kiocb *, const struct iovec *,
                unsigned long, loff_t *);
extern ssize_t generic_file_direct_write(struct kiocb *, const struct iovec *,
                unsigned long *, loff_t, loff_t *, size_t, size_t);
extern ssize_t generic_file_buffered_write(struct kiocb *, const struct iovec *,
                unsigned long, loff_t, loff_t *, size_t, ssize_t);
extern ssize_t do_sync_read(struct file *filp, char *buf, size_t len, loff_t *ppos);
extern ssize_t do_sync_write(struct file *filp, const char *buf, size_t len, loff_t *ppos);
ssize_t generic_file_write_nolock(struct file *file, const struct iovec *iov,
                                unsigned long nr_segs, loff_t *ppos);
extern ssize_t generic_file_sendfile(struct file *, loff_t *, size_t, read_actor_t, void *);
extern void do_generic_mapping_read(struct address_space *mapping,
                                    struct file_ra_state *, struct file *,
                                    loff_t *, read_descriptor_t *, read_actor_t);
extern void
file_ra_state_init(struct file_ra_state *ra, struct address_space *mapping);
extern ssize_t generic_file_readv(struct file *filp, const struct iovec *iov,
        unsigned long nr_segs, loff_t *ppos);
ssize_t generic_file_writev(struct file *filp, const struct iovec *iov,
                        unsigned long nr_segs, loff_t *ppos);
extern loff_t no_llseek(struct file *file, loff_t offset, int origin);
extern loff_t generic_file_llseek(struct file *file, loff_t offset, int origin);
extern loff_t remote_llseek(struct file *file, loff_t offset, int origin);
extern int generic_file_open(struct inode * inode, struct file * filp);
extern int nonseekable_open(struct inode * inode, struct file * filp);
# 1533 "include/linux/fs.h"
static inline __attribute__((always_inline)) int xip_truncate_page(struct address_space *mapping, loff_t from)
{
        return 0;
}


static inline __attribute__((always_inline)) void do_generic_file_read(struct file * filp, loff_t *ppos,
                                        read_descriptor_t * desc,
                                        read_actor_t actor)
{
        do_generic_mapping_read(filp->f_mapping,
                                &filp->f_ra,
                                filp,
                                ppos,
                                desc,
                                actor);
}

ssize_t __blockdev_direct_IO(int rw, struct kiocb *iocb, struct inode *inode,
        struct block_device *bdev, const struct iovec *iov, loff_t offset,
        unsigned long nr_segs, get_blocks_t get_blocks, dio_iodone_t end_io,
        int lock_type);

enum {
        DIO_LOCKING = 1,
        DIO_NO_LOCKING,
        DIO_OWN_LOCKING,
};

static inline __attribute__((always_inline)) ssize_t blockdev_direct_IO(int rw, struct kiocb *iocb,
        struct inode *inode, struct block_device *bdev, const struct iovec *iov,
        loff_t offset, unsigned long nr_segs, get_blocks_t get_blocks,
        dio_iodone_t end_io)
{
        return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
                                nr_segs, get_blocks, end_io, DIO_LOCKING);
}

static inline __attribute__((always_inline)) ssize_t blockdev_direct_IO_no_locking(int rw, struct kiocb *iocb,
        struct inode *inode, struct block_device *bdev, const struct iovec *iov,
        loff_t offset, unsigned long nr_segs, get_blocks_t get_blocks,
        dio_iodone_t end_io)
{
        return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
                                nr_segs, get_blocks, end_io, DIO_NO_LOCKING);
}

static inline __attribute__((always_inline)) ssize_t blockdev_direct_IO_own_locking(int rw, struct kiocb *iocb,
        struct inode *inode, struct block_device *bdev, const struct iovec *iov,
        loff_t offset, unsigned long nr_segs, get_blocks_t get_blocks,
        dio_iodone_t end_io)
{
        return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
                                nr_segs, get_blocks, end_io, DIO_OWN_LOCKING);
}

extern struct file_operations generic_ro_fops;



extern int vfs_readlink(struct dentry *, char *, int, const char *);
extern int vfs_follow_link(struct nameidata *, const char *);
extern int page_readlink(struct dentry *, char *, int);
extern void *page_follow_link_light(struct dentry *, struct nameidata *);
extern void page_put_link(struct dentry *, struct nameidata *, void *);
extern int page_symlink(struct inode *inode, const char *symname, int len);
extern struct inode_operations page_symlink_inode_operations;
extern int generic_readlink(struct dentry *, char *, int);
extern void generic_fillattr(struct inode *, struct kstat *);
extern int vfs_getattr(struct vfsmount *, struct dentry *, struct kstat *);
void inode_add_bytes(struct inode *inode, loff_t bytes);
void inode_sub_bytes(struct inode *inode, loff_t bytes);
loff_t inode_get_bytes(struct inode *inode);
void inode_set_bytes(struct inode *inode, loff_t bytes);

extern int vfs_readdir(struct file *, filldir_t, void *);

extern int vfs_stat(char *, struct kstat *);
extern int vfs_lstat(char *, struct kstat *);
extern int vfs_fstat(unsigned int, struct kstat *);

extern int vfs_ioctl(struct file *, unsigned int, unsigned int, unsigned long);

extern struct file_system_type *get_fs_type(const char *name);
extern struct super_block *get_super(struct block_device *);
extern struct super_block *user_get_super(dev_t);
extern void drop_super(struct super_block *sb);

extern int dcache_dir_open(struct inode *, struct file *);
extern int dcache_dir_close(struct inode *, struct file *);
extern loff_t dcache_dir_lseek(struct file *, loff_t, int);
extern int dcache_readdir(struct file *, void *, filldir_t);
extern int simple_getattr(struct vfsmount *, struct dentry *, struct kstat *);
extern int simple_statfs(struct super_block *, struct kstatfs *);
extern int simple_link(struct dentry *, struct inode *, struct dentry *);
extern int simple_unlink(struct inode *, struct dentry *);
extern int simple_rmdir(struct inode *, struct dentry *);
extern int simple_rename(struct inode *, struct dentry *, struct inode *, struct dentry *);
extern int simple_sync_file(struct file *, struct dentry *, int);
extern int simple_empty(struct dentry *);
extern int simple_readpage(struct file *file, struct page *page);
extern int simple_prepare_write(struct file *file, struct page *page,
                        unsigned offset, unsigned to);
extern int simple_commit_write(struct file *file, struct page *page,
                                unsigned offset, unsigned to);

extern struct dentry *simple_lookup(struct inode *, struct dentry *, struct nameidata *);
extern ssize_t generic_read_dir(struct file *, char *, size_t, loff_t *);
extern struct file_operations simple_dir_operations;
extern struct inode_operations simple_dir_inode_operations;
struct tree_descr { char *name; struct file_operations *ops; int mode; };
struct dentry *d_alloc_name(struct dentry *, const char *);
extern int simple_fill_super(struct super_block *, int, struct tree_descr *);
extern int simple_pin_fs(char *name, struct vfsmount **mount, int *count);
extern void simple_release_fs(struct vfsmount **mount, int *count);

extern ssize_t simple_read_from_buffer(void *, size_t, loff_t *, const void *, size_t);

extern int inode_change_ok(struct inode *, struct iattr *);
extern int inode_setattr(struct inode *, struct iattr *);

extern void inode_update_time(struct inode *inode, int ctime_too);

static inline __attribute__((always_inline)) ino_t parent_ino(struct dentry *dentry)
{
        ino_t res;

        _spin_lock(&dentry->d_lock);
        res = dentry->d_parent->d_inode->i_ino;
        _spin_unlock(&dentry->d_lock);
        return res;
}


extern int unshare_files(void);







struct simple_transaction_argresp {
        ssize_t size;
        char data[0];
};



char *simple_transaction_get(struct file *file, const char *buf,
                                size_t size);
ssize_t simple_transaction_read(struct file *file, char *buf,
                                size_t size, loff_t *pos);
int simple_transaction_release(struct inode *inode, struct file *file);

static inline __attribute__((always_inline)) void simple_transaction_set(struct file *file, size_t n)
{
        struct simple_transaction_argresp *ar = file->private_data;

        do { if (__builtin_expect(!!((n > ((1UL << 12) - sizeof(struct simple_transaction_argresp)))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (1692), "i" ("include/linux/fs.h")); } while(0);





        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "mfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        ar->size = n;
}
# 1732 "include/linux/fs.h"
static inline __attribute__((always_inline)) void __attribute__((format(printf, 1, 2)))
__simple_attr_check_format(const char *fmt, ...)
{

}

int simple_attr_open(struct inode *inode, struct file *file,
                     u64 (*get)(void *), void (*set)(void *, u64),
                     const char *fmt);
int simple_attr_close(struct inode *inode, struct file *file);
ssize_t simple_attr_read(struct file *file, char *buf,
                         size_t len, loff_t *ppos);
ssize_t simple_attr_write(struct file *file, const char *buf,
                          size_t len, loff_t *ppos);
# 1759 "include/linux/fs.h"
static inline __attribute__((always_inline)) char *alloc_secdata(void)
{
        return (char *)1;
}

static inline __attribute__((always_inline)) void free_secdata(void *secdata)
{ }
# 16 "include/linux/mm.h" 2

struct mempolicy;
struct anon_vma;


extern unsigned long max_mapnr;


extern unsigned long num_physpages;
extern void * high_memory;
extern unsigned long vmalloc_earlyreserve;
extern int page_cluster;


extern int sysctl_legacy_va_layout;





# 1 "include/asm/pgtable.h" 1
# 33 "include/asm/pgtable.h"
extern unsigned long empty_zero_page[1024];
extern pgd_t swapper_pg_dir[1024];
extern kmem_cache_t *pgd_cache;
extern kmem_cache_t *pmd_cache;
extern spinlock_t pgd_lock;
extern struct page *pgd_list;

void pmd_ctor(void *, kmem_cache_t *, unsigned long);
void pgd_ctor(void *, kmem_cache_t *, unsigned long);
void pgd_dtor(void *, kmem_cache_t *, unsigned long);
void pgtable_cache_init(void);
void paging_init(void);







# 1 "include/asm/pgtable-3level-defs.h" 1
# 53 "include/asm/pgtable.h" 2
# 157 "include/asm/pgtable.h"
extern unsigned long long __PAGE_KERNEL, __PAGE_KERNEL_EXEC;
# 201 "include/asm/pgtable.h"
extern unsigned long pg0[];
# 219 "include/asm/pgtable.h"
static inline __attribute__((always_inline)) int pte_user(pte_t pte) { return (pte).pte_low & 0x004; }
static inline __attribute__((always_inline)) int pte_read(pte_t pte) { return (pte).pte_low & 0x004; }
static inline __attribute__((always_inline)) int pte_dirty(pte_t pte) { return (pte).pte_low & 0x040; }
static inline __attribute__((always_inline)) int pte_young(pte_t pte) { return (pte).pte_low & 0x020; }
static inline __attribute__((always_inline)) int pte_write(pte_t pte) { return (pte).pte_low & 0x002; }
static inline __attribute__((always_inline)) int pte_huge(pte_t pte) { return ((pte).pte_low & (0x080 | 0x001)) == (0x080 | 0x001); }




static inline __attribute__((always_inline)) int pte_file(pte_t pte) { return (pte).pte_low & 0x040; }

static inline __attribute__((always_inline)) pte_t pte_rdprotect(pte_t pte) { (pte).pte_low &= ~0x004; return pte; }
static inline __attribute__((always_inline)) pte_t pte_exprotect(pte_t pte) { (pte).pte_low &= ~0x004; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkclean(pte_t pte) { (pte).pte_low &= ~0x040; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkold(pte_t pte) { (pte).pte_low &= ~0x020; return pte; }
static inline __attribute__((always_inline)) pte_t pte_wrprotect(pte_t pte) { (pte).pte_low &= ~0x002; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkread(pte_t pte) { (pte).pte_low |= 0x004; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkexec(pte_t pte) { (pte).pte_low |= 0x004; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkdirty(pte_t pte) { (pte).pte_low |= 0x040; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkyoung(pte_t pte) { (pte).pte_low |= 0x020; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkwrite(pte_t pte) { (pte).pte_low |= 0x002; return pte; }
static inline __attribute__((always_inline)) pte_t pte_mkhuge(pte_t pte) { (pte).pte_low |= (0x080 | 0x001); return pte; }


# 1 "include/asm/pgtable-3level.h" 1



# 1 "include/asm-generic/pgtable-nopud.h" 1
# 13 "include/asm-generic/pgtable-nopud.h"
typedef struct { pgd_t pgd; } pud_t;
# 25 "include/asm-generic/pgtable-nopud.h"
static inline __attribute__((always_inline)) int pgd_none(pgd_t pgd) { return 0; }
static inline __attribute__((always_inline)) int pgd_bad(pgd_t pgd) { return 0; }
static inline __attribute__((always_inline)) int pgd_present(pgd_t pgd) { return 1; }
static inline __attribute__((always_inline)) void pgd_clear(pgd_t *pgd) { }
# 38 "include/asm-generic/pgtable-nopud.h"
static inline __attribute__((always_inline)) pud_t * pud_offset(pgd_t * pgd, unsigned long address)
{
        return (pud_t *)pgd;
}
# 5 "include/asm/pgtable-3level.h" 2
# 27 "include/asm/pgtable-3level.h"
static inline __attribute__((always_inline)) int pte_x(pte_t pte)
{
        return !(((pte).pte_low | ((unsigned long long)(pte).pte_high << 32)) & (1ULL<<63));
}




static inline __attribute__((always_inline)) int pte_exec(pte_t pte)
{
        return pte_user(pte) && pte_x(pte);
}



static inline __attribute__((always_inline)) int pte_exec_kernel(pte_t pte)
{
        return pte_x(pte);
}







static inline __attribute__((always_inline)) void set_pte(pte_t *ptep, pte_t pte)
{
        ptep->pte_high = pte.pte_high;
        __asm__ __volatile__ ("": : :"memory");
        ptep->pte_low = pte.pte_low;
}
# 75 "include/asm/pgtable-3level.h"
static inline __attribute__((always_inline)) void pud_clear (pud_t * pud) { }
# 93 "include/asm/pgtable-3level.h"
static inline __attribute__((always_inline)) pte_t ptep_get_and_clear(struct mm_struct *mm, unsigned long addr, pte_t *ptep)
{
        pte_t res;


        res.pte_low = ((__typeof__(*(&ptep->pte_low)))__xchg((unsigned long)(0),(&ptep->pte_low),sizeof(*(&ptep->pte_low))));
        res.pte_high = ptep->pte_high;
        ptep->pte_high = 0;

        return res;
}

static inline __attribute__((always_inline)) int pte_same(pte_t a, pte_t b)
{
        return a.pte_low == b.pte_low && a.pte_high == b.pte_high;
}



static inline __attribute__((always_inline)) int pte_none(pte_t pte)
{
        return !pte.pte_low && !pte.pte_high;
}

static inline __attribute__((always_inline)) unsigned long pte_pfn(pte_t pte)
{
        return (pte.pte_low >> 12) |
                (pte.pte_high << (32 - 12));
}

extern unsigned long long __supported_pte_mask;

static inline __attribute__((always_inline)) pte_t pfn_pte(unsigned long page_nr, pgprot_t pgprot)
{
        pte_t pte;

        pte.pte_high = (page_nr >> (32 - 12)) | (((pgprot).pgprot) >> 32);

        pte.pte_high &= (__supported_pte_mask >> 32);
        pte.pte_low = ((page_nr << 12) | ((pgprot).pgprot)) & __supported_pte_mask;

        return pte;
}

static inline __attribute__((always_inline)) pmd_t pfn_pmd(unsigned long page_nr, pgprot_t pgprot)
{
        return ((pmd_t) { ((((unsigned long long)page_nr << 12) | ((pgprot).pgprot)) & __supported_pte_mask) } );

}
# 245 "include/asm/pgtable.h" 2




static inline __attribute__((always_inline)) int ptep_test_and_clear_dirty(struct vm_area_struct *vma, unsigned long addr, pte_t *ptep)
{
        if (!pte_dirty(*ptep))
                return 0;
        return test_and_clear_bit(6, &ptep->pte_low);
}

static inline __attribute__((always_inline)) int ptep_test_and_clear_young(struct vm_area_struct *vma, unsigned long addr, pte_t *ptep)
{
        if (!pte_young(*ptep))
                return 0;
        return test_and_clear_bit(5, &ptep->pte_low);
}

static inline __attribute__((always_inline)) pte_t ptep_get_and_clear_full(struct mm_struct *mm, unsigned long addr, pte_t *ptep, int full)
{
        pte_t pte;
        if (full) {
                pte = *ptep;
                *ptep = ((pte_t) { (0) } );
        } else {
                pte = ptep_get_and_clear(mm, addr, ptep);
        }
        return pte;
}

static inline __attribute__((always_inline)) void ptep_set_wrprotect(struct mm_struct *mm, unsigned long addr, pte_t *ptep)
{
        clear_bit(1, &ptep->pte_low);
}
# 290 "include/asm/pgtable.h"
static inline __attribute__((always_inline)) void clone_pgd_range(pgd_t *dst, pgd_t *src, int count)
{
       (__builtin_constant_p(count * sizeof(pgd_t)) ? __constant_memcpy((dst),(src),(count * sizeof(pgd_t))) : __memcpy((dst),(src),(count * sizeof(pgd_t))));
}
# 309 "include/asm/pgtable.h"
static inline __attribute__((always_inline)) pte_t pte_modify(pte_t pte, pgprot_t newprot)
{
        pte.pte_low &= ((~((1UL << 12)-1)) | 0x020 | 0x040);
        pte.pte_low |= ((newprot).pgprot);





        pte.pte_high &= ~(1 << (63 - 32));
        pte.pte_high |= (((newprot).pgprot) >> 32) & (__supported_pte_mask >> 32);


        return pte;
}
# 377 "include/asm/pgtable.h"
extern pte_t *lookup_address(unsigned long address);
# 386 "include/asm/pgtable.h"
 extern int set_kernel_exec(unsigned long vaddr, int enable);




extern void noexec_setup(const char *str);
# 447 "include/asm/pgtable.h"
# 1 "include/asm-generic/pgtable.h" 1
# 205 "include/asm-generic/pgtable.h"
void pgd_clear_bad(pgd_t *);
void pud_clear_bad(pud_t *);
void pmd_clear_bad(pmd_t *);

static inline __attribute__((always_inline)) int pgd_none_or_clear_bad(pgd_t *pgd)
{
        if (pgd_none(*pgd))
                return 1;
        if (__builtin_expect(!!(pgd_bad(*pgd)), 0)) {
                pgd_clear_bad(pgd);
                return 1;
        }
        return 0;
}

static inline __attribute__((always_inline)) int pud_none_or_clear_bad(pud_t *pud)
{
        if (0)
                return 1;
        if (__builtin_expect(!!(0), 0)) {
                pud_clear_bad(pud);
                return 1;
        }
        return 0;
}

static inline __attribute__((always_inline)) int pmd_none_or_clear_bad(pmd_t *pmd)
{
        if ((!((*pmd).pmd)))
                return 1;
        if (__builtin_expect(!!(((((*pmd).pmd) & (~(~((1UL << 12)-1)) & ~0x004)) != (0x001 | 0x002 | 0x020 | 0x040))), 0)) {
                pmd_clear_bad(pmd);
                return 1;
        }
        return 0;
}
# 448 "include/asm/pgtable.h" 2
# 37 "include/linux/mm.h" 2
# 57 "include/linux/mm.h"
struct vm_area_struct {
        struct mm_struct * vm_mm;
        unsigned long vm_start;
        unsigned long vm_end;



        struct vm_area_struct *vm_next;

        pgprot_t vm_page_prot;
        unsigned long vm_flags;

        struct rb_node vm_rb;







        union {
                struct {
                        struct list_head list;
                        void *parent;
                        struct vm_area_struct *head;
                } vm_set;

                struct raw_prio_tree_node prio_tree_node;
        } shared;







        struct list_head anon_vma_node;
        struct anon_vma *anon_vma;


        struct vm_operations_struct * vm_ops;


        unsigned long vm_pgoff;

        struct file * vm_file;
        void * vm_private_data;
        unsigned long vm_truncate_count;







};






struct vm_list_struct {
        struct vm_list_struct *next;
        struct vm_area_struct *vma;
};
# 186 "include/linux/mm.h"
extern pgprot_t protection_map[16];







struct vm_operations_struct {
        void (*open)(struct vm_area_struct * area);
        void (*close)(struct vm_area_struct * area);
        struct page * (*nopage)(struct vm_area_struct * area, unsigned long address, int *type);
        int (*populate)(struct vm_area_struct * area, unsigned long address, unsigned long len, pgprot_t prot, unsigned long pgoff, int nonblock);





};

struct mmu_gather;
struct inode;




typedef unsigned long page_flags_t;
# 221 "include/linux/mm.h"
struct page {
        page_flags_t flags;

        atomic_t _count;
        atomic_t _mapcount;



        unsigned long private;






        struct address_space *mapping;






        unsigned long index;
        struct list_head lru;
# 261 "include/linux/mm.h"
};





# 1 "include/linux/page-flags.h" 1
# 83 "include/linux/page-flags.h"
struct page_state {
        unsigned long nr_dirty;
        unsigned long nr_writeback;
        unsigned long nr_unstable;
        unsigned long nr_page_table_pages;
        unsigned long nr_mapped;
        unsigned long nr_slab;






        unsigned long pgpgin;
        unsigned long pgpgout;
        unsigned long pswpin;
        unsigned long pswpout;
        unsigned long pgalloc_high;

        unsigned long pgalloc_normal;
        unsigned long pgalloc_dma;
        unsigned long pgfree;
        unsigned long pgactivate;
        unsigned long pgdeactivate;

        unsigned long pgfault;
        unsigned long pgmajfault;
        unsigned long pgrefill_high;
        unsigned long pgrefill_normal;
        unsigned long pgrefill_dma;

        unsigned long pgsteal_high;
        unsigned long pgsteal_normal;
        unsigned long pgsteal_dma;
        unsigned long pgscan_kswapd_high;
        unsigned long pgscan_kswapd_normal;

        unsigned long pgscan_kswapd_dma;
        unsigned long pgscan_direct_high;
        unsigned long pgscan_direct_normal;
        unsigned long pgscan_direct_dma;
        unsigned long pginodesteal;

        unsigned long slabs_scanned;
        unsigned long kswapd_steal;
        unsigned long kswapd_inodesteal;
        unsigned long pageoutrun;
        unsigned long allocstall;

        unsigned long pgrotated;
        unsigned long nr_bounce;
};

extern void get_page_state(struct page_state *ret);
extern void get_page_state_node(struct page_state *ret, int node);
extern void get_full_page_state(struct page_state *ret);
extern unsigned long __read_page_state(unsigned long offset);
extern void __mod_page_state(unsigned long offset, unsigned long delta);
# 310 "include/linux/page-flags.h"
struct page;

int test_clear_page_dirty(struct page *page);
int test_clear_page_writeback(struct page *page);
int test_set_page_writeback(struct page *page);

static inline __attribute__((always_inline)) void clear_page_dirty(struct page *page)
{
        test_clear_page_dirty(page);
}

static inline __attribute__((always_inline)) void set_page_writeback(struct page *page)
{
        test_set_page_writeback(page);
}
# 268 "include/linux/mm.h" 2
# 310 "include/linux/mm.h"
extern void __page_cache_release(struct page *) __attribute__((regparm(3)));



static inline __attribute__((always_inline)) int page_count(struct page *p)
{
        if ((__builtin_constant_p(14) ? constant_test_bit((14),(&(p)->flags)) : variable_test_bit((14),(&(p)->flags))))
                p = (struct page *)p->private;
        return ((&(p)->_count)->counter) + 1;
}

static inline __attribute__((always_inline)) void get_page(struct page *page)
{
        if (__builtin_expect(!!((__builtin_constant_p(14) ? constant_test_bit((14),(&(page)->flags)) : variable_test_bit((14),(&(page)->flags)))), 0))
                page = (struct page *)page->private;
        atomic_inc(&page->_count);
}

void put_page(struct page *page);
# 470 "include/linux/mm.h"
static inline __attribute__((always_inline)) unsigned long page_zonenum(struct page *page)
{
        return (page->flags >> (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0))) & ((1UL << 2) - 1);
}

struct zone;
extern struct zone *zone_table[];

static inline __attribute__((always_inline)) struct zone *page_zone(struct page *page)
{
        return zone_table[(page->flags >> (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0))) &
                        ((1UL << (0 + 2)) - 1)];
}

static inline __attribute__((always_inline)) unsigned long page_to_nid(struct page *page)
{
        if ((0 > 0 || 0 == 0))
                return (page->flags >> ((((sizeof(page_flags_t)*8) - 0) - 0) * (0 != 0))) & ((1UL << 0) - 1);
        else
                return page_zone(page)->zone_pgdat->node_id;
}
static inline __attribute__((always_inline)) unsigned long page_to_section(struct page *page)
{
        return (page->flags >> (((sizeof(page_flags_t)*8) - 0) * (0 != 0))) & ((1UL << 0) - 1);
}

static inline __attribute__((always_inline)) void set_page_zone(struct page *page, unsigned long zone)
{
        page->flags &= ~(((1UL << 2) - 1) << (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0)));
        page->flags |= (zone & ((1UL << 2) - 1)) << (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0));
}
static inline __attribute__((always_inline)) void set_page_node(struct page *page, unsigned long node)
{
        page->flags &= ~(((1UL << 0) - 1) << ((((sizeof(page_flags_t)*8) - 0) - 0) * (0 != 0)));
        page->flags |= (node & ((1UL << 0) - 1)) << ((((sizeof(page_flags_t)*8) - 0) - 0) * (0 != 0));
}
static inline __attribute__((always_inline)) void set_page_section(struct page *page, unsigned long section)
{
        page->flags &= ~(((1UL << 0) - 1) << (((sizeof(page_flags_t)*8) - 0) * (0 != 0)));
        page->flags |= (section & ((1UL << 0) - 1)) << (((sizeof(page_flags_t)*8) - 0) * (0 != 0));
}

static inline __attribute__((always_inline)) void set_page_links(struct page *page, unsigned long zone,
        unsigned long node, unsigned long pfn)
{
        set_page_zone(page, zone);
        set_page_node(page, node);
        set_page_section(page, ((pfn) >> 0));
}



extern struct page *mem_map;


static inline __attribute__((always_inline)) void *lowmem_page_address(struct page *page)
{
        return ((void *)((unsigned long)(((unsigned long)((page) - mem_map)) << 12)+((unsigned long)(0xC0000000UL))));
}
# 544 "include/linux/mm.h"
void *page_address(struct page *page);
void set_page_address(struct page *page, void *virtual);
void page_address_init(void);
# 566 "include/linux/mm.h"
extern struct address_space swapper_space;
static inline __attribute__((always_inline)) struct address_space *page_mapping(struct page *page)
{
        struct address_space *mapping = page->mapping;

        if (__builtin_expect(!!((__builtin_constant_p(15) ? constant_test_bit((15),(&(page)->flags)) : variable_test_bit((15),(&(page)->flags)))), 0))
                mapping = &swapper_space;
        else if (__builtin_expect(!!((unsigned long)mapping & 1), 0))
                mapping = ((void *)0);
        return mapping;
}

static inline __attribute__((always_inline)) int PageAnon(struct page *page)
{
        return ((unsigned long)page->mapping & 1) != 0;
}





static inline __attribute__((always_inline)) unsigned long page_index(struct page *page)
{
        if (__builtin_expect(!!((__builtin_constant_p(15) ? constant_test_bit((15),(&(page)->flags)) : variable_test_bit((15),(&(page)->flags)))), 0))
                return page->private;
        return page->index;
}






static inline __attribute__((always_inline)) void reset_page_mapcount(struct page *page)
{
        (((&(page)->_mapcount)->counter) = (-1));
}

static inline __attribute__((always_inline)) int page_mapcount(struct page *page)
{
        return ((&(page)->_mapcount)->counter) + 1;
}




static inline __attribute__((always_inline)) int page_mapped(struct page *page)
{
        return ((&(page)->_mapcount)->counter) >= 0;
}
# 641 "include/linux/mm.h"
extern void show_free_areas(void);


struct page *shmem_nopage(struct vm_area_struct *vma,
                        unsigned long address, int *type);
int shmem_set_policy(struct vm_area_struct *vma, struct mempolicy *new);
struct mempolicy *shmem_get_policy(struct vm_area_struct *vma,
                                        unsigned long addr);
int shmem_lock(struct file *file, int lock, struct user_struct *user);






struct file *shmem_file_setup(char *name, loff_t size, unsigned long flags);

int shmem_zero_setup(struct vm_area_struct *);

static inline __attribute__((always_inline)) int can_do_mlock(void)
{
        if (capable(14))
                return 1;
        if (get_current()->signal->rlim[8].rlim_cur != 0)
                return 1;
        return 0;
}
extern int user_shm_lock(size_t, struct user_struct *);
extern void user_shm_unlock(size_t, struct user_struct *);




struct zap_details {
        struct vm_area_struct *nonlinear_vma;
        struct address_space *check_mapping;
        unsigned long first_index;
        unsigned long last_index;
        spinlock_t *i_mmap_lock;
        unsigned long truncate_count;
};

unsigned long zap_page_range(struct vm_area_struct *vma, unsigned long address,
                unsigned long size, struct zap_details *);
unsigned long unmap_vmas(struct mmu_gather **tlb, struct mm_struct *mm,
                struct vm_area_struct *start_vma, unsigned long start_addr,
                unsigned long end_addr, unsigned long *nr_accounted,
                struct zap_details *);
void free_pgd_range(struct mmu_gather **tlb, unsigned long addr,
                unsigned long end, unsigned long floor, unsigned long ceiling);
void free_pgtables(struct mmu_gather **tlb, struct vm_area_struct *start_vma,
                unsigned long floor, unsigned long ceiling);
int copy_page_range(struct mm_struct *dst, struct mm_struct *src,
                        struct vm_area_struct *vma);
int zeromap_page_range(struct vm_area_struct *vma, unsigned long from,
                        unsigned long size, pgprot_t prot);
void unmap_mapping_range(struct address_space *mapping,
                loff_t const holebegin, loff_t const holelen, int even_cows);

static inline __attribute__((always_inline)) void unmap_shared_mapping_range(struct address_space *mapping,
                loff_t const holebegin, loff_t const holelen)
{
        unmap_mapping_range(mapping, holebegin, holelen, 0);
}

extern int vmtruncate(struct inode * inode, loff_t offset);
extern pud_t *__pud_alloc(struct mm_struct *mm, pgd_t *pgd, unsigned long address) __attribute__((regparm(3)));
extern pmd_t *__pmd_alloc(struct mm_struct *mm, pud_t *pud, unsigned long address) __attribute__((regparm(3)));
extern pte_t *pte_alloc_kernel(struct mm_struct *mm, pmd_t *pmd, unsigned long address) __attribute__((regparm(3)));
extern pte_t *pte_alloc_map(struct mm_struct *mm, pmd_t *pmd, unsigned long address) __attribute__((regparm(3)));
extern int install_page(struct mm_struct *mm, struct vm_area_struct *vma, unsigned long addr, struct page *page, pgprot_t prot);
extern int install_file_pte(struct mm_struct *mm, struct vm_area_struct *vma, unsigned long addr, unsigned long pgoff, pgprot_t prot);
extern int __handle_mm_fault(struct mm_struct *mm,struct vm_area_struct *vma, unsigned long address, int write_access);

static inline __attribute__((always_inline)) int handle_mm_fault(struct mm_struct *mm, struct vm_area_struct *vma, unsigned long address, int write_access)
{
        return __handle_mm_fault(mm, vma, address, write_access) & (~0x10);
}

extern int make_pages_present(unsigned long addr, unsigned long end);
extern int access_process_vm(struct task_struct *tsk, unsigned long addr, void *buf, int len, int write);
void install_arg_page(struct vm_area_struct *, struct page *, unsigned long);

int get_user_pages(struct task_struct *tsk, struct mm_struct *mm, unsigned long start,
                int len, int write, int force, struct page **pages, struct vm_area_struct **vmas);

int __set_page_dirty_buffers(struct page *page);
int __set_page_dirty_nobuffers(struct page *page);
int redirty_page_for_writepage(struct writeback_control *wbc,
                                struct page *page);
int set_page_dirty(struct page *page) __attribute__((regparm(3)));
int set_page_dirty_lock(struct page *page);
int clear_page_dirty_for_io(struct page *page);

extern unsigned long do_mremap(unsigned long addr,
                               unsigned long old_len, unsigned long new_len,
                               unsigned long flags, unsigned long new_addr);
# 750 "include/linux/mm.h"
typedef int (*shrinker_t)(int nr_to_scan, unsigned int gfp_mask);







struct shrinker;
extern struct shrinker *set_shrinker(int, shrinker_t);
extern void remove_shrinker(struct shrinker *shrinker);
# 773 "include/linux/mm.h"
static inline __attribute__((always_inline)) pud_t *pud_alloc(struct mm_struct *mm, pgd_t *pgd, unsigned long address)
{
        if (pgd_none(*pgd))
                return __pud_alloc(mm, pgd, address);
        return pud_offset(pgd, address);
}

static inline __attribute__((always_inline)) pmd_t *pmd_alloc(struct mm_struct *mm, pud_t *pud, unsigned long address)
{
        if (0)
                return __pmd_alloc(mm, pud, address);
        return ((pmd_t *) ((struct page *) ((void *)((unsigned long)(((((*(pud)).pgd).pgd)) & (~((1UL << 12)-1)))+((unsigned long)(0xC0000000UL))))) + (((address) >> 21) & (512 -1)));
}



extern void free_area_init(unsigned long * zones_size);
extern void free_area_init_node(int nid, pg_data_t *pgdat,
        unsigned long * zones_size, unsigned long zone_start_pfn,
        unsigned long *zholes_size);
extern void memmap_init_zone(unsigned long, int, unsigned long, unsigned long);
extern void mem_init(void);
extern void show_mem(void);
extern void si_meminfo(struct sysinfo * val);
extern void si_meminfo_node(struct sysinfo *val, int nid);




static inline __attribute__((always_inline)) void setup_per_cpu_pageset(void) {}



void vma_prio_tree_add(struct vm_area_struct *, struct vm_area_struct *old);
void vma_prio_tree_insert(struct vm_area_struct *, struct prio_tree_root *);
void vma_prio_tree_remove(struct vm_area_struct *, struct prio_tree_root *);
struct vm_area_struct *vma_prio_tree_next(struct vm_area_struct *vma,
        struct prio_tree_iter *iter);





static inline __attribute__((always_inline)) void vma_nonlinear_insert(struct vm_area_struct *vma,
                                        struct list_head *list)
{
        vma->shared.vm_set.parent = ((void *)0);
        list_add_tail(&vma->shared.vm_set.list, list);
}


extern int __vm_enough_memory(long pages, int cap_sys_admin);
extern void vma_adjust(struct vm_area_struct *vma, unsigned long start,
        unsigned long end, unsigned long pgoff, struct vm_area_struct *insert);
extern struct vm_area_struct *vma_merge(struct mm_struct *,
        struct vm_area_struct *prev, unsigned long addr, unsigned long end,
        unsigned long vm_flags, struct anon_vma *, struct file *, unsigned long,
        struct mempolicy *);
extern struct anon_vma *find_mergeable_anon_vma(struct vm_area_struct *);
extern int split_vma(struct mm_struct *,
        struct vm_area_struct *, unsigned long addr, int new_below);
extern int insert_vm_struct(struct mm_struct *, struct vm_area_struct *);
extern void __vma_link_rb(struct mm_struct *, struct vm_area_struct *,
        struct rb_node **, struct rb_node *);
extern struct vm_area_struct *copy_vma(struct vm_area_struct **,
        unsigned long addr, unsigned long len, unsigned long pgoff);
extern void exit_mmap(struct mm_struct *);
extern int may_expand_vm(struct mm_struct *mm, unsigned long npages);

extern unsigned long get_unmapped_area(struct file *, unsigned long, unsigned long, unsigned long, unsigned long);

extern unsigned long do_mmap_pgoff(struct file *file, unsigned long addr,
        unsigned long len, unsigned long prot,
        unsigned long flag, unsigned long pgoff);

static inline __attribute__((always_inline)) unsigned long do_mmap(struct file *file, unsigned long addr,
        unsigned long len, unsigned long prot,
        unsigned long flag, unsigned long offset)
{
        unsigned long ret = -22;
        if ((offset + (((len)+(1UL << 12)-1)&(~((1UL << 12)-1)))) < offset)
                goto out;
        if (!(offset & ~(~((1UL << 12)-1))))
                ret = do_mmap_pgoff(file, addr, len, prot, flag, offset >> 12);
out:
        return ret;
}

extern int do_munmap(struct mm_struct *, unsigned long, size_t);

extern unsigned long do_brk(unsigned long, unsigned long);


extern unsigned long page_unuse(struct page *);
extern void truncate_inode_pages(struct address_space *, loff_t);


extern struct page *filemap_nopage(struct vm_area_struct *, unsigned long, int *);
extern int filemap_populate(struct vm_area_struct *, unsigned long,
                unsigned long, pgprot_t, unsigned long, int);


int write_one_page(struct page *page, int wait);







int do_page_cache_readahead(struct address_space *mapping, struct file *filp,
                        unsigned long offset, unsigned long nr_to_read);
int force_page_cache_readahead(struct address_space *mapping, struct file *filp,
                        unsigned long offset, unsigned long nr_to_read);
unsigned long page_cache_readahead(struct address_space *mapping,
                          struct file_ra_state *ra,
                          struct file *filp,
                          unsigned long offset,
                          unsigned long size);
void handle_ra_miss(struct address_space *mapping,
                    struct file_ra_state *ra, unsigned long offset);
unsigned long max_sane_readahead(unsigned long nr);


extern int expand_stack(struct vm_area_struct * vma, unsigned long address);


extern struct vm_area_struct * find_vma(struct mm_struct * mm, unsigned long addr);
extern struct vm_area_struct * find_vma_prev(struct mm_struct * mm, unsigned long addr,
                                             struct vm_area_struct **pprev);



static inline __attribute__((always_inline)) struct vm_area_struct * find_vma_intersection(struct mm_struct * mm, unsigned long start_addr, unsigned long end_addr)
{
        struct vm_area_struct * vma = find_vma(mm,start_addr);

        if (vma && end_addr <= vma->vm_start)
                vma = ((void *)0);
        return vma;
}

static inline __attribute__((always_inline)) unsigned long vma_pages(struct vm_area_struct *vma)
{
        return (vma->vm_end - vma->vm_start) >> 12;
}

extern struct vm_area_struct *find_extend_vma(struct mm_struct *mm, unsigned long addr);

extern struct page * vmalloc_to_page(void *addr);
extern unsigned long vmalloc_to_pfn(void *addr);
extern struct page * follow_page(struct mm_struct *mm, unsigned long address,
                int write);
extern int check_user_page_readable(struct mm_struct *mm, unsigned long address);
int remap_pfn_range(struct vm_area_struct *, unsigned long,
                unsigned long, unsigned long, pgprot_t);


void __vm_stat_account(struct mm_struct *, unsigned long, struct file *, long);







static inline __attribute__((always_inline)) void vm_stat_account(struct vm_area_struct *vma)
{
        __vm_stat_account(vma->vm_mm, vma->vm_flags, vma->vm_file,
                                                        vma_pages(vma));
}

static inline __attribute__((always_inline)) void vm_stat_unaccount(struct vm_area_struct *vma)
{
        __vm_stat_account(vma->vm_mm, vma->vm_flags, vma->vm_file,
                                                        -vma_pages(vma));
}


extern void update_mem_hiwater(struct task_struct *tsk);


static inline __attribute__((always_inline)) void
kernel_map_pages(struct page *page, int numpages, int enable)
{
}


extern struct vm_area_struct *get_gate_vma(struct task_struct *tsk);




int in_gate_area_no_task(unsigned long addr);
# 57 "drivers/ide/ide-disk.c" 2

# 1 "include/linux/major.h" 1
# 59 "drivers/ide/ide-disk.c" 2

# 1 "include/linux/genhd.h" 1
# 15 "include/linux/genhd.h"
# 1 "include/linux/device.h" 1
# 15 "include/linux/device.h"
# 1 "include/linux/ioport.h" 1
# 16 "include/linux/ioport.h"
struct resource {
        const char *name;
        unsigned long start, end;
        unsigned long flags;
        struct resource *parent, *sibling, *child;
};

struct resource_list {
        struct resource_list *next;
        struct resource *res;
        struct pci_dev *dev;
};
# 91 "include/linux/ioport.h"
extern struct resource ioport_resource;
extern struct resource iomem_resource;

extern int request_resource(struct resource *root, struct resource *new);
extern struct resource * ____request_resource(struct resource *root, struct resource *new);
extern int release_resource(struct resource *new);
extern int insert_resource(struct resource *parent, struct resource *new);
extern int allocate_resource(struct resource *root, struct resource *new,
                             unsigned long size,
                             unsigned long min, unsigned long max,
                             unsigned long align,
                             void (*alignf)(void *, struct resource *,
                                            unsigned long, unsigned long),
                             void *alignf_data);
int adjust_resource(struct resource *res, unsigned long start,
                    unsigned long size);






extern struct resource * __request_region(struct resource *, unsigned long start, unsigned long n, const char *name);






extern int __check_region(struct resource *, unsigned long, unsigned long);
extern void __release_region(struct resource *, unsigned long, unsigned long);

static inline __attribute__((always_inline)) int __attribute__((deprecated)) check_region(unsigned long s, unsigned long n)
{
        return __check_region(&ioport_resource, s, n);
}
# 16 "include/linux/device.h" 2

# 1 "include/linux/klist.h" 1
# 20 "include/linux/klist.h"
struct klist_node;
struct klist {
        spinlock_t k_lock;
        struct list_head k_list;
        void (*get)(struct klist_node *);
        void (*put)(struct klist_node *);
};


extern void klist_init(struct klist * k, void (*get)(struct klist_node *),
                       void (*put)(struct klist_node *));

struct klist_node {
        struct klist * n_klist;
        struct list_head n_node;
        struct kref n_ref;
        struct completion n_removed;
};

extern void klist_add_tail(struct klist_node * n, struct klist * k);
extern void klist_add_head(struct klist_node * n, struct klist * k);

extern void klist_del(struct klist_node * n);
extern void klist_remove(struct klist_node * n);

extern int klist_node_attached(struct klist_node * n);


struct klist_iter {
        struct klist * i_klist;
        struct list_head * i_head;
        struct klist_node * i_cur;
};


extern void klist_iter_init(struct klist * k, struct klist_iter * i);
extern void klist_iter_init_node(struct klist * k, struct klist_iter * i,
                                 struct klist_node * n);
extern void klist_iter_exit(struct klist_iter * i);
extern struct klist_node * klist_next(struct klist_iter * i);
# 18 "include/linux/device.h" 2
# 31 "include/linux/device.h"
enum {
        SUSPEND_NOTIFY,
        SUSPEND_SAVE_STATE,
        SUSPEND_DISABLE,
        SUSPEND_POWER_DOWN,
};

enum {
        RESUME_POWER_ON,
        RESUME_RESTORE_STATE,
        RESUME_ENABLE,
};

struct device;
struct device_driver;
struct class;
struct class_device;

struct bus_type {
        const char * name;

        struct subsystem subsys;
        struct kset drivers;
        struct kset devices;
        struct klist klist_devices;
        struct klist klist_drivers;

        struct bus_attribute * bus_attrs;
        struct device_attribute * dev_attrs;
        struct driver_attribute * drv_attrs;

        int (*match)(struct device * dev, struct device_driver * drv);
        int (*hotplug) (struct device *dev, char **envp,
                                    int num_envp, char *buffer, int buffer_size);
        int (*suspend)(struct device * dev, pm_message_t state);
        int (*resume)(struct device * dev);
};

extern int bus_register(struct bus_type * bus);
extern void bus_unregister(struct bus_type * bus);

extern void bus_rescan_devices(struct bus_type * bus);

extern struct bus_type * get_bus(struct bus_type * bus);
extern void put_bus(struct bus_type * bus);

extern struct bus_type * find_bus(char * name);



int bus_for_each_dev(struct bus_type * bus, struct device * start, void * data,
                     int (*fn)(struct device *, void *));
struct device * bus_find_device(struct bus_type *bus, struct device *start,
                                void *data, int (*match)(struct device *, void *));

int bus_for_each_drv(struct bus_type * bus, struct device_driver * start,
                     void * data, int (*fn)(struct device_driver *, void *));




struct bus_attribute {
        struct attribute attr;
        ssize_t (*show)(struct bus_type *, char * buf);
        ssize_t (*store)(struct bus_type *, const char * buf, size_t count);
};




extern int bus_create_file(struct bus_type *, struct bus_attribute *);
extern void bus_remove_file(struct bus_type *, struct bus_attribute *);

struct device_driver {
        const char * name;
        struct bus_type * bus;

        struct completion unloaded;
        struct kobject kobj;
        struct klist klist_devices;
        struct klist_node knode_bus;

        struct module * owner;

        int (*probe) (struct device * dev);
        int (*remove) (struct device * dev);
        void (*shutdown) (struct device * dev);
        int (*suspend) (struct device * dev, pm_message_t state, u32 level);
        int (*resume) (struct device * dev, u32 level);
};


extern int driver_register(struct device_driver * drv);
extern void driver_unregister(struct device_driver * drv);

extern struct device_driver * get_driver(struct device_driver * drv);
extern void put_driver(struct device_driver * drv);
extern struct device_driver *driver_find(const char *name, struct bus_type *bus);




struct driver_attribute {
        struct attribute attr;
        ssize_t (*show)(struct device_driver *, char * buf);
        ssize_t (*store)(struct device_driver *, const char * buf, size_t count);
};




extern int driver_create_file(struct device_driver *, struct driver_attribute *);
extern void driver_remove_file(struct device_driver *, struct driver_attribute *);

extern int driver_for_each_device(struct device_driver * drv, struct device * start,
                                  void * data, int (*fn)(struct device *, void *));
struct device * driver_find_device(struct device_driver *drv,
                                   struct device *start, void *data,
                                   int (*match)(struct device *, void *));





struct class {
        const char * name;
        struct module * owner;

        struct subsystem subsys;
        struct list_head children;
        struct list_head interfaces;
        struct semaphore sem;

        struct class_attribute * class_attrs;
        struct class_device_attribute * class_dev_attrs;

        int (*hotplug)(struct class_device *dev, char **envp,
                           int num_envp, char *buffer, int buffer_size);

        void (*release)(struct class_device *dev);
        void (*class_release)(struct class *class);
};

extern int class_register(struct class *);
extern void class_unregister(struct class *);

extern struct class * class_get(struct class *);
extern void class_put(struct class *);


struct class_attribute {
        struct attribute attr;
        ssize_t (*show)(struct class *, char * buf);
        ssize_t (*store)(struct class *, const char * buf, size_t count);
};




extern int class_create_file(struct class *, const struct class_attribute *);
extern void class_remove_file(struct class *, const struct class_attribute *);


struct class_device {
        struct list_head node;

        struct kobject kobj;
        struct class * class;
        dev_t devt;
        struct class_device_attribute *devt_attr;
        struct device * dev;
        void * class_data;

        char class_id[20];
};

static inline __attribute__((always_inline)) void *
class_get_devdata (struct class_device *dev)
{
        return dev->class_data;
}

static inline __attribute__((always_inline)) void
class_set_devdata (struct class_device *dev, void *data)
{
        dev->class_data = data;
}


extern int class_device_register(struct class_device *);
extern void class_device_unregister(struct class_device *);
extern void class_device_initialize(struct class_device *);
extern int class_device_add(struct class_device *);
extern void class_device_del(struct class_device *);

extern int class_device_rename(struct class_device *, char *);

extern struct class_device * class_device_get(struct class_device *);
extern void class_device_put(struct class_device *);

struct class_device_attribute {
        struct attribute attr;
        ssize_t (*show)(struct class_device *, char * buf);
        ssize_t (*store)(struct class_device *, const char * buf, size_t count);
};





extern int class_device_create_file(struct class_device *,
                                    const struct class_device_attribute *);
extern void class_device_remove_file(struct class_device *,
                                     const struct class_device_attribute *);
extern int class_device_create_bin_file(struct class_device *,
                                        struct bin_attribute *);
extern void class_device_remove_bin_file(struct class_device *,
                                         struct bin_attribute *);

struct class_interface {
        struct list_head node;
        struct class *class;

        int (*add) (struct class_device *);
        void (*remove) (struct class_device *);
};

extern int class_interface_register(struct class_interface *);
extern void class_interface_unregister(struct class_interface *);

extern struct class *class_create(struct module *owner, char *name);
extern void class_destroy(struct class *cls);
extern struct class_device *class_device_create(struct class *cls, dev_t devt,
                                                struct device *device, char *fmt, ...)
                                        __attribute__((format(printf,4,5)));
extern void class_device_destroy(struct class *cls, dev_t devt);


struct device {
        struct klist klist_children;
        struct klist_node knode_parent;
        struct klist_node knode_driver;
        struct klist_node knode_bus;
        struct device * parent;

        struct kobject kobj;
        char bus_id[20];

        struct semaphore sem;



        struct bus_type * bus;
        struct device_driver *driver;

        void *driver_data;
        void *platform_data;

        void *firmware_data;

        struct dev_pm_info power;

        u64 *dma_mask;
        u64 coherent_dma_mask;





        struct list_head dma_pools;

        struct dma_coherent_mem *dma_mem;


        void (*release)(struct device * dev);
};

static inline __attribute__((always_inline)) void *
dev_get_drvdata (struct device *dev)
{
        return dev->driver_data;
}

static inline __attribute__((always_inline)) void
dev_set_drvdata (struct device *dev, void *data)
{
        dev->driver_data = data;
}

static inline __attribute__((always_inline)) int device_is_registered(struct device *dev)
{
        return klist_node_attached(&dev->knode_bus);
}




extern int device_register(struct device * dev);
extern void device_unregister(struct device * dev);
extern void device_initialize(struct device * dev);
extern int device_add(struct device * dev);
extern void device_del(struct device * dev);
extern int device_for_each_child(struct device *, void *,
                     int (*fn)(struct device *, void *));





extern void device_bind_driver(struct device * dev);
extern void device_release_driver(struct device * dev);
extern int device_attach(struct device * dev);
extern void driver_attach(struct device_driver * drv);




struct device_attribute {
        struct attribute attr;
        ssize_t (*show)(struct device *dev, struct device_attribute *attr,
                        char *buf);
        ssize_t (*store)(struct device *dev, struct device_attribute *attr,
                         const char *buf, size_t count);
};





extern int device_create_file(struct device *device, struct device_attribute * entry);
extern void device_remove_file(struct device * dev, struct device_attribute * attr);







extern int (*platform_notify)(struct device * dev);

extern int (*platform_notify_remove)(struct device * dev);






extern struct device * get_device(struct device * dev);
extern void put_device(struct device * dev);




struct platform_device {
        const char * name;
        u32 id;
        struct device dev;
        u32 num_resources;
        struct resource * resource;
};



extern int platform_device_register(struct platform_device *);
extern void platform_device_unregister(struct platform_device *);

extern struct bus_type platform_bus_type;
extern struct device platform_bus;

extern struct resource *platform_get_resource(struct platform_device *, unsigned int, unsigned int);
extern int platform_get_irq(struct platform_device *, unsigned int);
extern struct resource *platform_get_resource_byname(struct platform_device *, unsigned int, char *);
extern int platform_get_irq_byname(struct platform_device *, char *);
extern int platform_add_devices(struct platform_device **, int);

extern struct platform_device *platform_device_register_simple(char *, unsigned int, struct resource *, unsigned int);


extern void device_shutdown(void);



extern int firmware_register(struct subsystem *);
extern void firmware_unregister(struct subsystem *);
# 16 "include/linux/genhd.h" 2




enum {


        DOS_EXTENDED_PARTITION = 5,
        LINUX_EXTENDED_PARTITION = 0x85,
        WIN98_EXTENDED_PARTITION = 0x0f,

        LINUX_SWAP_PARTITION = 0x82,
        LINUX_RAID_PARTITION = 0xfd,

        SOLARIS_X86_PARTITION = LINUX_SWAP_PARTITION,
        NEW_SOLARIS_X86_PARTITION = 0xbf,

        DM6_AUX1PARTITION = 0x51,
        DM6_AUX3PARTITION = 0x53,
        DM6_PARTITION = 0x54,
        EZD_PARTITION = 0x55,

        FREEBSD_PARTITION = 0xa5,
        OPENBSD_PARTITION = 0xa6,
        NETBSD_PARTITION = 0xa9,
        BSDI_PARTITION = 0xb7,
        MINIX_PARTITION = 0x81,
        UNIXWARE_PARTITION = 0x63,
};
# 64 "include/linux/genhd.h"
struct partition {
        unsigned char boot_ind;
        unsigned char head;
        unsigned char sector;
        unsigned char cyl;
        unsigned char sys_ind;
        unsigned char end_head;
        unsigned char end_sector;
        unsigned char end_cyl;
        __le32 start_sect;
        __le32 nr_sects;
} __attribute__((packed));

struct hd_struct {
        sector_t start_sect;
        sector_t nr_sects;
        struct kobject kobj;
        unsigned reads, read_sectors, writes, write_sectors;
        int policy, partno;
};







struct disk_stats {
        unsigned read_sectors, write_sectors;
        unsigned reads, writes;
        unsigned read_merges, write_merges;
        unsigned read_ticks, write_ticks;
        unsigned io_ticks;
        unsigned time_in_queue;
};

struct gendisk {
        int major;
        int first_minor;
        int minors;

        char disk_name[32];
        struct hd_struct **part;
        struct block_device_operations *fops;
        struct request_queue *queue;
        void *private_data;
        sector_t capacity;

        int flags;
        char devfs_name[64];
        int number;
        struct device *driverfs_dev;
        struct kobject kobj;

        struct timer_rand_state *random;
        int policy;

        atomic_t sync_io;
        unsigned long stamp, stamp_idle;
        int in_flight;

        struct disk_stats *dkstats;



};


struct disk_attribute {
        struct attribute attr;
        ssize_t (*show)(struct gendisk *, char *);
};
# 159 "include/linux/genhd.h"
static inline __attribute__((always_inline)) void disk_stat_set_all(struct gendisk *gendiskp, int value) {
        int i;
        for (i=0; i < 8; i++) {
                if ((__builtin_constant_p(((i))) ? constant_test_bit((((i))),((cpu_possible_map).bits)) : variable_test_bit((((i))),((cpu_possible_map).bits)))) {
                        (__builtin_constant_p(value) ? (__builtin_constant_p((sizeof (struct disk_stats))) ? __constant_c_and_count_memset(((({ struct percpu_data *__p = (struct percpu_data *)~(unsigned long)(gendiskp->dkstats); (__typeof__(gendiskp->dkstats))__p->ptrs[(i)]; }))),((0x01010101UL*(unsigned char)(value))),((sizeof (struct disk_stats)))) : __constant_c_memset(((({ struct percpu_data *__p = (struct percpu_data *)~(unsigned long)(gendiskp->dkstats); (__typeof__(gendiskp->dkstats))__p->ptrs[(i)]; }))),((0x01010101UL*(unsigned char)(value))),((sizeof (struct disk_stats))))) : (__builtin_constant_p((sizeof (struct disk_stats))) ? __memset_generic((((({ struct percpu_data *__p = (struct percpu_data *)~(unsigned long)(gendiskp->dkstats); (__typeof__(gendiskp->dkstats))__p->ptrs[(i)]; })))),(((value))),(((sizeof (struct disk_stats))))) : __memset_generic(((({ struct percpu_data *__p = (struct percpu_data *)~(unsigned long)(gendiskp->dkstats); (__typeof__(gendiskp->dkstats))__p->ptrs[(i)]; }))),((value)),((sizeof (struct disk_stats))))));

                }
        }
}
# 200 "include/linux/genhd.h"
static inline __attribute__((always_inline)) int init_disk_stats(struct gendisk *disk)
{
        disk->dkstats = ((struct disk_stats *)(__alloc_percpu(sizeof(struct disk_stats), __alignof__(struct disk_stats))));
        if (!disk->dkstats)
                return 0;
        return 1;
}

static inline __attribute__((always_inline)) void free_disk_stats(struct gendisk *disk)
{
        free_percpu(disk->dkstats);
}
# 224 "include/linux/genhd.h"
extern void disk_round_stats(struct gendisk *disk);


extern int get_blkdev_list(char *, int);
extern void add_disk(struct gendisk *disk);
extern void del_gendisk(struct gendisk *gp);
extern void unlink_gendisk(struct gendisk *gp);
extern struct gendisk *get_gendisk(dev_t dev, int *part);

extern void set_device_ro(struct block_device *bdev, int flag);
extern void set_disk_ro(struct gendisk *disk, int flag);


extern void add_disk_randomness(struct gendisk *disk);
extern void rand_initialize_disk(struct gendisk *disk);

static inline __attribute__((always_inline)) sector_t get_start_sect(struct block_device *bdev)
{
        return bdev->bd_contains == bdev ? 0 : bdev->bd_part->start_sect;
}
static inline __attribute__((always_inline)) sector_t get_capacity(struct gendisk *disk)
{
        return disk->capacity;
}
static inline __attribute__((always_inline)) void set_capacity(struct gendisk *disk, sector_t size)
{
        disk->capacity = size;
}
# 260 "include/linux/genhd.h"
struct solaris_x86_slice {
        __le16 s_tag;
        __le16 s_flag;
        __le32 s_start;
        __le32 s_size;
};

struct solaris_x86_vtoc {
        unsigned int v_bootinfo[3];
        __le32 v_sanity;
        __le32 v_version;
        char v_volume[8];
        __le16 v_sectorsz;
        __le16 v_nparts;
        unsigned int v_reserved[10];
        struct solaris_x86_slice
                v_slice[8];
        unsigned int timestamp[8];
        char v_asciilabel[128];
};
# 295 "include/linux/genhd.h"
struct bsd_disklabel {
        __le32 d_magic;
        __s16 d_type;
        __s16 d_subtype;
        char d_typename[16];
        char d_packname[16];
        __u32 d_secsize;
        __u32 d_nsectors;
        __u32 d_ntracks;
        __u32 d_ncylinders;
        __u32 d_secpercyl;
        __u32 d_secperunit;
        __u16 d_sparespertrack;
        __u16 d_sparespercyl;
        __u32 d_acylinders;
        __u16 d_rpm;
        __u16 d_interleave;
        __u16 d_trackskew;
        __u16 d_cylskew;
        __u32 d_headswitch;
        __u32 d_trkseek;
        __u32 d_flags;

        __u32 d_drivedata[5];

        __u32 d_spare[5];
        __le32 d_magic2;
        __le16 d_checksum;


        __le16 d_npartitions;
        __le32 d_bbsize;
        __le32 d_sbsize;
        struct bsd_partition {
                __le32 p_size;
                __le32 p_offset;
                __le32 p_fsize;
                __u8 p_fstype;
                __u8 p_frag;
                __le16 p_cpg;
        } d_partitions[16];
};
# 351 "include/linux/genhd.h"
struct unixware_slice {
        __le16 s_label;
        __le16 s_flags;
        __le32 start_sect;
        __le32 nr_sects;
};

struct unixware_disklabel {
        __le32 d_type;
        __le32 d_magic;
        __le32 d_version;
        char d_serial[12];
        __le32 d_ncylinders;
        __le32 d_ntracks;
        __le32 d_nsectors;
        __le32 d_secsize;
        __le32 d_part_start;
        __le32 d_unknown1[12];
        __le32 d_alt_tbl;
        __le32 d_alt_len;
        __le32 d_phys_cyl;
        __le32 d_phys_trk;
        __le32 d_phys_sec;
        __le32 d_phys_bytes;
        __le32 d_unknown2;
        __le32 d_unknown3;
        __le32 d_pad[8];

        struct unixware_vtoc {
                __le32 v_magic;
                __le32 v_version;
                char v_name[8];
                __le16 v_nslices;
                __le16 v_unknown1;
                __le32 v_reserved[10];
                struct unixware_slice
                        v_slice[16];
        } vtoc;

};
# 400 "include/linux/genhd.h"
char *disk_name (struct gendisk *hd, int part, char *buf);

extern int rescan_partitions(struct gendisk *disk, struct block_device *bdev);
extern void add_partition(struct gendisk *, int, sector_t, sector_t);
extern void delete_partition(struct gendisk *, int);

extern struct gendisk *alloc_disk_node(int minors, int node_id);
extern struct gendisk *alloc_disk(int minors);
extern struct kobject *get_disk(struct gendisk *disk);
extern void put_disk(struct gendisk *disk);

extern void blk_register_region(dev_t dev, unsigned long range,
                        struct module *module,
                        struct kobject *(*probe)(dev_t, int *, void *),
                        int (*lock)(dev_t, void *),
                        void *data);
extern void blk_unregister_region(dev_t dev, unsigned long range);

static inline __attribute__((always_inline)) struct block_device *bdget_disk(struct gendisk *disk, int index)
{
        return bdget((((disk->major) << 20) | (disk->first_minor)) + index);
}
# 61 "drivers/ide/ide-disk.c" 2

# 1 "include/linux/delay.h" 1
# 10 "include/linux/delay.h"
extern unsigned long loops_per_jiffy;

# 1 "include/asm/delay.h" 1
# 10 "include/asm/delay.h"
extern void __bad_udelay(void);
extern void __bad_ndelay(void);

extern void __udelay(unsigned long usecs);
extern void __ndelay(unsigned long nsecs);
extern void __const_udelay(unsigned long usecs);
extern void __delay(unsigned long loops);
# 13 "include/linux/delay.h" 2
# 41 "include/linux/delay.h"
void calibrate_delay(void);
void msleep(unsigned int msecs);
unsigned long msleep_interruptible(unsigned int msecs);

static inline __attribute__((always_inline)) void ssleep(unsigned int seconds)
{
        msleep(seconds * 1000);
}
# 63 "drivers/ide/ide-disk.c" 2



# 1 "include/linux/ide.h" 1
# 12 "include/linux/ide.h"
# 1 "include/linux/hdreg.h" 1




# 1 "include/linux/ata.h" 1
# 38 "include/linux/ata.h"
enum {

        ATA_MAX_DEVICES = 2,
        ATA_MAX_PRD = 256,
        ATA_SECT_SIZE = 512,

        ATA_ID_WORDS = 256,
        ATA_ID_PROD_OFS = 27,
        ATA_ID_FW_REV_OFS = 23,
        ATA_ID_SERNO_OFS = 10,
        ATA_ID_MAJOR_VER = 80,
        ATA_ID_PIO_MODES = 64,
        ATA_ID_MWDMA_MODES = 63,
        ATA_ID_UDMA_MODES = 88,
        ATA_ID_PIO4 = (1 << 1),

        ATA_PCI_CTL_OFS = 2,
        ATA_SERNO_LEN = 20,
        ATA_UDMA0 = (1 << 0),
        ATA_UDMA1 = ATA_UDMA0 | (1 << 1),
        ATA_UDMA2 = ATA_UDMA1 | (1 << 2),
        ATA_UDMA3 = ATA_UDMA2 | (1 << 3),
        ATA_UDMA4 = ATA_UDMA3 | (1 << 4),
        ATA_UDMA5 = ATA_UDMA4 | (1 << 5),
        ATA_UDMA6 = ATA_UDMA5 | (1 << 6),
        ATA_UDMA7 = ATA_UDMA6 | (1 << 7),


        ATA_UDMA_MASK_40C = ATA_UDMA2,


        ATA_PRD_SZ = 8,
        ATA_PRD_TBL_SZ = (ATA_MAX_PRD * ATA_PRD_SZ),
        ATA_PRD_EOT = (1 << 31),

        ATA_DMA_TABLE_OFS = 4,
        ATA_DMA_STATUS = 2,
        ATA_DMA_CMD = 0,
        ATA_DMA_WR = (1 << 3),
        ATA_DMA_START = (1 << 0),
        ATA_DMA_INTR = (1 << 2),
        ATA_DMA_ERR = (1 << 1),
        ATA_DMA_ACTIVE = (1 << 0),


        ATA_HOB = (1 << 7),
        ATA_NIEN = (1 << 1),
        ATA_LBA = (1 << 6),
        ATA_DEV1 = (1 << 4),
        ATA_DEVICE_OBS = (1 << 7) | (1 << 5),
        ATA_DEVCTL_OBS = (1 << 3),
        ATA_BUSY = (1 << 7),
        ATA_DRDY = (1 << 6),
        ATA_DF = (1 << 5),
        ATA_DRQ = (1 << 3),
        ATA_ERR = (1 << 0),
        ATA_SRST = (1 << 2),
        ATA_ABORTED = (1 << 2),


        ATA_REG_DATA = 0x00,
        ATA_REG_ERR = 0x01,
        ATA_REG_NSECT = 0x02,
        ATA_REG_LBAL = 0x03,
        ATA_REG_LBAM = 0x04,
        ATA_REG_LBAH = 0x05,
        ATA_REG_DEVICE = 0x06,
        ATA_REG_STATUS = 0x07,

        ATA_REG_FEATURE = ATA_REG_ERR,
        ATA_REG_CMD = ATA_REG_STATUS,
        ATA_REG_BYTEL = ATA_REG_LBAM,
        ATA_REG_BYTEH = ATA_REG_LBAH,
        ATA_REG_DEVSEL = ATA_REG_DEVICE,
        ATA_REG_IRQ = ATA_REG_NSECT,


        ATA_CMD_CHK_POWER = 0xE5,
        ATA_CMD_STANDBY = 0xE2,
        ATA_CMD_IDLE = 0xE3,
        ATA_CMD_EDD = 0x90,
        ATA_CMD_FLUSH = 0xE7,
        ATA_CMD_FLUSH_EXT = 0xEA,
        ATA_CMD_ID_ATA = 0xEC,
        ATA_CMD_ID_ATAPI = 0xA1,
        ATA_CMD_READ = 0xC8,
        ATA_CMD_READ_EXT = 0x25,
        ATA_CMD_WRITE = 0xCA,
        ATA_CMD_WRITE_EXT = 0x35,
        ATA_CMD_PIO_READ = 0x20,
        ATA_CMD_PIO_READ_EXT = 0x24,
        ATA_CMD_PIO_WRITE = 0x30,
        ATA_CMD_PIO_WRITE_EXT = 0x34,
        ATA_CMD_SET_FEATURES = 0xEF,
        ATA_CMD_PACKET = 0xA0,
        ATA_CMD_VERIFY = 0x40,
        ATA_CMD_VERIFY_EXT = 0x42,


        SETFEATURES_XFER = 0x03,
        XFER_UDMA_7 = 0x47,
        XFER_UDMA_6 = 0x46,
        XFER_UDMA_5 = 0x45,
        XFER_UDMA_4 = 0x44,
        XFER_UDMA_3 = 0x43,
        XFER_UDMA_2 = 0x42,
        XFER_UDMA_1 = 0x41,
        XFER_UDMA_0 = 0x40,
        XFER_MW_DMA_2 = 0x22,
        XFER_MW_DMA_1 = 0x21,
        XFER_MW_DMA_0 = 0x20,
        XFER_PIO_4 = 0x0C,
        XFER_PIO_3 = 0x0B,
        XFER_PIO_2 = 0x0A,
        XFER_PIO_1 = 0x09,
        XFER_PIO_0 = 0x08,
        XFER_SW_DMA_2 = 0x12,
        XFER_SW_DMA_1 = 0x11,
        XFER_SW_DMA_0 = 0x10,
        XFER_PIO_SLOW = 0x00,


        ATAPI_PKT_DMA = (1 << 0),
        ATAPI_DMADIR = (1 << 2),

        ATAPI_CDB_LEN = 16,


        ATA_CBL_NONE = 0,
        ATA_CBL_PATA40 = 1,
        ATA_CBL_PATA80 = 2,
        ATA_CBL_PATA_UNK = 3,
        ATA_CBL_SATA = 4,


        SCR_STATUS = 0,
        SCR_ERROR = 1,
        SCR_CONTROL = 2,
        SCR_ACTIVE = 3,
        SCR_NOTIFICATION = 4,


        ATA_TFLAG_LBA48 = (1 << 0),
        ATA_TFLAG_ISADDR = (1 << 1),
        ATA_TFLAG_DEVICE = (1 << 2),
        ATA_TFLAG_WRITE = (1 << 3),
};

enum ata_tf_protocols {

        ATA_PROT_UNKNOWN,
        ATA_PROT_NODATA,
        ATA_PROT_PIO,
        ATA_PROT_PIO_MULT,
        ATA_PROT_DMA,
        ATA_PROT_ATAPI,
        ATA_PROT_ATAPI_NODATA,
        ATA_PROT_ATAPI_DMA,
};

enum ata_ioctls {
        ATA_IOC_GET_IO32 = 0x309,
        ATA_IOC_SET_IO32 = 0x324,
};



struct ata_prd {
        u32 addr;
        u32 flags_len;
};

struct ata_taskfile {
        unsigned long flags;
        u8 protocol;

        u8 ctl;

        u8 hob_feature;
        u8 hob_nsect;
        u8 hob_lbal;
        u8 hob_lbam;
        u8 hob_lbah;

        u8 feature;
        u8 nsect;
        u8 lbal;
        u8 lbam;
        u8 lbah;

        u8 device;

        u8 command;
};
# 253 "include/linux/ata.h"
static inline __attribute__((always_inline)) int atapi_cdb_len(u16 *dev_id)
{
        u16 tmp = dev_id[0] & 0x3;
        switch (tmp) {
        case 0: return 12;
        case 1: return 16;
        default: return -1;
        }
}

static inline __attribute__((always_inline)) int is_atapi_taskfile(struct ata_taskfile *tf)
{
        return (tf->protocol == ATA_PROT_ATAPI) ||
               (tf->protocol == ATA_PROT_ATAPI_NODATA) ||
               (tf->protocol == ATA_PROT_ATAPI_DMA);
}

static inline __attribute__((always_inline)) int ata_ok(u8 status)
{
        return ((status & (ATA_BUSY | ATA_DRDY | ATA_DF | ATA_DRQ | ATA_ERR))
                        == ATA_DRDY);
}
# 6 "include/linux/hdreg.h" 2
# 88 "include/linux/hdreg.h"
typedef unsigned char task_ioreg_t;
typedef unsigned long sata_ioreg_t;

typedef union ide_reg_valid_s {
        unsigned all : 16;
        struct {
                unsigned data : 1;
                unsigned error_feature : 1;
                unsigned sector : 1;
                unsigned nsector : 1;
                unsigned lcyl : 1;
                unsigned hcyl : 1;
                unsigned select : 1;
                unsigned status_command : 1;

                unsigned data_hob : 1;
                unsigned error_feature_hob : 1;
                unsigned sector_hob : 1;
                unsigned nsector_hob : 1;
                unsigned lcyl_hob : 1;
                unsigned hcyl_hob : 1;
                unsigned select_hob : 1;
                unsigned control_hob : 1;
        } b;
} ide_reg_valid_t;

typedef struct ide_task_request_s {
        task_ioreg_t io_ports[8];
        task_ioreg_t hob_ports[8];
        ide_reg_valid_t out_flags;
        ide_reg_valid_t in_flags;
        int data_phase;
        int req_cmd;
        unsigned long out_size;
        unsigned long in_size;
} ide_task_request_t;

typedef struct ide_ioctl_request_s {
        ide_task_request_t *task_request;
        unsigned char *out_buffer;
        unsigned char *in_buffer;
} ide_ioctl_request_t;

struct hd_drive_cmd_hdr {
        task_ioreg_t command;
        task_ioreg_t sector_number;
        task_ioreg_t feature;
        task_ioreg_t sector_count;
};

typedef struct hd_drive_task_hdr {
        task_ioreg_t data;
        task_ioreg_t feature;
        task_ioreg_t sector_count;
        task_ioreg_t sector_number;
        task_ioreg_t low_cylinder;
        task_ioreg_t high_cylinder;
        task_ioreg_t device_head;
        task_ioreg_t command;
} task_struct_t;

typedef struct hd_drive_hob_hdr {
        task_ioreg_t data;
        task_ioreg_t feature;
        task_ioreg_t sector_count;
        task_ioreg_t sector_number;
        task_ioreg_t low_cylinder;
        task_ioreg_t high_cylinder;
        task_ioreg_t device_head;
        task_ioreg_t control;
} hob_struct_t;
# 373 "include/linux/hdreg.h"
struct hd_geometry {
      unsigned char heads;
      unsigned char sectors;
      unsigned short cylinders;
      unsigned long start;
};
# 425 "include/linux/hdreg.h"
enum {
        BUSSTATE_OFF = 0,
        BUSSTATE_ON,
        BUSSTATE_TRISTATE
};
# 445 "include/linux/hdreg.h"
struct hd_driveid {
        unsigned short config;
        unsigned short cyls;
        unsigned short reserved2;
        unsigned short heads;
        unsigned short track_bytes;
        unsigned short sector_bytes;
        unsigned short sectors;
        unsigned short vendor0;
        unsigned short vendor1;
        unsigned short vendor2;
        unsigned char serial_no[20];
        unsigned short buf_type;
        unsigned short buf_size;


        unsigned short ecc_bytes;
        unsigned char fw_rev[8];
        unsigned char model[40];
        unsigned char max_multsect;
        unsigned char vendor3;
        unsigned short dword_io;
        unsigned char vendor4;
        unsigned char capability;





        unsigned short reserved50;
        unsigned char vendor5;
        unsigned char tPIO;
        unsigned char vendor6;
        unsigned char tDMA;
        unsigned short field_valid;




        unsigned short cur_cyls;
        unsigned short cur_heads;
        unsigned short cur_sectors;
        unsigned short cur_capacity0;
        unsigned short cur_capacity1;
        unsigned char multsect;
        unsigned char multsect_valid;
        unsigned int lba_capacity;
        unsigned short dma_1word;
        unsigned short dma_mword;
        unsigned short eide_pio_modes;
        unsigned short eide_dma_min;
        unsigned short eide_dma_time;
        unsigned short eide_pio;
        unsigned short eide_pio_iordy;
        unsigned short words69_70[2];



        unsigned short words71_74[4];


        unsigned short queue_depth;



        unsigned short words76_79[4];
        unsigned short major_rev_num;
        unsigned short minor_rev_num;
        unsigned short command_set_1;
# 531 "include/linux/hdreg.h"
        unsigned short command_set_2;
# 549 "include/linux/hdreg.h"
        unsigned short cfsse;
# 561 "include/linux/hdreg.h"
        unsigned short cfs_enable_1;
# 580 "include/linux/hdreg.h"
        unsigned short cfs_enable_2;
# 599 "include/linux/hdreg.h"
        unsigned short csf_default;
# 611 "include/linux/hdreg.h"
        unsigned short dma_ultra;
        unsigned short trseuc;
        unsigned short trsEuc;
        unsigned short CurAPMvalues;
        unsigned short mprc;
        unsigned short hw_config;
# 634 "include/linux/hdreg.h"
        unsigned short acoustic;



        unsigned short msrqs;
        unsigned short sxfert;
        unsigned short sal;
        unsigned int spg;
        unsigned long long lba_capacity_2;
        unsigned short words104_125[22];
        unsigned short last_lun;
        unsigned short word127;







        unsigned short dlf;
# 665 "include/linux/hdreg.h"
        unsigned short csfo;







        unsigned short words130_155[26];
        unsigned short word156;
        unsigned short words157_159[3];
        unsigned short cfa_power;






        unsigned short words161_175[15];
        unsigned short words176_205[30];
        unsigned short words206_254[49];
        unsigned short integrity_word;



};
# 13 "include/linux/ide.h" 2
# 1 "include/linux/hdsmart.h" 1
# 27 "include/linux/hdsmart.h"
typedef struct ata_smart_attribute_s {
        unsigned char id;
        unsigned short status_flag;
        unsigned char normalized;
        unsigned char worse_normal;
        unsigned char raw[6];
        unsigned char reserv;
} __attribute__ ((packed)) ata_smart_attribute_t;


typedef struct ata_smart_values_s {
        unsigned short revnumber;
        ata_smart_attribute_t vendor_attributes [30];
        unsigned char offline_data_collection_status;
        unsigned char self_test_exec_status;
        unsigned short total_time_to_complete_off_line;
        unsigned char vendor_specific_366;
        unsigned char offline_data_collection_capability;
        unsigned short smart_capability;
        unsigned char errorlog_capability;
        unsigned char vendor_specific_371;
        unsigned char short_test_completion_time;
        unsigned char extend_test_completion_time;
        unsigned char reserved_374_385 [12];
        unsigned char vendor_specific_386_509 [125];
        unsigned char chksum;
} __attribute__ ((packed)) ata_smart_values_t;



typedef struct ata_smart_threshold_entry_s {
        unsigned char id;
        unsigned char normalized_threshold;
        unsigned char reserved[10];
} __attribute__ ((packed)) ata_smart_threshold_entry_t;


typedef struct ata_smart_thresholds_s {
        unsigned short revnumber;
        ata_smart_threshold_entry_t thres_entries[30];
        unsigned char reserved[149];
        unsigned char chksum;
} __attribute__ ((packed)) ata_smart_thresholds_t;

typedef struct ata_smart_errorlog_command_struct_s {
        unsigned char devicecontrolreg;
        unsigned char featuresreg;
        unsigned char sector_count;
        unsigned char sector_number;
        unsigned char cylinder_low;
        unsigned char cylinder_high;
        unsigned char drive_head;
        unsigned char commandreg;
        unsigned int timestamp;
} __attribute__ ((packed)) ata_smart_errorlog_command_struct_t;

typedef struct ata_smart_errorlog_error_struct_s {
        unsigned char error_condition;
        unsigned char extended_error[14];
        unsigned char state;
        unsigned short timestamp;
} __attribute__ ((packed)) ata_smart_errorlog_error_struct_t;

typedef struct ata_smart_errorlog_struct_s {
        ata_smart_errorlog_command_struct_t commands[6];
        ata_smart_errorlog_error_struct_t error_struct;
} __attribute__ ((packed)) ata_smart_errorlog_struct_t;

typedef struct ata_smart_errorlog_s {
        unsigned char revnumber;
        unsigned char error_log_pointer;
        ata_smart_errorlog_struct_t errorlog_struct[5];
        unsigned short ata_error_count;
        unsigned short non_fatal_count;
        unsigned short drive_timeout_count;
        unsigned char reserved[53];
        unsigned char chksum;
} __attribute__ ((packed)) ata_smart_errorlog_t;

typedef struct ata_smart_selftestlog_struct_s {
        unsigned char selftestnumber;
        unsigned char selfteststatus;
        unsigned short timestamp;
        unsigned char selftestfailurecheckpoint;
        unsigned int lbafirstfailure;
        unsigned char vendorspecific[15];
} __attribute__ ((packed)) ata_smart_selftestlog_struct_t;

typedef struct ata_smart_selftestlog_s {
        unsigned short revnumber;
        ata_smart_selftestlog_struct_t selftest_struct[21];
        unsigned char vendorspecific[2];
        unsigned char mostrecenttest;
        unsigned char resevered[2];
        unsigned char chksum;
} __attribute__ ((packed)) ata_smart_selftestlog_t;
# 14 "include/linux/ide.h" 2
# 1 "include/linux/blkdev.h" 1
# 10 "include/linux/blkdev.h"
# 1 "include/linux/pagemap.h" 1
# 10 "include/linux/pagemap.h"
# 1 "include/linux/highmem.h" 1







# 1 "include/asm/cacheflush.h" 1
# 26 "include/asm/cacheflush.h"
void global_flush_tlb(void);
int change_page_attr(struct page *page, int numpages, pgprot_t prot);
# 9 "include/linux/highmem.h" 2



# 1 "include/asm/highmem.h" 1
# 27 "include/asm/highmem.h"
# 1 "include/asm/tlbflush.h" 1
# 40 "include/asm/tlbflush.h"
extern unsigned long pgkern_mask;
# 115 "include/asm/tlbflush.h"
extern void flush_tlb_all(void);
extern void flush_tlb_current_task(void);
extern void flush_tlb_mm(struct mm_struct *);
extern void flush_tlb_page(struct vm_area_struct *, unsigned long);



static inline __attribute__((always_inline)) void flush_tlb_range(struct vm_area_struct * vma, unsigned long start, unsigned long end)
{
        flush_tlb_mm(vma->vm_mm);
}




struct tlb_state
{
        struct mm_struct *active_mm;
        int state;
        char __cacheline_padding[(1 << (4))-8];
};
extern __typeof__(struct tlb_state) per_cpu__cpu_tlbstate;






static inline __attribute__((always_inline)) void flush_tlb_pgtables(struct mm_struct *mm,
                                      unsigned long start, unsigned long end)
{

}
# 28 "include/asm/highmem.h" 2


extern unsigned long highstart_pfn, highend_pfn;

extern pte_t *kmap_pte;
extern pgprot_t kmap_prot;
extern pte_t *pkmap_page_table;
# 66 "include/asm/highmem.h"
extern void * kmap_high(struct page *page) __attribute__((regparm(3)));
extern void kunmap_high(struct page *page) __attribute__((regparm(3)));

void *kmap(struct page *page);
void kunmap(struct page *page);
void *kmap_atomic(struct page *page, enum km_type type);
void kunmap_atomic(void *kvaddr, enum km_type type);
void *kmap_atomic_pfn(unsigned long pfn, enum km_type type);
struct page *kmap_atomic_to_page(void *ptr);
# 13 "include/linux/highmem.h" 2


unsigned int nr_free_highpages(void);
# 37 "include/linux/highmem.h"
static inline __attribute__((always_inline)) void clear_user_highpage(struct page *page, unsigned long vaddr)
{
        void *addr = kmap_atomic(page, KM_USER0);
        (__builtin_constant_p(0) ? (__builtin_constant_p(((1UL << 12))) ? __constant_c_and_count_memset((((void *)(addr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12)))) : __constant_c_memset((((void *)(addr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12))))) : (__builtin_constant_p(((1UL << 12))) ? __memset_generic(((((void *)(addr)))),(((0))),((((1UL << 12))))) : __memset_generic((((void *)(addr))),((0)),(((1UL << 12))))));
        kunmap_atomic(addr, KM_USER0);

        __asm__ __volatile__ ("": : :"memory");
}
# 59 "include/linux/highmem.h"
static inline __attribute__((always_inline)) void clear_highpage(struct page *page)
{
        void *kaddr = kmap_atomic(page, KM_USER0);
        (__builtin_constant_p(0) ? (__builtin_constant_p(((1UL << 12))) ? __constant_c_and_count_memset((((void *)(kaddr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12)))) : __constant_c_memset((((void *)(kaddr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12))))) : (__builtin_constant_p(((1UL << 12))) ? __memset_generic(((((void *)(kaddr)))),(((0))),((((1UL << 12))))) : __memset_generic((((void *)(kaddr))),((0)),(((1UL << 12))))));
        kunmap_atomic(kaddr, KM_USER0);
}




static inline __attribute__((always_inline)) void memclear_highpage_flush(struct page *page, unsigned int offset, unsigned int size)
{
        void *kaddr;

        do { if (__builtin_expect(!!((offset + size > (1UL << 12))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (73), "i" ("include/linux/highmem.h")); } while(0);

        kaddr = kmap_atomic(page, KM_USER0);
        (__builtin_constant_p(0) ? (__builtin_constant_p((size)) ? __constant_c_and_count_memset((((char *)kaddr + offset)),((0x01010101UL*(unsigned char)(0))),((size))) : __constant_c_memset((((char *)kaddr + offset)),((0x01010101UL*(unsigned char)(0))),((size)))) : (__builtin_constant_p((size)) ? __memset_generic(((((char *)kaddr + offset))),(((0))),(((size)))) : __memset_generic((((char *)kaddr + offset)),((0)),((size)))));
        do { } while (0);
        kunmap_atomic(kaddr, KM_USER0);
}

static inline __attribute__((always_inline)) void copy_user_highpage(struct page *to, struct page *from, unsigned long vaddr)
{
        char *vfrom, *vto;

        vfrom = kmap_atomic(from, KM_USER0);
        vto = kmap_atomic(to, KM_USER1);
        (__builtin_constant_p((1UL << 12)) ? __constant_memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))) : __memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))));
        kunmap_atomic(vfrom, KM_USER0);
        kunmap_atomic(vto, KM_USER1);

        __asm__ __volatile__ ("": : :"memory");
}

static inline __attribute__((always_inline)) void copy_highpage(struct page *to, struct page *from)
{
        char *vfrom, *vto;

        vfrom = kmap_atomic(from, KM_USER0);
        vto = kmap_atomic(to, KM_USER1);
        (__builtin_constant_p((1UL << 12)) ? __constant_memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))) : __memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))));
        kunmap_atomic(vfrom, KM_USER0);
        kunmap_atomic(vto, KM_USER1);
}
# 11 "include/linux/pagemap.h" 2

# 1 "include/asm/uaccess.h" 1
# 99 "include/asm/uaccess.h"
struct exception_table_entry
{
        unsigned long insn, fixup;
};

extern int fixup_exception(struct pt_regs *regs);
# 121 "include/asm/uaccess.h"
extern void __get_user_1(void);
extern void __get_user_2(void);
extern void __get_user_4(void);
# 163 "include/asm/uaccess.h"
extern void __put_user_bad(void);





extern void __put_user_1(void);
extern void __put_user_2(void);
extern void __put_user_4(void);
extern void __put_user_8(void);
# 324 "include/asm/uaccess.h"
struct __large_struct { unsigned long buf[100]; };
# 357 "include/asm/uaccess.h"
extern long __get_user_bad(void);
# 388 "include/asm/uaccess.h"
unsigned long __copy_to_user_ll(void *to,
                                const void *from, unsigned long n);
unsigned long __copy_from_user_ll(void *to,
                                const void *from, unsigned long n);
# 414 "include/asm/uaccess.h"
static inline __attribute__((always_inline)) unsigned long
__copy_to_user_inatomic(void *to, const void *from, unsigned long n)
{
        if (__builtin_constant_p(n)) {
                unsigned long ret;

                switch (n) {
                case 1:
                        do { __typeof__(*((u8 *)to)) __pus_tmp = *(u8 *)from; ret = 0; if(__builtin_expect(!!(__copy_to_user_ll((u8 *)to, &__pus_tmp, 1) != 0), 0)) ret = 1; } while (0);
                        return ret;
                case 2:
                        do { __typeof__(*((u16 *)to)) __pus_tmp = *(u16 *)from; ret = 0; if(__builtin_expect(!!(__copy_to_user_ll((u16 *)to, &__pus_tmp, 2) != 0), 0)) ret = 2; } while (0);
                        return ret;
                case 4:
                        do { __typeof__(*((u32 *)to)) __pus_tmp = *(u32 *)from; ret = 0; if(__builtin_expect(!!(__copy_to_user_ll((u32 *)to, &__pus_tmp, 4) != 0), 0)) ret = 4; } while (0);
                        return ret;
                }
        }
        return __copy_to_user_ll(to, from, n);
}

static inline __attribute__((always_inline)) unsigned long
__copy_to_user(void *to, const void *from, unsigned long n)
{
       do { do { } while (0); } while (0);
       return __copy_to_user_inatomic(to, from, n);
}
# 459 "include/asm/uaccess.h"
static inline __attribute__((always_inline)) unsigned long
__copy_from_user_inatomic(void *to, const void *from, unsigned long n)
{
        if (__builtin_constant_p(n)) {
                unsigned long ret;

                switch (n) {
                case 1:
                        do { ret = 0; (void)0; switch (1) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; default: (*(u8 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                case 2:
                        do { ret = 0; (void)0; switch (2) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; default: (*(u16 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                case 4:
                        do { ret = 0; (void)0; switch (4) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; default: (*(u32 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                }
        }
        return __copy_from_user_ll(to, from, n);
}

static inline __attribute__((always_inline)) unsigned long
__copy_from_user(void *to, const void *from, unsigned long n)
{
       do { do { } while (0); } while (0);
       return __copy_from_user_inatomic(to, from, n);
}
unsigned long copy_to_user(void *to,
                                const void *from, unsigned long n);
unsigned long copy_from_user(void *to,
                                const void *from, unsigned long n);
long strncpy_from_user(char *dst, const char *src,
                                long count);
long __strncpy_from_user(char *dst,
                                const char *src, long count);
# 511 "include/asm/uaccess.h"
long strnlen_user(const char *str, long n);
unsigned long clear_user(void *mem, unsigned long len);
unsigned long __clear_user(void *mem, unsigned long len);
# 13 "include/linux/pagemap.h" 2
# 22 "include/linux/pagemap.h"
static inline __attribute__((always_inline)) gfp_t mapping_gfp_mask(struct address_space * mapping)
{
        return mapping->flags & ((1 << 20) - 1);
}





static inline __attribute__((always_inline)) void mapping_set_gfp_mask(struct address_space *m, int mask)
{
        m->flags = (m->flags & ~((1 << 20) - 1)) | mask;
}
# 51 "include/linux/pagemap.h"
void release_pages(struct page **pages, int nr, int cold);

static inline __attribute__((always_inline)) struct page *page_cache_alloc(struct address_space *x)
{
        return alloc_pages_node(((0)), mapping_gfp_mask(x)|0x20000u, 0);
}

static inline __attribute__((always_inline)) struct page *page_cache_alloc_cold(struct address_space *x)
{
        return alloc_pages_node(((0)), mapping_gfp_mask(x)|0x100u|0x20000u, 0);
}

typedef int filler_t(void *, struct page *);

extern struct page * find_get_page(struct address_space *mapping,
                                unsigned long index);
extern struct page * find_lock_page(struct address_space *mapping,
                                unsigned long index);
extern struct page * find_trylock_page(struct address_space *mapping,
                                unsigned long index);
extern struct page * find_or_create_page(struct address_space *mapping,
                                unsigned long index, unsigned int gfp_mask);
unsigned find_get_pages(struct address_space *mapping, unsigned long start,
                        unsigned int nr_pages, struct page **pages);
unsigned find_get_pages_tag(struct address_space *mapping, unsigned long *index,
                        int tag, unsigned int nr_pages, struct page **pages);




static inline __attribute__((always_inline)) struct page *grab_cache_page(struct address_space *mapping, unsigned long index)
{
        return find_or_create_page(mapping, index, mapping_gfp_mask(mapping));
}

extern struct page * grab_cache_page_nowait(struct address_space *mapping,
                                unsigned long index);
extern struct page * read_cache_page(struct address_space *mapping,
                                unsigned long index, filler_t *filler,
                                void *data);
extern int read_cache_pages(struct address_space *mapping,
                struct list_head *pages, filler_t *filler, void *data);

int add_to_page_cache(struct page *page, struct address_space *mapping,
                                unsigned long index, int gfp_mask);
int add_to_page_cache_lru(struct page *page, struct address_space *mapping,
                                unsigned long index, int gfp_mask);
extern void remove_from_page_cache(struct page *page);
extern void __remove_from_page_cache(struct page *page);

extern atomic_t nr_pagecache;




extern __typeof__(long) per_cpu__nr_pagecache_local;
# 118 "include/linux/pagemap.h"
static inline __attribute__((always_inline)) void pagecache_acct(int count)
{
        long *local;

        local = &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__nr_pagecache_local)); (typeof(&per_cpu__nr_pagecache_local)) (__ptr + (__per_cpu_offset[(current_thread_info()->cpu)])); }));
        *local += count;
        if (*local > ({ typeof(16) _x = (16); typeof(8 * 2) _y = (8 * 2); (void) (&_x == &_y); _x > _y ? _x : _y; }) || *local < -({ typeof(16) _x = (16); typeof(8 * 2) _y = (8 * 2); (void) (&_x == &_y); _x > _y ? _x : _y; })) {
                atomic_add(*local, &nr_pagecache);
                *local = 0;
        }
}
# 138 "include/linux/pagemap.h"
static inline __attribute__((always_inline)) unsigned long get_page_cache_size(void)
{
        int ret = ((&nr_pagecache)->counter);
        if (__builtin_expect(!!(ret < 0), 0))
                ret = 0;
        return ret;
}




static inline __attribute__((always_inline)) loff_t page_offset(struct page *page)
{
        return ((loff_t)page->index) << 12;
}

static inline __attribute__((always_inline)) unsigned long linear_page_index(struct vm_area_struct *vma,
                                        unsigned long address)
{
        unsigned long pgoff = (address - vma->vm_start) >> 12;
        pgoff += vma->vm_pgoff;
        return pgoff >> (12 - 12);
}

extern void __lock_page(struct page *page) __attribute__((regparm(3)));
extern void unlock_page(struct page *page) __attribute__((regparm(3)));

static inline __attribute__((always_inline)) void lock_page(struct page *page)
{
        do { do { } while (0); } while (0);
        if (test_and_set_bit(0, &(page)->flags))
                __lock_page(page);
}





extern void wait_on_page_bit(struct page *page, int bit_nr) __attribute__((regparm(3)));
# 185 "include/linux/pagemap.h"
static inline __attribute__((always_inline)) void wait_on_page_locked(struct page *page)
{
        if ((__builtin_constant_p(0) ? constant_test_bit((0),(&(page)->flags)) : variable_test_bit((0),(&(page)->flags))))
                wait_on_page_bit(page, 0);
}




static inline __attribute__((always_inline)) void wait_on_page_writeback(struct page *page)
{
        if ((__builtin_constant_p(12) ? constant_test_bit((12),(&(page)->flags)) : variable_test_bit((12),(&(page)->flags))))
                wait_on_page_bit(page, 12);
}

extern void end_page_writeback(struct page *page);







static inline __attribute__((always_inline)) int fault_in_pages_writeable(char *uaddr, int size)
{
        int ret;





        ret = ({ long __pu_err; do { __typeof__(*(((uaddr)))) __pus_tmp = ((__typeof__(*(uaddr)))(0)); __pu_err = 0; if(__builtin_expect(!!(__copy_to_user_ll(((uaddr)), &__pus_tmp, (sizeof(*(uaddr)))) != 0), 0)) __pu_err = -14; } while (0); __pu_err; });
        if (ret == 0) {
                char *end = uaddr + size - 1;





                if (((unsigned long)uaddr & (~((1UL << 12)-1))) !=
                                ((unsigned long)end & (~((1UL << 12)-1))))
                        ret = ({ long __pu_err; do { __typeof__(*(((end)))) __pus_tmp = ((__typeof__(*(end)))(0)); __pu_err = 0; if(__builtin_expect(!!(__copy_to_user_ll(((end)), &__pus_tmp, (sizeof(*(end)))) != 0), 0)) __pu_err = -14; } while (0); __pu_err; });
        }
        return ret;
}

static inline __attribute__((always_inline)) void fault_in_pages_readable(const char *uaddr, int size)
{
        volatile char c;
        int ret;

        ret = ({ long __gu_err; unsigned long __gu_val; do { __gu_err = 0; (void)0; switch ((sizeof(*(uaddr)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=q" (__gu_val) : "m"((*(struct __large_struct *)(((uaddr))))), "i"(-14), "0"(__gu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)(((uaddr))))), "i"(-14), "0"(__gu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)(((uaddr))))), "i"(-14), "0"(__gu_err));break; default: (__gu_val) = __get_user_bad(); } } while (0); ((c)) = (__typeof__(*((uaddr))))__gu_val; __gu_err; });
        if (ret == 0) {
                const char *end = uaddr + size - 1;

                if (((unsigned long)uaddr & (~((1UL << 12)-1))) !=
                                ((unsigned long)end & (~((1UL << 12)-1))))
                        ({ long __gu_err; unsigned long __gu_val; do { __gu_err = 0; (void)0; switch ((sizeof(*(end)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=q" (__gu_val) : "m"((*(struct __large_struct *)(((end))))), "i"(-14), "0"(__gu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)(((end))))), "i"(-14), "0"(__gu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__gu_err), "=r" (__gu_val) : "m"((*(struct __large_struct *)(((end))))), "i"(-14), "0"(__gu_err));break; default: (__gu_val) = __get_user_bad(); } } while (0); ((c)) = (__typeof__(*((end))))__gu_val; __gu_err; });
        }
}
# 11 "include/linux/blkdev.h" 2
# 1 "include/linux/backing-dev.h" 1
# 16 "include/linux/backing-dev.h"
enum bdi_state {
        BDI_pdflush,
        BDI_write_congested,
        BDI_read_congested,
        BDI_unused,
};

typedef int (congested_fn)(void *, int);

struct backing_dev_info {
        unsigned long ra_pages;
        unsigned long state;
        unsigned int capabilities;
        congested_fn *congested_fn;
        void *congested_data;
        void (*unplug_io_fn)(struct backing_dev_info *, struct page *);
        void *unplug_io_data;
};
# 61 "include/linux/backing-dev.h"
extern struct backing_dev_info default_backing_dev_info;
void default_unplug_io_fn(struct backing_dev_info *bdi, struct page *page);

int writeback_acquire(struct backing_dev_info *bdi);
int writeback_in_progress(struct backing_dev_info *bdi);
void writeback_release(struct backing_dev_info *bdi);

static inline __attribute__((always_inline)) int bdi_congested(struct backing_dev_info *bdi, int bdi_bits)
{
        if (bdi->congested_fn)
                return bdi->congested_fn(bdi->congested_data, bdi_bits);
        return (bdi->state & bdi_bits);
}

static inline __attribute__((always_inline)) int bdi_read_congested(struct backing_dev_info *bdi)
{
        return bdi_congested(bdi, 1 << BDI_read_congested);
}

static inline __attribute__((always_inline)) int bdi_write_congested(struct backing_dev_info *bdi)
{
        return bdi_congested(bdi, 1 << BDI_write_congested);
}

static inline __attribute__((always_inline)) int bdi_rw_congested(struct backing_dev_info *bdi)
{
        return bdi_congested(bdi, (1 << BDI_read_congested)|
                                  (1 << BDI_write_congested));
}
# 12 "include/linux/blkdev.h" 2

# 1 "include/linux/mempool.h" 1
# 9 "include/linux/mempool.h"
typedef void * (mempool_alloc_t)(gfp_t gfp_mask, void *pool_data);
typedef void (mempool_free_t)(void *element, void *pool_data);

typedef struct mempool_s {
        spinlock_t lock;
        int min_nr;
        int curr_nr;
        void **elements;

        void *pool_data;
        mempool_alloc_t *alloc;
        mempool_free_t *free;
        wait_queue_head_t wait;
} mempool_t;

extern mempool_t *mempool_create(int min_nr, mempool_alloc_t *alloc_fn,
                        mempool_free_t *free_fn, void *pool_data);
extern mempool_t *mempool_create_node(int min_nr, mempool_alloc_t *alloc_fn,
                        mempool_free_t *free_fn, void *pool_data, int nid);

extern int mempool_resize(mempool_t *pool, int new_min_nr, gfp_t gfp_mask);
extern void mempool_destroy(mempool_t *pool);
extern void * mempool_alloc(mempool_t *pool, gfp_t gfp_mask);
extern void mempool_free(void *element, mempool_t *pool);





void *mempool_alloc_slab(gfp_t gfp_mask, void *pool_data);
void mempool_free_slab(void *element, void *pool_data);
# 14 "include/linux/blkdev.h" 2
# 1 "include/linux/bio.h" 1
# 25 "include/linux/bio.h"
# 1 "include/linux/ioprio.h" 1
# 25 "include/linux/ioprio.h"
enum {
        IOPRIO_CLASS_NONE,
        IOPRIO_CLASS_RT,
        IOPRIO_CLASS_BE,
        IOPRIO_CLASS_IDLE,
};






enum {
        IOPRIO_WHO_PROCESS = 1,
        IOPRIO_WHO_PGRP,
        IOPRIO_WHO_USER,
};






static inline __attribute__((always_inline)) int task_ioprio(struct task_struct *task)
{
        do { if (__builtin_expect(!!((!((((task->ioprio)) >> (13)) != IOPRIO_CLASS_NONE))!=0), 0)) { printk("Badness in %s at %s:%d\n", (__func__), "include/linux/ioprio.h", 50); dump_stack(); } } while (0);
        return ((task->ioprio) & ((1UL << (13)) - 1));
}

static inline __attribute__((always_inline)) int task_nice_ioprio(struct task_struct *task)
{
        return (task_nice(task) + 20) / 5;
}




static inline __attribute__((always_inline)) int ioprio_best(unsigned short aprio, unsigned short bprio)
{
        unsigned short aclass = ((aprio) >> (13));
        unsigned short bclass = ((bprio) >> (13));

        if (!((((aprio)) >> (13)) != IOPRIO_CLASS_NONE))
                return bprio;
        if (!((((bprio)) >> (13)) != IOPRIO_CLASS_NONE))
                return aprio;

        if (aclass == IOPRIO_CLASS_NONE)
                aclass = IOPRIO_CLASS_BE;
        if (bclass == IOPRIO_CLASS_NONE)
                bclass = IOPRIO_CLASS_BE;

        if (aclass == bclass)
                return ({ typeof(aprio) _x = (aprio); typeof(bprio) _y = (bprio); (void) (&_x == &_y); _x < _y ? _x : _y; });
        if (aclass > bclass)
                return bprio;
        else
                return aprio;
}
# 26 "include/linux/bio.h" 2


# 1 "include/asm/io.h" 1
# 48 "include/asm/io.h"
# 1 "include/asm-generic/iomap.h" 1
# 28 "include/asm-generic/iomap.h"
extern unsigned int __attribute__((regparm(3))) ioread8(void *);
extern unsigned int __attribute__((regparm(3))) ioread16(void *);
extern unsigned int __attribute__((regparm(3))) ioread16be(void *);
extern unsigned int __attribute__((regparm(3))) ioread32(void *);
extern unsigned int __attribute__((regparm(3))) ioread32be(void *);

extern void __attribute__((regparm(3))) iowrite8(u8, void *);
extern void __attribute__((regparm(3))) iowrite16(u16, void *);
extern void __attribute__((regparm(3))) iowrite16be(u16, void *);
extern void __attribute__((regparm(3))) iowrite32(u32, void *);
extern void __attribute__((regparm(3))) iowrite32be(u32, void *);
# 51 "include/asm-generic/iomap.h"
extern void __attribute__((regparm(3))) ioread8_rep(void *port, void *buf, unsigned long count);
extern void __attribute__((regparm(3))) ioread16_rep(void *port, void *buf, unsigned long count);
extern void __attribute__((regparm(3))) ioread32_rep(void *port, void *buf, unsigned long count);

extern void __attribute__((regparm(3))) iowrite8_rep(void *port, const void *buf, unsigned long count);
extern void __attribute__((regparm(3))) iowrite16_rep(void *port, const void *buf, unsigned long count);
extern void __attribute__((regparm(3))) iowrite32_rep(void *port, const void *buf, unsigned long count);


extern void *ioport_map(unsigned long port, unsigned int nr);
extern void ioport_unmap(void *);


struct pci_dev;
extern void *pci_iomap(struct pci_dev *dev, int bar, unsigned long max);
extern void pci_iounmap(struct pci_dev *dev, void *);
# 49 "include/asm/io.h" 2

# 1 "include/linux/vmalloc.h" 1
# 21 "include/linux/vmalloc.h"
struct vm_struct {
        void *addr;
        unsigned long size;
        unsigned long flags;
        struct page **pages;
        unsigned int nr_pages;
        unsigned long phys_addr;
        struct vm_struct *next;
};




extern void *vmalloc(unsigned long size);
extern void *vmalloc_exec(unsigned long size);
extern void *vmalloc_32(unsigned long size);
extern void *__vmalloc(unsigned long size, gfp_t gfp_mask, pgprot_t prot);
extern void *__vmalloc_area(struct vm_struct *area, gfp_t gfp_mask, pgprot_t prot);
extern void vfree(void *addr);

extern void *vmap(struct page **pages, unsigned int count,
                        unsigned long flags, pgprot_t prot);
extern void vunmap(void *addr);




extern struct vm_struct *get_vm_area(unsigned long size, unsigned long flags);
extern struct vm_struct *__get_vm_area(unsigned long size, unsigned long flags,
                                        unsigned long start, unsigned long end);
extern struct vm_struct *remove_vm_area(void *addr);
extern struct vm_struct *__remove_vm_area(void *addr);
extern int map_vm_area(struct vm_struct *area, pgprot_t prot,
                        struct page ***pages);
extern void unmap_vm_area(struct vm_struct *area);




extern rwlock_t vmlist_lock;
extern struct vm_struct *vmlist;
# 51 "include/asm/io.h" 2
# 76 "include/asm/io.h"
static inline __attribute__((always_inline)) unsigned long virt_to_phys(volatile void * address)
{
        return ((unsigned long)(address)-((unsigned long)(0xC0000000UL)));
}
# 94 "include/asm/io.h"
static inline __attribute__((always_inline)) void * phys_to_virt(unsigned long address)
{
        return ((void *)((unsigned long)(address)+((unsigned long)(0xC0000000UL))));
}






extern void * __ioremap(unsigned long offset, unsigned long size, unsigned long flags);
# 118 "include/asm/io.h"
static inline __attribute__((always_inline)) void * ioremap(unsigned long offset, unsigned long size)
{
        return __ioremap(offset, size, 0);
}

extern void * ioremap_nocache(unsigned long offset, unsigned long size);
extern void iounmap(volatile void *addr);






extern void *bt_ioremap(unsigned long offset, unsigned long size);
extern void bt_iounmap(void *addr, unsigned long size);
# 157 "include/asm/io.h"
static inline __attribute__((always_inline)) unsigned char readb(const volatile void *addr)
{
        return *(volatile unsigned char *) addr;
}
static inline __attribute__((always_inline)) unsigned short readw(const volatile void *addr)
{
        return *(volatile unsigned short *) addr;
}
static inline __attribute__((always_inline)) unsigned int readl(const volatile void *addr)
{
        return *(volatile unsigned int *) addr;
}







static inline __attribute__((always_inline)) void writeb(unsigned char b, volatile void *addr)
{
        *(volatile unsigned char *) addr = b;
}
static inline __attribute__((always_inline)) void writew(unsigned short b, volatile void *addr)
{
        *(volatile unsigned short *) addr = b;
}
static inline __attribute__((always_inline)) void writel(unsigned int b, volatile void *addr)
{
        *(volatile unsigned int *) addr = b;
}






static inline __attribute__((always_inline)) void memset_io(volatile void *addr, unsigned char val, int count)
{
        (__builtin_constant_p(val) ? (__builtin_constant_p((count)) ? __constant_c_and_count_memset((((void *) addr)),((0x01010101UL*(unsigned char)(val))),((count))) : __constant_c_memset((((void *) addr)),((0x01010101UL*(unsigned char)(val))),((count)))) : (__builtin_constant_p((count)) ? __memset_generic(((((void *) addr))),(((val))),(((count)))) : __memset_generic((((void *) addr)),((val)),((count)))));
}
static inline __attribute__((always_inline)) void memcpy_fromio(void *dst, const volatile void *src, int count)
{
        __memcpy(dst, (void *) src, count);
}
static inline __attribute__((always_inline)) void memcpy_toio(volatile void *dst, const void *src, int count)
{
        __memcpy((void *) dst, src, count);
}
# 246 "include/asm/io.h"
static inline __attribute__((always_inline)) int check_signature(volatile void * io_addr,
        const unsigned char *signature, int length)
{
        int retval = 0;
        do {
                if (readb(io_addr) != *signature)
                        goto out;
                io_addr++;
                signature++;
                length--;
        } while (length);
        retval = 1;
out:
        return retval;
}
# 272 "include/asm/io.h"
static inline __attribute__((always_inline)) void flush_write_buffers(void)
{
        __asm__ __volatile__ ("lock; addl $0,0(%%esp)": : :"memory");
}
# 300 "include/asm/io.h"
static inline __attribute__((always_inline)) void slow_down_io(void) {
        __asm__ __volatile__(
                "outb %%al,$0x80;"



                : : );
}
# 377 "include/asm/io.h"
static inline __attribute__((always_inline)) void outb_local(unsigned char value, int port) { __asm__ __volatile__("out" "b" " %" "b" "0, %w1" : : "a"(value), "Nd"(port)); } static inline __attribute__((always_inline)) unsigned char inb_local(int port) { unsigned char value; __asm__ __volatile__("in" "b" " %w1, %" "b" "0" : "=a"(value) : "Nd"(port)); return value; } static inline __attribute__((always_inline)) void outb_local_p(unsigned char value, int port) { outb_local(value, port); slow_down_io(); } static inline __attribute__((always_inline)) unsigned char inb_local_p(int port) { unsigned char value = inb_local(port); slow_down_io(); return value; } static inline __attribute__((always_inline)) void outb(unsigned char value, int port) { outb_local(value, port); } static inline __attribute__((always_inline)) unsigned char inb(int port) { return inb_local(port); } static inline __attribute__((always_inline)) void outb_p(unsigned char value, int port) { outb(value, port); slow_down_io(); } static inline __attribute__((always_inline)) unsigned char inb_p(int port) { unsigned char value = inb(port); slow_down_io(); return value; } static inline __attribute__((always_inline)) void outsb(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "b" : "+S"(addr), "+c"(count) : "d"(port)); } static inline __attribute__((always_inline)) void insb(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "b" : "+D"(addr), "+c"(count) : "d"(port)); }
static inline __attribute__((always_inline)) void outw_local(unsigned short value, int port) { __asm__ __volatile__("out" "w" " %" "w" "0, %w1" : : "a"(value), "Nd"(port)); } static inline __attribute__((always_inline)) unsigned short inw_local(int port) { unsigned short value; __asm__ __volatile__("in" "w" " %w1, %" "w" "0" : "=a"(value) : "Nd"(port)); return value; } static inline __attribute__((always_inline)) void outw_local_p(unsigned short value, int port) { outw_local(value, port); slow_down_io(); } static inline __attribute__((always_inline)) unsigned short inw_local_p(int port) { unsigned short value = inw_local(port); slow_down_io(); return value; } static inline __attribute__((always_inline)) void outw(unsigned short value, int port) { outw_local(value, port); } static inline __attribute__((always_inline)) unsigned short inw(int port) { return inw_local(port); } static inline __attribute__((always_inline)) void outw_p(unsigned short value, int port) { outw(value, port); slow_down_io(); } static inline __attribute__((always_inline)) unsigned short inw_p(int port) { unsigned short value = inw(port); slow_down_io(); return value; } static inline __attribute__((always_inline)) void outsw(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "w" : "+S"(addr), "+c"(count) : "d"(port)); } static inline __attribute__((always_inline)) void insw(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "w" : "+D"(addr), "+c"(count) : "d"(port)); }
static inline __attribute__((always_inline)) void outl_local(unsigned int value, int port) { __asm__ __volatile__("out" "l" " %" "" "0, %w1" : : "a"(value), "Nd"(port)); } static inline __attribute__((always_inline)) unsigned int inl_local(int port) { unsigned int value; __asm__ __volatile__("in" "l" " %w1, %" "" "0" : "=a"(value) : "Nd"(port)); return value; } static inline __attribute__((always_inline)) void outl_local_p(unsigned int value, int port) { outl_local(value, port); slow_down_io(); } static inline __attribute__((always_inline)) unsigned int inl_local_p(int port) { unsigned int value = inl_local(port); slow_down_io(); return value; } static inline __attribute__((always_inline)) void outl(unsigned int value, int port) { outl_local(value, port); } static inline __attribute__((always_inline)) unsigned int inl(int port) { return inl_local(port); } static inline __attribute__((always_inline)) void outl_p(unsigned int value, int port) { outl(value, port); slow_down_io(); } static inline __attribute__((always_inline)) unsigned int inl_p(int port) { unsigned int value = inl(port); slow_down_io(); return value; } static inline __attribute__((always_inline)) void outsl(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "l" : "+S"(addr), "+c"(count) : "d"(port)); } static inline __attribute__((always_inline)) void insl(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "l" : "+D"(addr), "+c"(count) : "d"(port)); }
# 29 "include/linux/bio.h" 2
# 57 "include/linux/bio.h"
struct bio_vec {
        struct page *bv_page;
        unsigned int bv_len;
        unsigned int bv_offset;
};

struct bio_set;
struct bio;
typedef int (bio_end_io_t) (struct bio *, unsigned int, int);
typedef void (bio_destructor_t) (struct bio *);





struct bio {
        sector_t bi_sector;
        struct bio *bi_next;
        struct block_device *bi_bdev;
        unsigned long bi_flags;
        unsigned long bi_rw;



        unsigned short bi_vcnt;
        unsigned short bi_idx;




        unsigned short bi_phys_segments;




        unsigned short bi_hw_segments;

        unsigned int bi_size;






        unsigned int bi_hw_front_size;
        unsigned int bi_hw_back_size;

        unsigned int bi_max_vecs;

        struct bio_vec *bi_io_vec;

        bio_end_io_t *bi_end_io;
        atomic_t bi_cnt;

        void *bi_private;

        bio_destructor_t *bi_destructor;
};
# 265 "include/linux/bio.h"
struct bio_pair {
        struct bio bio1, bio2;
        struct bio_vec bv1, bv2;
        atomic_t cnt;
        int error;
};
extern struct bio_pair *bio_split(struct bio *bi, mempool_t *pool,
                                  int first_sectors);
extern mempool_t *bio_split_pool;
extern void bio_pair_release(struct bio_pair *dbio);

extern struct bio_set *bioset_create(int, int, int);
extern void bioset_free(struct bio_set *);

extern struct bio *bio_alloc(gfp_t, int);
extern struct bio *bio_alloc_bioset(gfp_t, int, struct bio_set *);
extern void bio_put(struct bio *);
extern void bio_free(struct bio *, struct bio_set *);

extern void bio_endio(struct bio *, unsigned int, int);
struct request_queue;
extern int bio_phys_segments(struct request_queue *, struct bio *);
extern int bio_hw_segments(struct request_queue *, struct bio *);

extern void __bio_clone(struct bio *, struct bio *);
extern struct bio *bio_clone(struct bio *, gfp_t);

extern void bio_init(struct bio *);

extern int bio_add_page(struct bio *, struct page *, unsigned int,unsigned int);
extern int bio_get_nr_vecs(struct block_device *);
extern struct bio *bio_map_user(struct request_queue *, struct block_device *,
                                unsigned long, unsigned int, int);
struct sg_iovec;
extern struct bio *bio_map_user_iov(struct request_queue *,
                                    struct block_device *,
                                    struct sg_iovec *, int, int);
extern void bio_unmap_user(struct bio *);
extern struct bio *bio_map_kern(struct request_queue *, void *, unsigned int,
                                unsigned int);
extern void bio_set_pages_dirty(struct bio *bio);
extern void bio_check_pages_dirty(struct bio *bio);
extern struct bio *bio_copy_user(struct request_queue *, unsigned long, unsigned int, int);
extern int bio_uncopy_user(struct bio *);
void zero_fill_bio(struct bio *bio);
# 318 "include/linux/bio.h"
static inline __attribute__((always_inline)) char *bvec_kmap_irq(struct bio_vec *bvec, unsigned long *flags)
{
        unsigned long addr;





        __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (*flags): :"memory");
        addr = (unsigned long) kmap_atomic(bvec->bv_page, KM_BIO_SRC_IRQ);

        do { if (__builtin_expect(!!((addr & ~(~((1UL << 12)-1)))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (329), "i" ("include/linux/bio.h")); } while(0);

        return (char *) addr + bvec->bv_offset;
}

static inline __attribute__((always_inline)) void bvec_kunmap_irq(char *buffer, unsigned long *flags)
{
        unsigned long ptr = (unsigned long) buffer & (~((1UL << 12)-1));

        kunmap_atomic((void *) ptr, KM_BIO_SRC_IRQ);
        do { ({ unsigned long __dummy; typeof(*flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); __asm__ __volatile__("pushl %0 ; popfl": :"g" (*flags):"memory", "cc"); } while (0);
}






static inline __attribute__((always_inline)) char *__bio_kmap_irq(struct bio *bio, unsigned short idx,
                                   unsigned long *flags)
{
        return bvec_kmap_irq((&((bio)->bi_io_vec[(idx)])), flags);
}
# 15 "include/linux/blkdev.h" 2



# 1 "include/asm/scatterlist.h" 1



struct scatterlist {
    struct page *page;
    unsigned int offset;
    dma_addr_t dma_address;
    unsigned int length;
};
# 19 "include/linux/blkdev.h" 2

struct request_queue;
typedef struct request_queue request_queue_t;
struct elevator_queue;
typedef struct elevator_queue elevator_t;
struct request_pm_state;







struct as_io_context {
        spinlock_t lock;

        void (*dtor)(struct as_io_context *aic);
        void (*exit)(struct as_io_context *aic);

        unsigned long state;
        atomic_t nr_queued;
        atomic_t nr_dispatched;



        unsigned long last_end_request;
        unsigned long ttime_total;
        unsigned long ttime_samples;
        unsigned long ttime_mean;

        unsigned int seek_samples;
        sector_t last_request_pos;
        u64 seek_total;
        sector_t seek_mean;
};

struct cfq_queue;
struct cfq_io_context {



        struct list_head list;
        struct cfq_queue *cfqq;
        void *key;

        struct io_context *ioc;

        unsigned long last_end_request;
        unsigned long last_queue;
        unsigned long ttime_total;
        unsigned long ttime_samples;
        unsigned long ttime_mean;

        void (*dtor)(struct cfq_io_context *);
        void (*exit)(struct cfq_io_context *);
};






struct io_context {
        atomic_t refcount;
        struct task_struct *task;

        int (*set_ioprio)(struct io_context *, unsigned int);




        unsigned long last_waited;
        int nr_batch_requests;

        struct as_io_context *aic;
        struct cfq_io_context *cic;
};

void put_io_context(struct io_context *ioc);
void exit_io_context(void);
struct io_context *current_io_context(int gfp_flags);
struct io_context *get_io_context(int gfp_flags);
void copy_io_context(struct io_context **pdst, struct io_context **psrc);
void swap_io_context(struct io_context **ioc1, struct io_context **ioc2);

struct request;
typedef void (rq_end_io_fn)(struct request *);

struct request_list {
        int count[2];
        int starved[2];
        mempool_t *rq_pool;
        wait_queue_head_t wait[2];
        wait_queue_head_t drain;
};






struct request {
        struct list_head queuelist;


        unsigned long flags;





        sector_t sector;
        unsigned long nr_sectors;

        unsigned int current_nr_sectors;

        sector_t hard_sector;
        unsigned long hard_nr_sectors;

        unsigned int hard_cur_sectors;

        struct bio *bio;
        struct bio *biotail;

        void *elevator_private;

        unsigned short ioprio;

        int rq_status;
        struct gendisk *rq_disk;
        int errors;
        unsigned long start_time;




        unsigned short nr_phys_segments;






        unsigned short nr_hw_segments;

        int tag;
        char *buffer;

        int ref_count;
        request_queue_t *q;
        struct request_list *rl;

        struct completion *waiting;
        void *special;




        unsigned int cmd_len;
        unsigned char cmd[16];

        unsigned int data_len;
        void *data;

        unsigned int sense_len;
        void *sense;

        unsigned int timeout;




        struct request_pm_state *pm;




        rq_end_io_fn *end_io;
        void *end_io_data;
};




enum rq_flag_bits {
        __REQ_RW,
        __REQ_FAILFAST,
        __REQ_SOFTBARRIER,
        __REQ_HARDBARRIER,
        __REQ_CMD,
        __REQ_NOMERGE,
        __REQ_STARTED,
        __REQ_DONTPREP,
        __REQ_QUEUED,



        __REQ_PC,
        __REQ_BLOCK_PC,
        __REQ_SENSE,

        __REQ_FAILED,
        __REQ_QUIET,
        __REQ_SPECIAL,
        __REQ_DRIVE_CMD,
        __REQ_DRIVE_TASK,
        __REQ_DRIVE_TASKFILE,
        __REQ_PREEMPT,
        __REQ_PM_SUSPEND,
        __REQ_PM_RESUME,
        __REQ_PM_SHUTDOWN,
        __REQ_BAR_PREFLUSH,
        __REQ_BAR_POSTFLUSH,
        __REQ_BAR_FLUSH,
        __REQ_NR_BITS,
};
# 266 "include/linux/blkdev.h"
struct request_pm_state
{

        int pm_step;

        u32 pm_state;
        void* data;
};

# 1 "include/linux/elevator.h" 1



typedef int (elevator_merge_fn) (request_queue_t *, struct request **,
                                 struct bio *);

typedef void (elevator_merge_req_fn) (request_queue_t *, struct request *, struct request *);

typedef void (elevator_merged_fn) (request_queue_t *, struct request *);

typedef struct request *(elevator_next_req_fn) (request_queue_t *);

typedef void (elevator_add_req_fn) (request_queue_t *, struct request *, int);
typedef int (elevator_queue_empty_fn) (request_queue_t *);
typedef void (elevator_remove_req_fn) (request_queue_t *, struct request *);
typedef void (elevator_requeue_req_fn) (request_queue_t *, struct request *);
typedef struct request *(elevator_request_list_fn) (request_queue_t *, struct request *);
typedef void (elevator_completed_req_fn) (request_queue_t *, struct request *);
typedef int (elevator_may_queue_fn) (request_queue_t *, int, struct bio *);

typedef int (elevator_set_req_fn) (request_queue_t *, struct request *, struct bio *, int);
typedef void (elevator_put_req_fn) (request_queue_t *, struct request *);
typedef void (elevator_deactivate_req_fn) (request_queue_t *, struct request *);

typedef int (elevator_init_fn) (request_queue_t *, elevator_t *);
typedef void (elevator_exit_fn) (elevator_t *);

struct elevator_ops
{
        elevator_merge_fn *elevator_merge_fn;
        elevator_merged_fn *elevator_merged_fn;
        elevator_merge_req_fn *elevator_merge_req_fn;

        elevator_next_req_fn *elevator_next_req_fn;
        elevator_add_req_fn *elevator_add_req_fn;
        elevator_remove_req_fn *elevator_remove_req_fn;
        elevator_requeue_req_fn *elevator_requeue_req_fn;
        elevator_deactivate_req_fn *elevator_deactivate_req_fn;

        elevator_queue_empty_fn *elevator_queue_empty_fn;
        elevator_completed_req_fn *elevator_completed_req_fn;

        elevator_request_list_fn *elevator_former_req_fn;
        elevator_request_list_fn *elevator_latter_req_fn;

        elevator_set_req_fn *elevator_set_req_fn;
        elevator_put_req_fn *elevator_put_req_fn;

        elevator_may_queue_fn *elevator_may_queue_fn;

        elevator_init_fn *elevator_init_fn;
        elevator_exit_fn *elevator_exit_fn;
};






struct elevator_type
{
        struct list_head list;
        struct elevator_ops ops;
        struct elevator_type *elevator_type;
        struct kobj_type *elevator_ktype;
        char elevator_name[(16)];
        struct module *elevator_owner;
};




struct elevator_queue
{
        struct elevator_ops *ops;
        void *elevator_data;
        struct kobject kobj;
        struct elevator_type *elevator_type;
};




extern void elv_add_request(request_queue_t *, struct request *, int, int);
extern void __elv_add_request(request_queue_t *, struct request *, int, int);
extern int elv_merge(request_queue_t *, struct request **, struct bio *);
extern void elv_merge_requests(request_queue_t *, struct request *,
                               struct request *);
extern void elv_merged_request(request_queue_t *, struct request *);
extern void elv_remove_request(request_queue_t *, struct request *);
extern void elv_requeue_request(request_queue_t *, struct request *);
extern void elv_deactivate_request(request_queue_t *, struct request *);
extern int elv_queue_empty(request_queue_t *);
extern struct request *elv_next_request(struct request_queue *q);
extern struct request *elv_former_request(request_queue_t *, struct request *);
extern struct request *elv_latter_request(request_queue_t *, struct request *);
extern int elv_register_queue(request_queue_t *q);
extern void elv_unregister_queue(request_queue_t *q);
extern int elv_may_queue(request_queue_t *, int, struct bio *);
extern void elv_completed_request(request_queue_t *, struct request *);
extern int elv_set_request(request_queue_t *, struct request *, struct bio *, int);
extern void elv_put_request(request_queue_t *, struct request *);




extern int elv_register(struct elevator_type *);
extern void elv_unregister(struct elevator_type *);




extern ssize_t elv_iosched_show(request_queue_t *, char *);
extern ssize_t elv_iosched_store(request_queue_t *, const char *, size_t);

extern int elevator_init(request_queue_t *, char *);
extern void elevator_exit(elevator_t *);
extern int elv_rq_merge_ok(struct request *, struct bio *);
extern int elv_try_merge(struct request *, struct bio *);
extern int elv_try_last_merge(request_queue_t *, struct bio *);
# 139 "include/linux/elevator.h"
enum {
        ELV_MQUEUE_MAY,
        ELV_MQUEUE_NO,
        ELV_MQUEUE_MUST,
};
# 276 "include/linux/blkdev.h" 2

typedef int (merge_request_fn) (request_queue_t *, struct request *,
                                struct bio *);
typedef int (merge_requests_fn) (request_queue_t *, struct request *,
                                 struct request *);
typedef void (request_fn_proc) (request_queue_t *q);
typedef int (make_request_fn) (request_queue_t *q, struct bio *bio);
typedef int (prep_rq_fn) (request_queue_t *, struct request *);
typedef void (unplug_fn) (request_queue_t *);

struct bio_vec;
typedef int (merge_bvec_fn) (request_queue_t *, struct bio *, struct bio_vec *);
typedef void (activity_fn) (void *data, int rw);
typedef int (issue_flush_fn) (request_queue_t *, struct gendisk *, sector_t *);
typedef int (prepare_flush_fn) (request_queue_t *, struct request *);
typedef void (end_flush_fn) (request_queue_t *, struct request *);

enum blk_queue_state {
        Queue_down,
        Queue_up,
};

struct blk_queue_tag {
        struct request **tag_index;
        unsigned long *tag_map;
        struct list_head busy_list;
        int busy;
        int max_depth;
        int real_max_depth;
        atomic_t refcnt;
};

struct request_queue
{



        struct list_head queue_head;
        struct request *last_merge;
        elevator_t *elevator;




        struct request_list rq;

        request_fn_proc *request_fn;
        merge_request_fn *back_merge_fn;
        merge_request_fn *front_merge_fn;
        merge_requests_fn *merge_requests_fn;
        make_request_fn *make_request_fn;
        prep_rq_fn *prep_rq_fn;
        unplug_fn *unplug_fn;
        merge_bvec_fn *merge_bvec_fn;
        activity_fn *activity_fn;
        issue_flush_fn *issue_flush_fn;
        prepare_flush_fn *prepare_flush_fn;
        end_flush_fn *end_flush_fn;




        struct timer_list unplug_timer;
        int unplug_thresh;
        unsigned long unplug_delay;
        struct work_struct unplug_work;

        struct backing_dev_info backing_dev_info;





        void *queuedata;

        void *activity_data;




        unsigned long bounce_pfn;
        unsigned int bounce_gfp;




        unsigned long queue_flags;






        spinlock_t __queue_lock;
        spinlock_t *queue_lock;




        struct kobject kobj;




        unsigned long nr_requests;
        unsigned int nr_congestion_on;
        unsigned int nr_congestion_off;
        unsigned int nr_batching;

        unsigned short max_sectors;
        unsigned short max_hw_sectors;
        unsigned short max_phys_segments;
        unsigned short max_hw_segments;
        unsigned short hardsect_size;
        unsigned int max_segment_size;

        unsigned long seg_boundary_mask;
        unsigned int dma_alignment;

        struct blk_queue_tag *queue_tags;

        atomic_t refcnt;

        unsigned int in_flight;




        unsigned int sg_timeout;
        unsigned int sg_reserved_size;
        int node;

        struct list_head drain_list;




        struct request *flush_rq;
        unsigned char ordered;
};

enum {
        QUEUE_ORDERED_NONE,
        QUEUE_ORDERED_TAG,
        QUEUE_ORDERED_FLUSH,
};
# 465 "include/linux/blkdev.h"
static inline __attribute__((always_inline)) int blk_queue_full(struct request_queue *q, int rw)
{
        if (rw == 0)
                return (__builtin_constant_p(3) ? constant_test_bit((3),(&q->queue_flags)) : variable_test_bit((3),(&q->queue_flags)));
        return (__builtin_constant_p(4) ? constant_test_bit((4),(&q->queue_flags)) : variable_test_bit((4),(&q->queue_flags)));
}

static inline __attribute__((always_inline)) void blk_set_queue_full(struct request_queue *q, int rw)
{
        if (rw == 0)
                set_bit(3, &q->queue_flags);
        else
                set_bit(4, &q->queue_flags);
}

static inline __attribute__((always_inline)) void blk_clear_queue_full(struct request_queue *q, int rw)
{
        if (rw == 0)
                clear_bit(3, &q->queue_flags);
        else
                clear_bit(4, &q->queue_flags);
}
# 511 "include/linux/blkdev.h"
extern unsigned long blk_max_low_pfn, blk_max_pfn;
# 525 "include/linux/blkdev.h"
extern int init_emergency_isa_pool(void);
extern void blk_queue_bounce(request_queue_t *q, struct bio **bio);
# 541 "include/linux/blkdev.h"
struct sec_size {
        unsigned block_size;
        unsigned block_size_bits;
};

extern int blk_register_queue(struct gendisk *disk);
extern void blk_unregister_queue(struct gendisk *disk);
extern void register_disk(struct gendisk *dev);
extern void generic_make_request(struct bio *bio);
extern void blk_put_request(struct request *);
extern void blk_end_sync_rq(struct request *rq);
extern void blk_attempt_remerge(request_queue_t *, struct request *);
extern struct request *blk_get_request(request_queue_t *, int, int);
extern void blk_insert_request(request_queue_t *, struct request *, int, void *);
extern void blk_requeue_request(request_queue_t *, struct request *);
extern void blk_plug_device(request_queue_t *);
extern int blk_remove_plug(request_queue_t *);
extern void blk_recount_segments(request_queue_t *, struct bio *);
extern int scsi_cmd_ioctl(struct file *, struct gendisk *, unsigned int, void *);
extern void blk_start_queue(request_queue_t *q);
extern void blk_stop_queue(request_queue_t *q);
extern void blk_sync_queue(struct request_queue *q);
extern void __blk_stop_queue(request_queue_t *q);
extern void blk_run_queue(request_queue_t *);
extern void blk_queue_activity_fn(request_queue_t *, activity_fn *, void *);
extern int blk_rq_map_user(request_queue_t *, struct request *, void *, unsigned int);
extern int blk_rq_unmap_user(struct bio *, unsigned int);
extern int blk_rq_map_kern(request_queue_t *, struct request *, void *, unsigned int, unsigned int);
extern int blk_rq_map_user_iov(request_queue_t *, struct request *, struct sg_iovec *, int);
extern int blk_execute_rq(request_queue_t *, struct gendisk *,
                          struct request *, int);
static inline __attribute__((always_inline)) request_queue_t *bdev_get_queue(struct block_device *bdev)
{
        return bdev->bd_disk->queue;
}

static inline __attribute__((always_inline)) void blk_run_backing_dev(struct backing_dev_info *bdi,
                                       struct page *page)
{
        if (bdi && bdi->unplug_io_fn)
                bdi->unplug_io_fn(bdi, page);
}

static inline __attribute__((always_inline)) void blk_run_address_space(struct address_space *mapping)
{
        if (mapping)
                blk_run_backing_dev(mapping->backing_dev_info, ((void *)0));
}
# 599 "include/linux/blkdev.h"
extern int end_that_request_first(struct request *, int, int);
extern int end_that_request_chunk(struct request *, int, int);
extern void end_that_request_last(struct request *);
extern void end_request(struct request *req, int uptodate);
# 612 "include/linux/blkdev.h"
static inline __attribute__((always_inline)) void blkdev_dequeue_request(struct request *req)
{
        do { if (__builtin_expect(!!((list_empty(&req->queuelist))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (614), "i" ("include/linux/blkdev.h")); } while(0);

        list_del_init(&req->queuelist);

        if (req->rl)
                elv_remove_request(req->q, req);
}




extern request_queue_t *blk_init_queue_node(request_fn_proc *rfn,
                                        spinlock_t *lock, int node_id);
extern request_queue_t *blk_init_queue(request_fn_proc *, spinlock_t *);
extern void blk_cleanup_queue(request_queue_t *);
extern void blk_queue_make_request(request_queue_t *, make_request_fn *);
extern void blk_queue_bounce_limit(request_queue_t *, u64);
extern void blk_queue_max_sectors(request_queue_t *, unsigned short);
extern void blk_queue_max_phys_segments(request_queue_t *, unsigned short);
extern void blk_queue_max_hw_segments(request_queue_t *, unsigned short);
extern void blk_queue_max_segment_size(request_queue_t *, unsigned int);
extern void blk_queue_hardsect_size(request_queue_t *, unsigned short);
extern void blk_queue_stack_limits(request_queue_t *t, request_queue_t *b);
extern void blk_queue_segment_boundary(request_queue_t *, unsigned long);
extern void blk_queue_prep_rq(request_queue_t *, prep_rq_fn *pfn);
extern void blk_queue_merge_bvec(request_queue_t *, merge_bvec_fn *);
extern void blk_queue_dma_alignment(request_queue_t *, int);
extern struct backing_dev_info *blk_get_backing_dev_info(struct block_device *bdev);
extern void blk_queue_ordered(request_queue_t *, int);
extern void blk_queue_issue_flush_fn(request_queue_t *, issue_flush_fn *);
extern struct request *blk_start_pre_flush(request_queue_t *,struct request *);
extern int blk_complete_barrier_rq(request_queue_t *, struct request *, int);
extern int blk_complete_barrier_rq_locked(request_queue_t *, struct request *, int);

extern int blk_rq_map_sg(request_queue_t *, struct request *, struct scatterlist *);
extern void blk_dump_rq_flags(struct request *, char *);
extern void generic_unplug_device(request_queue_t *);
extern void __generic_unplug_device(request_queue_t *);
extern long nr_blockdev_pages(void);
extern void blk_wait_queue_drained(request_queue_t *, int);
extern void blk_finish_queue_drain(request_queue_t *);

int blk_get_queue(request_queue_t *);
request_queue_t *blk_alloc_queue(int gfp_mask);
request_queue_t *blk_alloc_queue_node(int,int);
# 667 "include/linux/blkdev.h"
extern int blk_queue_start_tag(request_queue_t *, struct request *);
extern struct request *blk_queue_find_tag(request_queue_t *, int);
extern void blk_queue_end_tag(request_queue_t *, struct request *);
extern int blk_queue_init_tags(request_queue_t *, int, struct blk_queue_tag *);
extern void blk_queue_free_tags(request_queue_t *);
extern int blk_queue_resize_tags(request_queue_t *, int);
extern void blk_queue_invalidate_tags(request_queue_t *);
extern long blk_congestion_wait(int rw, long timeout);

extern void blk_rq_bio_prep(request_queue_t *, struct request *, struct bio *);
extern int blkdev_issue_flush(struct block_device *, sector_t *);
# 687 "include/linux/blkdev.h"
static inline __attribute__((always_inline)) int queue_hardsect_size(request_queue_t *q)
{
        int retval = 512;

        if (q && q->hardsect_size)
                retval = q->hardsect_size;

        return retval;
}

static inline __attribute__((always_inline)) int bdev_hardsect_size(struct block_device *bdev)
{
        return queue_hardsect_size(bdev_get_queue(bdev));
}

static inline __attribute__((always_inline)) int queue_dma_alignment(request_queue_t *q)
{
        int retval = 511;

        if (q && q->dma_alignment)
                retval = q->dma_alignment;

        return retval;
}

static inline __attribute__((always_inline)) int bdev_dma_aligment(struct block_device *bdev)
{
        return queue_dma_alignment(bdev_get_queue(bdev));
}





static inline __attribute__((always_inline)) unsigned int blksize_bits(unsigned int size)
{
        unsigned int bits = 8;
        do {
                bits++;
                size >>= 1;
        } while (size > 256);
        return bits;
}

static inline __attribute__((always_inline)) unsigned int block_size(struct block_device *bdev)
{
        return bdev->bd_block_size;
}

typedef struct {struct page *v;} Sector;

unsigned char *read_dev_sector(struct block_device *, sector_t, Sector *);

static inline __attribute__((always_inline)) void put_dev_sector(Sector p)
{
        put_page(p.v);
}

struct work_struct;
int kblockd_schedule_work(struct work_struct *work);
void kblockd_flush(void);
# 15 "include/linux/ide.h" 2
# 1 "include/linux/proc_fs.h" 1
# 23 "include/linux/proc_fs.h"
enum {
        PROC_ROOT_INO = 1,
};
# 44 "include/linux/proc_fs.h"
typedef int (read_proc_t)(char *page, char **start, off_t off,
                          int count, int *eof, void *data);
typedef int (write_proc_t)(struct file *file, const char *buffer,
                           unsigned long count, void *data);
typedef int (get_info_t)(char *, char **, off_t, int);

struct proc_dir_entry {
        unsigned int low_ino;
        unsigned short namelen;
        const char *name;
        mode_t mode;
        nlink_t nlink;
        uid_t uid;
        gid_t gid;
        unsigned long size;
        struct inode_operations * proc_iops;
        struct file_operations * proc_fops;
        get_info_t *get_info;
        struct module *owner;
        struct proc_dir_entry *next, *parent, *subdir;
        void *data;
        read_proc_t *read_proc;
        write_proc_t *write_proc;
        atomic_t count;
        int deleted;
};

struct kcore_list {
        struct kcore_list *next;
        unsigned long addr;
        size_t size;
};

struct vmcore {
        struct list_head list;
        unsigned long long paddr;
        unsigned long size;
        loff_t offset;
};



extern struct proc_dir_entry proc_root;
extern struct proc_dir_entry *proc_root_fs;
extern struct proc_dir_entry *proc_net;
extern struct proc_dir_entry *proc_net_stat;
extern struct proc_dir_entry *proc_bus;
extern struct proc_dir_entry *proc_root_driver;
extern struct proc_dir_entry *proc_root_kcore;

extern void proc_root_init(void);
extern void proc_misc_init(void);

struct mm_struct;

struct dentry *proc_pid_lookup(struct inode *dir, struct dentry * dentry, struct nameidata *);
struct dentry *proc_pid_unhash(struct task_struct *p);
void proc_pid_flush(struct dentry *proc_dentry);
int proc_pid_readdir(struct file * filp, void * dirent, filldir_t filldir);
unsigned long task_vsize(struct mm_struct *);
int task_statm(struct mm_struct *, int *, int *, int *, int *);
char *task_mem(struct mm_struct *, char *);

extern struct proc_dir_entry *create_proc_entry(const char *name, mode_t mode,
                                                struct proc_dir_entry *parent);
extern void remove_proc_entry(const char *name, struct proc_dir_entry *parent);

extern struct vfsmount *proc_mnt;
extern int proc_fill_super(struct super_block *,void *,int);
extern struct inode *proc_get_inode(struct super_block *, unsigned int, struct proc_dir_entry *);

extern int proc_match(int, const char *,struct proc_dir_entry *);
# 124 "include/linux/proc_fs.h"
extern int proc_readdir(struct file *, void *, filldir_t);
extern struct dentry *proc_lookup(struct inode *, struct dentry *, struct nameidata *);

extern struct file_operations proc_kcore_operations;
extern struct file_operations proc_kmsg_operations;
extern struct file_operations ppc_htab_operations;




struct tty_driver;
extern void proc_tty_init(void);
extern void proc_tty_register_driver(struct tty_driver *driver);
extern void proc_tty_unregister_driver(struct tty_driver *driver);




struct device_node;
extern void proc_device_tree_init(void);



static inline __attribute__((always_inline)) void proc_device_tree_add_node(struct device_node *np, struct proc_dir_entry *pde)
{
        return;
}


extern struct proc_dir_entry *proc_symlink(const char *,
                struct proc_dir_entry *, const char *);
extern struct proc_dir_entry *proc_mkdir(const char *,struct proc_dir_entry *);
extern struct proc_dir_entry *proc_mkdir_mode(const char *name, mode_t mode,
                        struct proc_dir_entry *parent);

static inline __attribute__((always_inline)) struct proc_dir_entry *create_proc_read_entry(const char *name,
        mode_t mode, struct proc_dir_entry *base,
        read_proc_t *read_proc, void * data)
{
        struct proc_dir_entry *res=create_proc_entry(name,mode,base);
        if (res) {
                res->read_proc=read_proc;
                res->data=data;
        }
        return res;
}

static inline __attribute__((always_inline)) struct proc_dir_entry *create_proc_info_entry(const char *name,
        mode_t mode, struct proc_dir_entry *base, get_info_t *get_info)
{
        struct proc_dir_entry *res=create_proc_entry(name,mode,base);
        if (res) res->get_info=get_info;
        return res;
}

static inline __attribute__((always_inline)) struct proc_dir_entry *proc_net_create(const char *name,
        mode_t mode, get_info_t *get_info)
{
        return create_proc_info_entry(name,mode,proc_net,get_info);
}

static inline __attribute__((always_inline)) struct proc_dir_entry *proc_net_fops_create(const char *name,
        mode_t mode, struct file_operations *fops)
{
        struct proc_dir_entry *res = create_proc_entry(name, mode, proc_net);
        if (res)
                res->proc_fops = fops;
        return res;
}

static inline __attribute__((always_inline)) void proc_net_remove(const char *name)
{
        remove_proc_entry(name,proc_net);
}
# 238 "include/linux/proc_fs.h"
static inline __attribute__((always_inline)) void kclist_add(struct kcore_list *new, void *addr, size_t size)
{
}




struct proc_inode {
        struct task_struct *task;
        int type;
        union {
                int (*proc_get_link)(struct inode *, struct dentry **, struct vfsmount **);
                int (*proc_read)(struct task_struct *task, char *page);
        } op;
        struct proc_dir_entry *pde;
        struct inode vfs_inode;
};

static inline __attribute__((always_inline)) struct proc_inode *PROC_I(const struct inode *inode)
{
        return ({ const typeof( ((struct proc_inode *)0)->vfs_inode ) *__mptr = (inode); (struct proc_inode *)( (char *)__mptr - ((size_t) &((struct proc_inode *)0)->vfs_inode) );});
}

static inline __attribute__((always_inline)) struct proc_dir_entry *PDE(const struct inode *inode)
{
        return PROC_I(inode)->pde;
}
# 16 "include/linux/ide.h" 2




# 1 "include/linux/pci.h" 1
# 20 "include/linux/pci.h"
# 1 "include/linux/mod_devicetable.h" 1
# 12 "include/linux/mod_devicetable.h"
typedef unsigned long kernel_ulong_t;




struct pci_device_id {
        __u32 vendor, device;
        __u32 subvendor, subdevice;
        __u32 class, class_mask;
        kernel_ulong_t driver_data;
};







struct ieee1394_device_id {
        __u32 match_flags;
        __u32 vendor_id;
        __u32 model_id;
        __u32 specifier_id;
        __u32 version;
        kernel_ulong_t driver_data
                __attribute__((aligned(sizeof(kernel_ulong_t))));
};
# 98 "include/linux/mod_devicetable.h"
struct usb_device_id {

        __u16 match_flags;


        __u16 idVendor;
        __u16 idProduct;
        __u16 bcdDevice_lo;
        __u16 bcdDevice_hi;


        __u8 bDeviceClass;
        __u8 bDeviceSubClass;
        __u8 bDeviceProtocol;


        __u8 bInterfaceClass;
        __u8 bInterfaceSubClass;
        __u8 bInterfaceProtocol;


        kernel_ulong_t driver_info;
};
# 135 "include/linux/mod_devicetable.h"
struct ccw_device_id {
        __u16 match_flags;

        __u16 cu_type;
        __u16 dev_type;
        __u8 cu_model;
        __u8 dev_model;

        kernel_ulong_t driver_info;
};
# 155 "include/linux/mod_devicetable.h"
struct pnp_device_id {
        __u8 id[8];
        kernel_ulong_t driver_data;
};

struct pnp_card_device_id {
        __u8 id[8];
        kernel_ulong_t driver_data;
        struct {
                __u8 id[8];
        } devs[8];
};




struct serio_device_id {
        __u8 type;
        __u8 extra;
        __u8 id;
        __u8 proto;
};




struct of_device_id
{
        char name[32];
        char type[32];
        char compatible[128];

        void *data;



};


struct vio_device_id {
        char type[32];
        char compat[32];
};



struct pcmcia_device_id {
        __u16 match_flags;

        __u16 manf_id;
        __u16 card_id;

        __u8 func_id;


        __u8 function;


        __u8 device_no;

        __u32 prod_id_hash[4]
                __attribute__((aligned(sizeof(__u32))));



        const char * prod_id[4];






        kernel_ulong_t driver_info;

        char * cisfile;



};
# 21 "include/linux/pci.h" 2


# 1 "include/linux/pci_regs.h" 1
# 24 "include/linux/pci.h" 2


# 1 "include/linux/pci_ids.h" 1
# 27 "include/linux/pci.h" 2
# 57 "include/linux/pci.h"
enum pci_mmap_state {
        pci_mmap_io,
        pci_mmap_mem
};
# 71 "include/linux/pci.h"
typedef int pci_power_t;
# 84 "include/linux/pci.h"
struct pci_dev {
        struct list_head global_list;
        struct list_head bus_list;
        struct pci_bus *bus;
        struct pci_bus *subordinate;

        void *sysdata;
        struct proc_dir_entry *procent;

        unsigned int devfn;
        unsigned short vendor;
        unsigned short device;
        unsigned short subsystem_vendor;
        unsigned short subsystem_device;
        unsigned int class;
        u8 hdr_type;
        u8 rom_base_reg;

        struct pci_driver *driver;
        u64 dma_mask;





        pci_power_t current_state;



        struct device dev;


        unsigned short vendor_compatible[4];
        unsigned short device_compatible[4];

        int cfg_size;





        unsigned int irq;
        struct resource resource[12];


        unsigned int transparent:1;
        unsigned int multifunction:1;

        unsigned int is_enabled:1;
        unsigned int is_busmaster:1;
        unsigned int no_msi:1;

        u32 saved_config_space[16];
        struct bin_attribute *rom_attr;
        int rom_attr_enabled;
        struct bin_attribute *res_attr[12];
};
# 165 "include/linux/pci.h"
struct pci_bus {
        struct list_head node;
        struct pci_bus *parent;
        struct list_head children;
        struct list_head devices;
        struct pci_dev *self;
        struct resource *resource[8];


        struct pci_ops *ops;
        void *sysdata;
        struct proc_dir_entry *procdir;

        unsigned char number;
        unsigned char primary;
        unsigned char secondary;
        unsigned char subordinate;

        char name[48];

        unsigned short bridge_ctl;
        unsigned short pad2;
        struct device *bridge;
        struct class_device class_dev;
        struct bin_attribute *legacy_io;
        struct bin_attribute *legacy_mem;
};
# 209 "include/linux/pci.h"
struct pci_ops {
        int (*read)(struct pci_bus *bus, unsigned int devfn, int where, int size, u32 *val);
        int (*write)(struct pci_bus *bus, unsigned int devfn, int where, int size, u32 val);
};

struct pci_raw_ops {
        int (*read)(unsigned int domain, unsigned int bus, unsigned int devfn,
                    int reg, int len, u32 *val);
        int (*write)(unsigned int domain, unsigned int bus, unsigned int devfn,
                     int reg, int len, u32 val);
};

extern struct pci_raw_ops *raw_pci_ops;

struct pci_bus_region {
        unsigned long start;
        unsigned long end;
};

struct pci_dynids {
        spinlock_t lock;
        struct list_head list;
        unsigned int use_driver_data:1;
};

struct module;
struct pci_driver {
        struct list_head node;
        char *name;
        struct module *owner;
        const struct pci_device_id *id_table;
        int (*probe) (struct pci_dev *dev, const struct pci_device_id *id);
        void (*remove) (struct pci_dev *dev);
        int (*suspend) (struct pci_dev *dev, pm_message_t state);
        int (*resume) (struct pci_dev *dev);
        int (*enable_wake) (struct pci_dev *dev, pci_power_t state, int enable);
        void (*shutdown) (struct pci_dev *dev);

        struct device_driver driver;
        struct pci_dynids dynids;
};
# 289 "include/linux/pci.h"
extern struct bus_type pci_bus_type;



extern struct list_head pci_root_buses;
extern struct list_head pci_devices;

void pcibios_fixup_bus(struct pci_bus *);
int pcibios_enable_device(struct pci_dev *, int mask);
char *pcibios_setup (char *str);


void pcibios_align_resource(void *, struct resource *,
                            unsigned long, unsigned long);
void pcibios_update_irq(struct pci_dev *, int irq);



extern struct pci_bus *pci_find_bus(int domain, int busnr);
void pci_bus_add_devices(struct pci_bus *bus);
struct pci_bus *pci_scan_bus_parented(struct device *parent, int bus, struct pci_ops *ops, void *sysdata);
static inline __attribute__((always_inline)) struct pci_bus *pci_scan_bus(int bus, struct pci_ops *ops, void *sysdata)
{
        struct pci_bus *root_bus;
        root_bus = pci_scan_bus_parented(((void *)0), bus, ops, sysdata);
        if (root_bus)
                pci_bus_add_devices(root_bus);
        return root_bus;
}
struct pci_bus *pci_create_bus(struct device *parent, int bus, struct pci_ops *ops, void *sysdata);
struct pci_bus * pci_add_new_bus(struct pci_bus *parent, struct pci_dev *dev, int busnr);
int pci_scan_slot(struct pci_bus *bus, int devfn);
struct pci_dev * pci_scan_single_device(struct pci_bus *bus, int devfn);
void pci_device_add(struct pci_dev *dev, struct pci_bus *bus);
unsigned int pci_scan_child_bus(struct pci_bus *bus);
void pci_bus_add_device(struct pci_dev *dev);
void pci_read_bridge_bases(struct pci_bus *child);
struct resource *pci_find_parent_resource(const struct pci_dev *dev, struct resource *res);
int pci_get_interrupt_pin(struct pci_dev *dev, struct pci_dev **bridge);
extern struct pci_dev *pci_dev_get(struct pci_dev *dev);
extern void pci_dev_put(struct pci_dev *dev);
extern void pci_remove_bus(struct pci_bus *b);
extern void pci_remove_bus_device(struct pci_dev *dev);
void pci_setup_cardbus(struct pci_bus *bus);



struct pci_dev *pci_find_device (unsigned int vendor, unsigned int device, const struct pci_dev *from);
struct pci_dev *pci_find_device_reverse (unsigned int vendor, unsigned int device, const struct pci_dev *from);
struct pci_dev *pci_find_slot (unsigned int bus, unsigned int devfn);
int pci_find_capability (struct pci_dev *dev, int cap);
int pci_find_ext_capability (struct pci_dev *dev, int cap);
struct pci_bus * pci_find_next_bus(const struct pci_bus *from);

struct pci_dev *pci_get_device (unsigned int vendor, unsigned int device, struct pci_dev *from);
struct pci_dev *pci_get_subsys (unsigned int vendor, unsigned int device,
                                unsigned int ss_vendor, unsigned int ss_device,
                                struct pci_dev *from);
struct pci_dev *pci_get_slot (struct pci_bus *bus, unsigned int devfn);
struct pci_dev *pci_get_class (unsigned int class, struct pci_dev *from);
int pci_dev_present(const struct pci_device_id *ids);

int pci_bus_read_config_byte (struct pci_bus *bus, unsigned int devfn, int where, u8 *val);
int pci_bus_read_config_word (struct pci_bus *bus, unsigned int devfn, int where, u16 *val);
int pci_bus_read_config_dword (struct pci_bus *bus, unsigned int devfn, int where, u32 *val);
int pci_bus_write_config_byte (struct pci_bus *bus, unsigned int devfn, int where, u8 val);
int pci_bus_write_config_word (struct pci_bus *bus, unsigned int devfn, int where, u16 val);
int pci_bus_write_config_dword (struct pci_bus *bus, unsigned int devfn, int where, u32 val);

static inline __attribute__((always_inline)) int pci_read_config_byte(struct pci_dev *dev, int where, u8 *val)
{
        return pci_bus_read_config_byte (dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_read_config_word(struct pci_dev *dev, int where, u16 *val)
{
        return pci_bus_read_config_word (dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_read_config_dword(struct pci_dev *dev, int where, u32 *val)
{
        return pci_bus_read_config_dword (dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_write_config_byte(struct pci_dev *dev, int where, u8 val)
{
        return pci_bus_write_config_byte (dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_write_config_word(struct pci_dev *dev, int where, u16 val)
{
        return pci_bus_write_config_word (dev->bus, dev->devfn, where, val);
}
static inline __attribute__((always_inline)) int pci_write_config_dword(struct pci_dev *dev, int where, u32 val)
{
        return pci_bus_write_config_dword (dev->bus, dev->devfn, where, val);
}

int pci_enable_device(struct pci_dev *dev);
int pci_enable_device_bars(struct pci_dev *dev, int mask);
void pci_disable_device(struct pci_dev *dev);
void pci_set_master(struct pci_dev *dev);

int pci_set_mwi(struct pci_dev *dev);
void pci_clear_mwi(struct pci_dev *dev);
void pci_intx(struct pci_dev *dev, int enable);
int pci_set_dma_mask(struct pci_dev *dev, u64 mask);
int pci_set_consistent_dma_mask(struct pci_dev *dev, u64 mask);
void pci_update_resource(struct pci_dev *dev, struct resource *res, int resno);
int pci_assign_resource(struct pci_dev *dev, int i);
void pci_restore_bars(struct pci_dev *dev);


void *pci_map_rom(struct pci_dev *pdev, size_t *size);
void *pci_map_rom_copy(struct pci_dev *pdev, size_t *size);
void pci_unmap_rom(struct pci_dev *pdev, void *rom);
void pci_remove_rom(struct pci_dev *pdev);


int pci_save_state(struct pci_dev *dev);
int pci_restore_state(struct pci_dev *dev);
int pci_set_power_state(struct pci_dev *dev, pci_power_t state);
pci_power_t pci_choose_state(struct pci_dev *dev, pm_message_t state);
int pci_enable_wake(struct pci_dev *dev, pci_power_t state, int enable);


void pci_bus_assign_resources(struct pci_bus *bus);
void pci_bus_size_bridges(struct pci_bus *bus);
int pci_claim_resource(struct pci_dev *, int);
void pci_assign_unassigned_resources(void);
void pdev_enable_device(struct pci_dev *);
void pdev_sort_resources(struct pci_dev *, struct resource_list *);
void pci_fixup_irqs(u8 (*)(struct pci_dev *, u8 *),
                    int (*)(struct pci_dev *, u8, u8));

int pci_request_regions(struct pci_dev *, char *);
void pci_release_regions(struct pci_dev *);
int pci_request_region(struct pci_dev *, int, char *);
void pci_release_region(struct pci_dev *, int);


int pci_bus_alloc_resource(struct pci_bus *bus, struct resource *res,
                           unsigned long size, unsigned long align,
                           unsigned long min, unsigned int type_mask,
                           void (*alignf)(void *, struct resource *,
                                          unsigned long, unsigned long),
                           void *alignf_data);
void pci_enable_bridges(struct pci_bus *bus);


int pci_register_driver(struct pci_driver *);
void pci_unregister_driver(struct pci_driver *);
void pci_remove_behind_bridge(struct pci_dev *);
struct pci_driver *pci_dev_driver(const struct pci_dev *);
const struct pci_device_id *pci_match_device(struct pci_driver *drv, struct pci_dev *dev);
const struct pci_device_id *pci_match_id(const struct pci_device_id *ids, struct pci_dev *dev);
int pci_scan_bridge(struct pci_bus *bus, struct pci_dev * dev, int max, int pass);

void pci_walk_bus(struct pci_bus *top, void (*cb)(struct pci_dev *, void *),
                  void *userdata);



# 1 "include/linux/dmapool.h" 1
# 17 "include/linux/dmapool.h"
struct dma_pool *dma_pool_create(const char *name, struct device *dev,
                        size_t size, size_t align, size_t allocation);

void dma_pool_destroy(struct dma_pool *pool);

void *dma_pool_alloc(struct dma_pool *pool, gfp_t mem_flags,
                     dma_addr_t *handle);

void dma_pool_free(struct dma_pool *pool, void *vaddr, dma_addr_t addr);
# 449 "include/linux/pci.h" 2
# 457 "include/linux/pci.h"
enum pci_dma_burst_strategy {
        PCI_DMA_BURST_INFINITY,

        PCI_DMA_BURST_BOUNDARY,

        PCI_DMA_BURST_MULTIPLE,

};


extern struct pci_dev *isa_bridge;


struct msix_entry {
        u16 vector;
        u16 entry;
};


static inline __attribute__((always_inline)) void pci_scan_msi_device(struct pci_dev *dev) {}
static inline __attribute__((always_inline)) int pci_enable_msi(struct pci_dev *dev) {return -1;}
static inline __attribute__((always_inline)) void pci_disable_msi(struct pci_dev *dev) {}
static inline __attribute__((always_inline)) int pci_enable_msix(struct pci_dev* dev,
        struct msix_entry *entries, int nvec) {return -1;}
static inline __attribute__((always_inline)) void pci_disable_msix(struct pci_dev *dev) {}
static inline __attribute__((always_inline)) void msi_remove_pci_irq_vectors(struct pci_dev *dev) {}
# 499 "include/linux/pci.h"
static inline __attribute__((always_inline)) int pci_domain_nr(struct pci_bus *bus) { return 0; }
static inline __attribute__((always_inline)) int pci_proc_domain(struct pci_bus *bus)
{
        return 0;
}
# 567 "include/linux/pci.h"
# 1 "include/asm/pci.h" 1
# 14 "include/asm/pci.h"
extern unsigned int pcibios_assign_all_busses(void);





extern unsigned long pci_mem_start;





void pcibios_config_init(void);
struct pci_bus * pcibios_scan_root(int bus);

void pcibios_set_master(struct pci_dev *dev);
void pcibios_penalize_isa_irq(int irq, int active);
struct irq_routing_table *pcibios_get_irq_routing_table(void);
int pcibios_set_irq_routing(struct pci_dev *dev, int pin, int irq);
# 44 "include/asm/pci.h"
struct pci_dev;
# 63 "include/asm/pci.h"
static inline __attribute__((always_inline)) dma64_addr_t
pci_dac_page_to_dma(struct pci_dev *pdev, struct page *page, unsigned long offset, int direction)
{
        return ((dma64_addr_t) ((dma_addr_t)((unsigned long)((page) - mem_map)) << 12) +
                (dma64_addr_t) offset);
}

static inline __attribute__((always_inline)) struct page *
pci_dac_dma_to_page(struct pci_dev *pdev, dma64_addr_t dma_addr)
{
        return (mem_map + (dma_addr >> 12));
}

static inline __attribute__((always_inline)) unsigned long
pci_dac_dma_to_offset(struct pci_dev *pdev, dma64_addr_t dma_addr)
{
        return (dma_addr & ~(~((1UL << 12)-1)));
}

static inline __attribute__((always_inline)) void
pci_dac_dma_sync_single_for_cpu(struct pci_dev *pdev, dma64_addr_t dma_addr, size_t len, int direction)
{
}

static inline __attribute__((always_inline)) void
pci_dac_dma_sync_single_for_device(struct pci_dev *pdev, dma64_addr_t dma_addr, size_t len, int direction)
{
        flush_write_buffers();
}


extern int pci_mmap_page_range(struct pci_dev *dev, struct vm_area_struct *vma,
                               enum pci_mmap_state mmap_state, int write_combine);


static inline __attribute__((always_inline)) void pcibios_add_platform_entries(struct pci_dev *dev)
{
}


static inline __attribute__((always_inline)) void pci_dma_burst_advice(struct pci_dev *pdev,
                                        enum pci_dma_burst_strategy *strat,
                                        unsigned long *strategy_parameter)
{
        *strat = PCI_DMA_BURST_INFINITY;
        *strategy_parameter = ~0UL;
}





# 1 "include/asm-generic/pci-dma-compat.h" 1






# 1 "include/linux/dma-mapping.h" 1
# 9 "include/linux/dma-mapping.h"
enum dma_data_direction {
        DMA_BIDIRECTIONAL = 0,
        DMA_TO_DEVICE = 1,
        DMA_FROM_DEVICE = 2,
        DMA_NONE = 3,
};
# 24 "include/linux/dma-mapping.h"
# 1 "include/asm/dma-mapping.h" 1
# 13 "include/asm/dma-mapping.h"
void *dma_alloc_coherent(struct device *dev, size_t size,
                           dma_addr_t *dma_handle, gfp_t flag);

void dma_free_coherent(struct device *dev, size_t size,
                         void *vaddr, dma_addr_t dma_handle);

static inline __attribute__((always_inline)) dma_addr_t
dma_map_single(struct device *dev, void *ptr, size_t size,
               enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (23), "i" ("include/asm/dma-mapping.h")); } while(0);
        flush_write_buffers();
        return virt_to_phys(ptr);
}

static inline __attribute__((always_inline)) void
dma_unmap_single(struct device *dev, dma_addr_t dma_addr, size_t size,
                 enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (32), "i" ("include/asm/dma-mapping.h")); } while(0);
}

static inline __attribute__((always_inline)) int
dma_map_sg(struct device *dev, struct scatterlist *sg, int nents,
           enum dma_data_direction direction)
{
        int i;

        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (41), "i" ("include/asm/dma-mapping.h")); } while(0);

        for (i = 0; i < nents; i++ ) {
                do { if (__builtin_expect(!!((!sg[i].page)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (44), "i" ("include/asm/dma-mapping.h")); } while(0);

                sg[i].dma_address = ((dma_addr_t)((unsigned long)((sg[i].page) - mem_map)) << 12) + sg[i].offset;
        }

        flush_write_buffers();
        return nents;
}

static inline __attribute__((always_inline)) dma_addr_t
dma_map_page(struct device *dev, struct page *page, unsigned long offset,
             size_t size, enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (57), "i" ("include/asm/dma-mapping.h")); } while(0);
        return ((dma_addr_t)((unsigned long)((page) - mem_map)) << 12) + offset;
}

static inline __attribute__((always_inline)) void
dma_unmap_page(struct device *dev, dma_addr_t dma_address, size_t size,
               enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (65), "i" ("include/asm/dma-mapping.h")); } while(0);
}


static inline __attribute__((always_inline)) void
dma_unmap_sg(struct device *dev, struct scatterlist *sg, int nhwentries,
             enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (73), "i" ("include/asm/dma-mapping.h")); } while(0);
}

static inline __attribute__((always_inline)) void
dma_sync_single_for_cpu(struct device *dev, dma_addr_t dma_handle, size_t size,
                        enum dma_data_direction direction)
{
}

static inline __attribute__((always_inline)) void
dma_sync_single_for_device(struct device *dev, dma_addr_t dma_handle, size_t size,
                        enum dma_data_direction direction)
{
        flush_write_buffers();
}

static inline __attribute__((always_inline)) void
dma_sync_single_range_for_cpu(struct device *dev, dma_addr_t dma_handle,
                              unsigned long offset, size_t size,
                              enum dma_data_direction direction)
{
}

static inline __attribute__((always_inline)) void
dma_sync_single_range_for_device(struct device *dev, dma_addr_t dma_handle,
                                 unsigned long offset, size_t size,
                                 enum dma_data_direction direction)
{
        flush_write_buffers();
}

static inline __attribute__((always_inline)) void
dma_sync_sg_for_cpu(struct device *dev, struct scatterlist *sg, int nelems,
                    enum dma_data_direction direction)
{
}

static inline __attribute__((always_inline)) void
dma_sync_sg_for_device(struct device *dev, struct scatterlist *sg, int nelems,
                    enum dma_data_direction direction)
{
        flush_write_buffers();
}

static inline __attribute__((always_inline)) int
dma_mapping_error(dma_addr_t dma_addr)
{
        return 0;
}

static inline __attribute__((always_inline)) int
dma_supported(struct device *dev, u64 mask)
{





        if(mask < 0x00ffffff)
                return 0;

        return 1;
}

static inline __attribute__((always_inline)) int
dma_set_mask(struct device *dev, u64 mask)
{
        if(!dev->dma_mask || !dma_supported(dev, mask))
                return -5;

        *dev->dma_mask = mask;

        return 0;
}

static inline __attribute__((always_inline)) int
dma_get_cache_alignment(void)
{


        return (1 << 7);
}



static inline __attribute__((always_inline)) void
dma_cache_sync(void *vaddr, size_t size,
               enum dma_data_direction direction)
{
        flush_write_buffers();
}


extern int
dma_declare_coherent_memory(struct device *dev, dma_addr_t bus_addr,
                            dma_addr_t device_addr, size_t size, int flags);

extern void
dma_release_declared_memory(struct device *dev);

extern void *
dma_mark_declared_memory_occupied(struct device *dev,
                                  dma_addr_t device_addr, size_t size);
# 25 "include/linux/dma-mapping.h" 2





extern u64 dma_get_required_mask(struct device *dev);
# 8 "include/asm-generic/pci-dma-compat.h" 2




static inline __attribute__((always_inline)) int
pci_dma_supported(struct pci_dev *hwdev, u64 mask)
{
        return dma_supported(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, mask);
}

static inline __attribute__((always_inline)) void *
pci_alloc_consistent(struct pci_dev *hwdev, size_t size,
                     dma_addr_t *dma_handle)
{
        return dma_alloc_coherent(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, size, dma_handle, (0x20u));
}

static inline __attribute__((always_inline)) void
pci_free_consistent(struct pci_dev *hwdev, size_t size,
                    void *vaddr, dma_addr_t dma_handle)
{
        dma_free_coherent(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, size, vaddr, dma_handle);
}

static inline __attribute__((always_inline)) dma_addr_t
pci_map_single(struct pci_dev *hwdev, void *ptr, size_t size, int direction)
{
        return dma_map_single(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, ptr, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_unmap_single(struct pci_dev *hwdev, dma_addr_t dma_addr,
                 size_t size, int direction)
{
        dma_unmap_single(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_addr, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) dma_addr_t
pci_map_page(struct pci_dev *hwdev, struct page *page,
             unsigned long offset, size_t size, int direction)
{
        return dma_map_page(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, page, offset, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_unmap_page(struct pci_dev *hwdev, dma_addr_t dma_address,
               size_t size, int direction)
{
        dma_unmap_page(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_address, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) int
pci_map_sg(struct pci_dev *hwdev, struct scatterlist *sg,
           int nents, int direction)
{
        return dma_map_sg(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nents, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_unmap_sg(struct pci_dev *hwdev, struct scatterlist *sg,
             int nents, int direction)
{
        dma_unmap_sg(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nents, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_single_for_cpu(struct pci_dev *hwdev, dma_addr_t dma_handle,
                    size_t size, int direction)
{
        dma_sync_single_for_cpu(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_handle, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_single_for_device(struct pci_dev *hwdev, dma_addr_t dma_handle,
                    size_t size, int direction)
{
        dma_sync_single_for_device(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_handle, size, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_sg_for_cpu(struct pci_dev *hwdev, struct scatterlist *sg,
                int nelems, int direction)
{
        dma_sync_sg_for_cpu(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nelems, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) void
pci_dma_sync_sg_for_device(struct pci_dev *hwdev, struct scatterlist *sg,
                int nelems, int direction)
{
        dma_sync_sg_for_device(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nelems, (enum dma_data_direction)direction);
}

static inline __attribute__((always_inline)) int
pci_dma_mapping_error(dma_addr_t dma_addr)
{
        return dma_mapping_error(dma_addr);
}
# 116 "include/asm/pci.h" 2


# 1 "include/asm-generic/pci.h" 1
# 17 "include/asm-generic/pci.h"
static inline __attribute__((always_inline)) void
pcibios_resource_to_bus(struct pci_dev *dev, struct pci_bus_region *region,
                         struct resource *res)
{
        region->start = res->start;
        region->end = res->end;
}

static inline __attribute__((always_inline)) void
pcibios_bus_to_resource(struct pci_dev *dev, struct resource *res,
                        struct pci_bus_region *region)
{
        res->start = region->start;
        res->end = region->end;
}

static inline __attribute__((always_inline)) struct resource *
pcibios_select_root(struct pci_dev *pdev, struct resource *res)
{
        struct resource *root = ((void *)0);

        if (res->flags & 0x00000100)
                root = &ioport_resource;
        if (res->flags & 0x00000200)
                root = &iomem_resource;

        return root;
}




static inline __attribute__((always_inline)) int pci_get_legacy_ide_irq(struct pci_dev *dev, int channel)
{
        return channel ? 15 : 14;
}
# 119 "include/asm/pci.h" 2
# 568 "include/linux/pci.h" 2
# 586 "include/linux/pci.h"
static inline __attribute__((always_inline)) void *pci_get_drvdata (struct pci_dev *pdev)
{
        return dev_get_drvdata(&pdev->dev);
}

static inline __attribute__((always_inline)) void pci_set_drvdata (struct pci_dev *pdev, void *data)
{
        dev_set_drvdata(&pdev->dev, data);
}




static inline __attribute__((always_inline)) char *pci_name(struct pci_dev *pdev)
{
        return pdev->dev.bus_id;
}






static inline __attribute__((always_inline)) void pci_resource_to_user(const struct pci_dev *dev, int bar,
                const struct resource *rsrc, u64 *start, u64 *end)
{
        *start = rsrc->start;
        *end = rsrc->end;
}
# 625 "include/linux/pci.h"
struct pci_fixup {
        u16 vendor, device;
        void (*hook)(struct pci_dev *dev);
};

enum pci_fixup_pass {
        pci_fixup_early,
        pci_fixup_header,
        pci_fixup_final,
        pci_fixup_enable,
};
# 655 "include/linux/pci.h"
void pci_fixup_device(enum pci_fixup_pass pass, struct pci_dev *dev);

extern int pci_pci_problems;
# 21 "include/linux/ide.h" 2
# 69 "include/linux/ide.h"
typedef unsigned char byte;
# 204 "include/linux/ide.h"
struct hwif_s;
typedef int (ide_ack_intr_t)(struct hwif_s *);
# 215 "include/linux/ide.h"
typedef enum { ide_unknown, ide_generic, ide_pci,
                ide_cmd640, ide_dtc2278, ide_ali14xx,
                ide_qd65xx, ide_umc8672, ide_ht6560b,
                ide_rz1000, ide_trm290,
                ide_cmd646, ide_cy82c693, ide_4drives,
                ide_pmac, ide_etrax100, ide_acorn,
                ide_forced
} hwif_chipset_t;




typedef struct hw_regs_s {
        unsigned long io_ports[(10)];
        int irq;
        int dma;
        ide_ack_intr_t *ack_intr;
        hwif_chipset_t chipset;
} hw_regs_t;




int ide_register_hw(hw_regs_t *hw, struct hwif_s **hwifp);
int ide_register_hw_with_fixup(hw_regs_t *, struct hwif_s **, void (*)(struct hwif_s *));




void ide_setup_ports( hw_regs_t *hw,
                        unsigned long base,
                        int *offsets,
                        unsigned long ctrl,
                        unsigned long intr,
                        ide_ack_intr_t *ack_intr,



                        int irq);

static inline __attribute__((always_inline)) void ide_std_init_ports(hw_regs_t *hw,
                                      unsigned long io_addr,
                                      unsigned long ctl_addr)
{
        unsigned int i;

        for (i = (0); i <= (7); i++)
                hw->io_ports[i] = io_addr++;

        hw->io_ports[(8)] = ctl_addr;
}

# 1 "include/asm/ide.h" 1
# 28 "include/asm/ide.h"
static __inline__ __attribute__((always_inline)) int ide_default_irq(unsigned long base)
{
        switch (base) {
                case 0x1f0: return 14;
                case 0x170: return 15;
                case 0x1e8: return 11;
                case 0x168: return 10;
                case 0x1e0: return 8;
                case 0x160: return 12;
                default:
                        return 0;
        }
}

static __inline__ __attribute__((always_inline)) unsigned long ide_default_io_base(int index)
{
        if (pci_find_device((~0), (~0), ((void *)0)) == ((void *)0)) {
                switch(index) {
                        case 2: return 0x1e8;
                        case 3: return 0x168;
                        case 4: return 0x1e0;
                        case 5: return 0x160;
                }
        }
        switch (index) {
                case 0: return 0x1f0;
                case 1: return 0x170;
                default:
                        return 0;
        }
}
# 69 "include/asm/ide.h"
# 1 "include/asm-generic/ide_iops.h" 1







static __inline__ __attribute__((always_inline)) void __ide_mm_insw(void *port, void *addr, u32 count)
{
        while (count--) {
                *(u16 *)addr = readw(port);
                addr += 2;
        }
}

static __inline__ __attribute__((always_inline)) void __ide_mm_insl(void *port, void *addr, u32 count)
{
        while (count--) {
                *(u32 *)addr = readl(port);
                addr += 4;
        }
}

static __inline__ __attribute__((always_inline)) void __ide_mm_outsw(void *port, void *addr, u32 count)
{
        while (count--) {
                writew(*(u16 *)addr, port);
                addr += 2;
        }
}

static __inline__ __attribute__((always_inline)) void __ide_mm_outsl(void * port, void *addr, u32 count)
{
        while (count--) {
                writel(*(u32 *)addr, port);
                addr += 4;
        }
}
# 70 "include/asm/ide.h" 2
# 268 "include/linux/ide.h" 2
# 281 "include/linux/ide.h"
static inline __attribute__((always_inline)) void ide_init_hwif_ports(hw_regs_t *hw,
                                       unsigned long io_addr,
                                       unsigned long ctl_addr,
                                       int *irq)
{
        if (!ctl_addr)
                ide_std_init_ports(hw, io_addr, ((io_addr) + 0x206));
        else
                ide_std_init_ports(hw, io_addr, ctl_addr);

        if (irq)
                *irq = 0;

        hw->io_ports[(9)] = 0;





}
# 344 "include/linux/ide.h"
typedef union {
        unsigned all : 8;
        struct {

                unsigned set_geometry : 1;
                unsigned recalibrate : 1;
                unsigned set_multmode : 1;
                unsigned set_tune : 1;
                unsigned serviced : 1;
                unsigned reserved : 3;
# 364 "include/linux/ide.h"
        } b;
} special_t;







typedef union {
        unsigned all :16;
        struct {

                unsigned low :8;
                unsigned high :8;






        } b;
} ata_nsector_t, ata_data_t, atapi_bcount_t, ata_index_t;
# 400 "include/linux/ide.h"
typedef union {
        unsigned all :8;
        struct {

                unsigned mark :1;
                unsigned tzero :1;
                unsigned abrt :1;
                unsigned mcr :1;
                unsigned id :1;
                unsigned mce :1;
                unsigned ecc :1;
                unsigned bdd :1;
# 424 "include/linux/ide.h"
        } b;
} ata_error_t;
# 436 "include/linux/ide.h"
typedef union {
        unsigned all : 8;
        struct {

                unsigned head : 4;
                unsigned unit : 1;
                unsigned bit5 : 1;
                unsigned lba : 1;
                unsigned bit7 : 1;
# 454 "include/linux/ide.h"
        } b;
} select_t, ata_select_t;
# 474 "include/linux/ide.h"
typedef union {
        unsigned all :8;
        struct {

                unsigned check :1;
                unsigned idx :1;
                unsigned corr :1;
                unsigned drq :1;
                unsigned dsc :1;
                unsigned df :1;
                unsigned drdy :1;
                unsigned bsy :1;
# 498 "include/linux/ide.h"
        } b;
} ata_status_t, atapi_status_t;
# 511 "include/linux/ide.h"
typedef union {
        unsigned all : 8;
        struct {

                unsigned bit0 : 1;
                unsigned nIEN : 1;
                unsigned SRST : 1;
                unsigned bit3 : 1;
                unsigned reserved456 : 3;
                unsigned HOB : 1;
# 531 "include/linux/ide.h"
        } b;
} ata_control_t;
# 542 "include/linux/ide.h"
typedef union {
        unsigned all :8;
        struct {

                unsigned dma :1;
                unsigned reserved321 :3;
                unsigned reserved654 :3;
                unsigned reserved7 :1;
# 558 "include/linux/ide.h"
        } b;
} atapi_feature_t;
# 568 "include/linux/ide.h"
typedef union {
        unsigned all :8;
        struct {

                unsigned cod :1;
                unsigned io :1;
                unsigned reserved :6;







        } b;
} atapi_ireason_t;
# 594 "include/linux/ide.h"
typedef union {
        unsigned all :8;
        struct {

                unsigned ili :1;
                unsigned eom :1;
                unsigned abrt :1;
                unsigned mcr :1;
                unsigned sense_key :4;
# 612 "include/linux/ide.h"
        } b;
} atapi_error_t;
# 625 "include/linux/ide.h"
typedef union {
        unsigned all :8;
        struct {

                unsigned sam_lun :3;
                unsigned reserved3 :1;
                unsigned drv :1;
                unsigned one5 :1;
                unsigned reserved6 :1;
                unsigned one7 :1;
# 645 "include/linux/ide.h"
        } b;
} atapi_select_t;




typedef enum {
        ide_stopped,
        ide_started,
} ide_startstop_t;

struct ide_driver_s;
struct ide_settings_s;

typedef struct ide_drive_s {
        char name[4];
        char driver_req[10];

        request_queue_t *queue;

        struct request *rq;
        struct ide_drive_s *next;
        void *driver_data;
        struct hd_driveid *id;
        struct proc_dir_entry *proc;
        struct ide_settings_s *settings;
        char devfs_name[64];

        struct hwif_s *hwif;

        unsigned long sleep;
        unsigned long service_start;
        unsigned long service_time;
        unsigned long timeout;

        special_t special;
        select_t select;

        u8 keep_settings;
        u8 autodma;
        u8 using_dma;
        u8 retry_pio;
        u8 state;
        u8 waiting_for_dma;
        u8 unmask;
        u8 bswap;
        u8 dsc_overlap;
        u8 nice1;

        unsigned present : 1;
        unsigned dead : 1;
        unsigned id_read : 1;
        unsigned noprobe : 1;
        unsigned removable : 1;
        unsigned attach : 1;
        unsigned is_flash : 1;
        unsigned forced_geom : 1;
        unsigned no_unmask : 1;
        unsigned no_io_32bit : 1;
        unsigned atapi_overlap : 1;
        unsigned nice0 : 1;
        unsigned nice2 : 1;
        unsigned doorlocking : 1;
        unsigned autotune : 2;
        unsigned remap_0_to_1 : 1;
        unsigned blocked : 1;
        unsigned vdma : 1;
        unsigned addressing;





        unsigned scsi : 1;
        unsigned sleeping : 1;
        unsigned post_reset : 1;

        u8 quirk_list;
        u8 init_speed;
        u8 pio_speed;
        u8 current_speed;
        u8 dn;
        u8 wcache;
        u8 acoustic;
        u8 media;
        u8 ctl;
        u8 ready_stat;
        u8 mult_count;
        u8 mult_req;
        u8 tune_req;
        u8 io_32bit;
        u8 bad_wstat;
        u8 nowerr;
        u8 sect0;
        u8 head;
        u8 sect;
        u8 bios_head;
        u8 bios_sect;

        unsigned int bios_cyl;
        unsigned int cyl;
        unsigned int drive_data;
        unsigned int usage;
        unsigned int failures;
        unsigned int max_failures;

        u64 capacity64;

        int lun;
        int crc_count;
        struct list_head list;
        struct device gendev;
        struct semaphore gendev_rel_sem;
} ide_drive_t;







struct ide_pci_device_s;

typedef struct hwif_s {
        struct hwif_s *next;
        struct hwif_s *mate;
        struct hwgroup_s *hwgroup;
        struct proc_dir_entry *proc;

        char name[6];


        unsigned long io_ports[(10)];
        unsigned long sata_scr[(3)];
        unsigned long sata_misc[(3)];

        hw_regs_t hw;
        ide_drive_t drives[2];

        u8 major;
        u8 index;
        u8 channel;
        u8 straight8;
        u8 bus_state;

        u8 atapi_dma;
        u8 ultra_mask;
        u8 mwdma_mask;
        u8 swdma_mask;

        hwif_chipset_t chipset;

        struct pci_dev *pci_dev;
        struct ide_pci_device_s *cds;

        void (*rw_disk)(ide_drive_t *, struct request *);





        void (*tuneproc)(ide_drive_t *, u8);

        int (*speedproc)(ide_drive_t *, u8);

        void (*selectproc)(ide_drive_t *);

        int (*reset_poll)(ide_drive_t *);

        void (*pre_reset)(ide_drive_t *);

        void (*resetproc)(ide_drive_t *);

        void (*intrproc)(ide_drive_t *);

        void (*maskproc)(ide_drive_t *, int);

        int (*quirkproc)(ide_drive_t *);

        int (*busproc)(ide_drive_t *, int);






        void (*ata_input_data)(ide_drive_t *, void *, u32);
        void (*ata_output_data)(ide_drive_t *, void *, u32);

        void (*atapi_input_bytes)(ide_drive_t *, void *, u32);
        void (*atapi_output_bytes)(ide_drive_t *, void *, u32);

        int (*dma_setup)(ide_drive_t *);
        void (*dma_exec_cmd)(ide_drive_t *, u8);
        void (*dma_start)(ide_drive_t *);
        int (*ide_dma_end)(ide_drive_t *drive);
        int (*ide_dma_check)(ide_drive_t *drive);
        int (*ide_dma_on)(ide_drive_t *drive);
        int (*ide_dma_off_quietly)(ide_drive_t *drive);
        int (*ide_dma_test_irq)(ide_drive_t *drive);
        int (*ide_dma_host_on)(ide_drive_t *drive);
        int (*ide_dma_host_off)(ide_drive_t *drive);
        int (*ide_dma_lostirq)(ide_drive_t *drive);
        int (*ide_dma_timeout)(ide_drive_t *drive);

        void (*OUTB)(u8 addr, unsigned long port);
        void (*OUTBSYNC)(ide_drive_t *drive, u8 addr, unsigned long port);
        void (*OUTW)(u16 addr, unsigned long port);
        void (*OUTL)(u32 addr, unsigned long port);
        void (*OUTSW)(unsigned long port, void *addr, u32 count);
        void (*OUTSL)(unsigned long port, void *addr, u32 count);

        u8 (*INB)(unsigned long port);
        u16 (*INW)(unsigned long port);
        u32 (*INL)(unsigned long port);
        void (*INSW)(unsigned long port, void *addr, u32 count);
        void (*INSL)(unsigned long port, void *addr, u32 count);


        unsigned int *dmatable_cpu;

        dma_addr_t dmatable_dma;

        struct scatterlist *sg_table;
        int sg_max_nents;
        int sg_nents;
        int sg_dma_direction;


        int data_phase;

        unsigned int nsect;
        unsigned int nleft;
        unsigned int cursg;
        unsigned int cursg_ofs;

        int mmio;
        int rqsize;
        int irq;

        unsigned long dma_master;
        unsigned long dma_base;
        unsigned long dma_command;
        unsigned long dma_vendor1;
        unsigned long dma_status;
        unsigned long dma_vendor3;
        unsigned long dma_prdtable;
        unsigned long dma_base2;

        unsigned dma_extra;
        unsigned long config_data;
        unsigned long select_data;

        unsigned noprobe : 1;
        unsigned present : 1;
        unsigned hold : 1;
        unsigned serialized : 1;
        unsigned sharing_irq: 1;
        unsigned reset : 1;
        unsigned autodma : 1;
        unsigned udma_four : 1;
        unsigned no_lba48 : 1;
        unsigned no_lba48_dma : 1;
        unsigned no_dsc : 1;
        unsigned auto_poll : 1;
        unsigned sg_mapped : 1;

        struct device gendev;
        struct semaphore gendev_rel_sem;

        void *hwif_data;

        unsigned dma;

        void (*led_act)(void *data, int rw);
} __attribute__((__aligned__(1 << (7)))) ide_hwif_t;




typedef ide_startstop_t (ide_pre_handler_t)(ide_drive_t *, struct request *);
typedef ide_startstop_t (ide_handler_t)(ide_drive_t *);
typedef int (ide_expiry_t)(ide_drive_t *);

typedef struct hwgroup_s {

        ide_startstop_t (*handler)(ide_drive_t *);

        ide_startstop_t (*handler_save)(ide_drive_t *);

        volatile int busy;

        unsigned int sleeping : 1;

        unsigned int polling : 1;

        ide_drive_t *drive;

        ide_hwif_t *hwif;


        struct pci_dev *pci_dev;

        struct ide_pci_device_s *cds;


        struct request *rq;

        struct timer_list timer;

        struct request wrq;

        unsigned long poll_timeout;

        int (*expiry)(ide_drive_t *);

        int pio_clock;

        unsigned char cmd_buf[4];
} ide_hwgroup_t;
# 981 "include/linux/ide.h"
typedef int (ide_procset_t)(ide_drive_t *, int);
typedef struct ide_settings_s {
        char *name;
        int rw;
        int read_ioctl;
        int write_ioctl;
        int data_type;
        int min;
        int max;
        int mul_factor;
        int div_factor;
        void *data;
        ide_procset_t *set;
        int auto_remove;
        struct ide_settings_s *next;
} ide_settings_t;

extern struct semaphore ide_setting_sem;
extern int ide_add_setting(ide_drive_t *drive, const char *name, int rw, int read_ioctl, int write_ioctl, int data_type, int min, int max, int mul_factor, int div_factor, void *data, ide_procset_t *set);
extern ide_settings_t *ide_find_setting_by_name(ide_drive_t *drive, char *name);
extern int ide_read_setting(ide_drive_t *t, ide_settings_t *setting);
extern int ide_write_setting(ide_drive_t *drive, ide_settings_t *setting, int val);
extern void ide_add_generic_settings(ide_drive_t *drive);




typedef struct {
        const char *name;
        mode_t mode;
        read_proc_t *read_proc;
        write_proc_t *write_proc;
} ide_proc_entry_t;


extern struct proc_dir_entry *proc_ide_root;

extern void proc_ide_create(void);
extern void proc_ide_destroy(void);
extern void create_proc_ide_interfaces(void);
void destroy_proc_ide_interface(ide_hwif_t *);
extern void ide_add_proc_entries(struct proc_dir_entry *, ide_proc_entry_t *, void *);
extern void ide_remove_proc_entries(struct proc_dir_entry *, ide_proc_entry_t *);
read_proc_t proc_ide_read_capacity;
read_proc_t proc_ide_read_geometry;


void ide_pci_create_host_proc(const char *, get_info_t *);
# 1079 "include/linux/ide.h"
enum {
        ide_pm_state_completed = -1,
        ide_pm_state_start_suspend = 0,
        ide_pm_state_start_resume = 1000,
};




typedef struct ide_driver_s {
        struct module *owner;
        const char *version;
        u8 media;
        unsigned supports_dsc_overlap : 1;
        ide_startstop_t (*do_request)(ide_drive_t *, struct request *, sector_t);
        int (*end_request)(ide_drive_t *, int, int);
        ide_startstop_t (*error)(ide_drive_t *, struct request *rq, u8, u8);
        ide_startstop_t (*abort)(ide_drive_t *, struct request *rq);
        int (*ioctl)(ide_drive_t *, struct inode *, struct file *, unsigned int, unsigned long);
        ide_proc_entry_t *proc;
        void (*ata_prebuilder)(ide_drive_t *);
        void (*atapi_prebuilder)(ide_drive_t *);
        struct device_driver gen_driver;
} ide_driver_t;

int generic_ide_ioctl(ide_drive_t *, struct file *, struct block_device *, unsigned, unsigned long);
# 1115 "include/linux/ide.h"
extern ide_hwif_t ide_hwifs[];

extern int noautodma;

extern int ide_end_request (ide_drive_t *drive, int uptodate, int nrsecs);
extern int __ide_end_request (ide_drive_t *drive, struct request *rq, int uptodate, int nrsecs);





extern void ide_set_handler (ide_drive_t *drive, ide_handler_t *handler, unsigned int timeout, ide_expiry_t *expiry);






extern void ide_execute_command(ide_drive_t *, task_ioreg_t cmd, ide_handler_t *, unsigned int, ide_expiry_t *);

ide_startstop_t __ide_error(ide_drive_t *, struct request *, u8, u8);







ide_startstop_t ide_error (ide_drive_t *drive, const char *msg, byte stat);

ide_startstop_t __ide_abort(ide_drive_t *, struct request *);






extern ide_startstop_t ide_abort(ide_drive_t *, const char *);

extern void ide_fix_driveid(struct hd_driveid *);
# 1163 "include/linux/ide.h"
extern void ide_fixstring(u8 *, const int, const int);
# 1174 "include/linux/ide.h"
extern int ide_wait_stat(ide_startstop_t *, ide_drive_t *, u8, u8, unsigned long);





extern ide_startstop_t ide_do_reset (ide_drive_t *);




extern void ide_init_drive_cmd (struct request *rq);




extern u64 ide_get_error_location(ide_drive_t *, char *);




typedef enum {
        ide_wait,
        ide_next,
        ide_preempt,
        ide_head_wait,
        ide_end
} ide_action_t;
# 1228 "include/linux/ide.h"
extern int ide_do_drive_cmd(ide_drive_t *, struct request *, ide_action_t);
# 1237 "include/linux/ide.h"
extern void ide_end_drive_cmd(ide_drive_t *, u8, u8);







extern int ide_wait_cmd(ide_drive_t *, u8, u8, u8, u8, u8 *);

typedef struct ide_task_s {






        task_ioreg_t tfRegister[8];
        task_ioreg_t hobRegister[8];
        ide_reg_valid_t tf_out_flags;
        ide_reg_valid_t tf_in_flags;
        int data_phase;
        int command_type;
        ide_pre_handler_t *prehandler;
        ide_handler_t *handler;
        struct request *rq;
        void *special;
} ide_task_t;

extern u32 ide_read_24(ide_drive_t *);

extern void SELECT_DRIVE(ide_drive_t *);
extern void SELECT_INTERRUPT(ide_drive_t *);
extern void SELECT_MASK(ide_drive_t *, int);
extern void QUIRK_LIST(ide_drive_t *);

extern int drive_is_ready(ide_drive_t *);
extern int wait_for_ready(ide_drive_t *, int );




extern ide_startstop_t do_rw_taskfile(ide_drive_t *, ide_task_t *);




extern ide_startstop_t flagged_taskfile(ide_drive_t *, ide_task_t *);

extern ide_startstop_t set_multmode_intr(ide_drive_t *);
extern ide_startstop_t set_geometry_intr(ide_drive_t *);
extern ide_startstop_t recal_intr(ide_drive_t *);
extern ide_startstop_t task_no_data_intr(ide_drive_t *);
extern ide_startstop_t task_in_intr(ide_drive_t *);
extern ide_startstop_t pre_task_out_intr(ide_drive_t *, struct request *);

extern int ide_raw_taskfile(ide_drive_t *, ide_task_t *, u8 *);

int ide_taskfile_ioctl(ide_drive_t *, unsigned int, unsigned long);
int ide_cmd_ioctl(ide_drive_t *, unsigned int, unsigned long);
int ide_task_ioctl(ide_drive_t *, unsigned int, unsigned long);

extern int system_bus_clock(void);

extern int ide_driveid_update(ide_drive_t *);
extern int ide_ata66_check(ide_drive_t *, ide_task_t *);
extern int ide_config_drive_speed(ide_drive_t *, u8);
extern u8 eighty_ninty_three (ide_drive_t *);
extern int set_transfer(ide_drive_t *, ide_task_t *);
extern int taskfile_lib_get_identify(ide_drive_t *drive, u8 *);

extern int ide_wait_not_busy(ide_hwif_t *hwif, unsigned long timeout);





extern void ide_stall_queue(ide_drive_t *drive, unsigned long timeout);

extern int ide_spin_wait_hwgroup(ide_drive_t *);
extern void ide_timer_expiry(unsigned long);
extern irqreturn_t ide_intr(int irq, void *dev_id, struct pt_regs *regs);
extern void do_ide_request(request_queue_t *);
extern void ide_init_subdrivers(void);

void ide_init_disk(struct gendisk *, ide_drive_t *);

extern int ideprobe_init(void);

extern void ide_scan_pcibus(int scan_direction) __attribute__ ((__section__ (".init.text")));
extern int ide_pci_register_driver(struct pci_driver *driver);
extern void ide_pci_unregister_driver(struct pci_driver *driver);
void ide_pci_setup_ports(struct pci_dev *, struct ide_pci_device_s *, int, ata_index_t *);
extern void ide_setup_pci_noise (struct pci_dev *dev, struct ide_pci_device_s *d);

extern void default_hwif_iops(ide_hwif_t *);
extern void default_hwif_mmiops(ide_hwif_t *);
extern void default_hwif_transport(ide_hwif_t *);

void ide_register_subdriver(ide_drive_t *, ide_driver_t *);
void ide_unregister_subdriver(ide_drive_t *, ide_driver_t *);
# 1352 "include/linux/ide.h"
typedef struct ide_pci_enablebit_s {
        u8 reg;
        u8 mask;
        u8 val;
} ide_pci_enablebit_t;

enum {

        IDEPCI_FLAG_ISA_PORTS = (1 << 0),
        IDEPCI_FLAG_FORCE_PDC = (1 << 1),
};

typedef struct ide_pci_device_s {
        char *name;
        int (*init_setup)(struct pci_dev *, struct ide_pci_device_s *);
        void (*init_setup_dma)(struct pci_dev *, struct ide_pci_device_s *, ide_hwif_t *);
        unsigned int (*init_chipset)(struct pci_dev *, const char *);
        void (*init_iops)(ide_hwif_t *);
        void (*init_hwif)(ide_hwif_t *);
        void (*init_dma)(ide_hwif_t *, unsigned long);
        void (*fixup)(ide_hwif_t *);
        u8 channels;
        u8 autodma;
        ide_pci_enablebit_t enablebits[2];
        u8 bootable;
        unsigned int extra;
        struct ide_pci_device_s *next;
        u8 flags;
} ide_pci_device_t;

extern int ide_setup_pci_device(struct pci_dev *, ide_pci_device_t *);
extern int ide_setup_pci_devices(struct pci_dev *, struct pci_dev *, ide_pci_device_t *);

void ide_map_sg(ide_drive_t *, struct request *);
void ide_init_sg_cmd(ide_drive_t *, struct request *);





int __ide_dma_bad_drive(ide_drive_t *);
int __ide_dma_good_drive(ide_drive_t *);
int ide_use_dma(ide_drive_t *);
int __ide_dma_off(ide_drive_t *);
void ide_dma_verbose(ide_drive_t *);
ide_startstop_t ide_dma_intr(ide_drive_t *);


extern int ide_build_sglist(ide_drive_t *, struct request *);
extern int ide_build_dmatable(ide_drive_t *, struct request *);
extern void ide_destroy_dmatable(ide_drive_t *);
extern int ide_release_dma(ide_hwif_t *);
extern void ide_setup_dma(ide_hwif_t *, unsigned long, unsigned int);

extern int __ide_dma_host_off(ide_drive_t *);
extern int __ide_dma_off_quietly(ide_drive_t *);
extern int __ide_dma_host_on(ide_drive_t *);
extern int __ide_dma_on(ide_drive_t *);
extern int __ide_dma_check(ide_drive_t *);
extern int ide_dma_setup(ide_drive_t *);
extern void ide_dma_start(ide_drive_t *);
extern int __ide_dma_end(ide_drive_t *);
extern int __ide_dma_lostirq(ide_drive_t *);
extern int __ide_dma_timeout(ide_drive_t *);
# 1428 "include/linux/ide.h"
extern int ide_hwif_request_regions(ide_hwif_t *hwif);
extern void ide_hwif_release_regions(ide_hwif_t* hwif);
extern void ide_unregister (unsigned int index);

void ide_register_region(struct gendisk *);
void ide_unregister_region(struct gendisk *);

void ide_undecoded_slave(ide_hwif_t *);

int probe_hwif_init_with_fixup(ide_hwif_t *, void (*)(ide_hwif_t *));
extern int probe_hwif_init(ide_hwif_t *);

static inline __attribute__((always_inline)) void *ide_get_hwifdata (ide_hwif_t * hwif)
{
        return hwif->hwif_data;
}

static inline __attribute__((always_inline)) void ide_set_hwifdata (ide_hwif_t * hwif, void *data)
{
        hwif->hwif_data = data;
}


extern u8 ide_dma_speed(ide_drive_t *drive, u8 mode);
extern u8 ide_rate_filter(u8 mode, u8 speed);
extern int ide_dma_enable(ide_drive_t *drive);
extern char *ide_xfer_verbose(u8 xfer_rate);
extern void ide_toggle_bounce(ide_drive_t *drive, int on);
extern int ide_set_xfer_rate(ide_drive_t *drive, u8 rate);

u8 ide_dump_status(ide_drive_t *, const char *, u8);

typedef struct ide_pio_timings_s {
        int setup_time;
        int active_time;
        int cycle_time;
} ide_pio_timings_t;

typedef struct ide_pio_data_s {
        u8 pio_mode;
        u8 use_iordy;
        u8 overridden;
        u8 blacklisted;
        unsigned int cycle_time;
} ide_pio_data_t;

extern u8 ide_get_best_pio_mode (ide_drive_t *drive, u8 mode_wanted, u8 max_mode, ide_pio_data_t *d);
extern const ide_pio_timings_t ide_pio_timings[6];


extern spinlock_t ide_lock;
extern struct semaphore ide_cfg_sem;
# 1495 "include/linux/ide.h"
extern struct bus_type ide_bus_type;
# 1504 "include/linux/ide.h"
static inline __attribute__((always_inline)) int hwif_to_node(ide_hwif_t *hwif)
{
        struct pci_dev *dev = hwif->pci_dev;
        return dev ? (-1) : -1;
}
# 67 "drivers/ide/ide-disk.c" 2







struct ide_disk_obj {
        ide_drive_t *drive;
        ide_driver_t *driver;
        struct gendisk *disk;
        struct kref kref;
};

static struct semaphore idedisk_ref_sem = { .count = { (1) }, .sleepers = 0, .wait = { .lock = (spinlock_t) { .raw_lock = { 1 } }, .task_list = { &((idedisk_ref_sem).wait).task_list, &((idedisk_ref_sem).wait).task_list } } };






static struct ide_disk_obj *ide_disk_get(struct gendisk *disk)
{
        struct ide_disk_obj *idkp = ((void *)0);

        down(&idedisk_ref_sem);
        idkp = ({ const typeof( ((struct ide_disk_obj *)0)->driver ) *__mptr = ((disk)->private_data); (struct ide_disk_obj *)( (char *)__mptr - ((size_t) &((struct ide_disk_obj *)0)->driver) );});
        if (idkp)
                kref_get(&idkp->kref);
        up(&idedisk_ref_sem);
        return idkp;
}

static void ide_disk_release(struct kref *);

static void ide_disk_put(struct ide_disk_obj *idkp)
{
        down(&idedisk_ref_sem);
        kref_put(&idkp->kref, ide_disk_release);
        up(&idedisk_ref_sem);
}
# 118 "drivers/ide/ide-disk.c"
static int lba_capacity_is_ok (struct hd_driveid *id)
{
        unsigned long lba_sects, chs_sects, head, tail;


        if (id->cyls == 0)
                return 1;







        if ((id->cyls == 16383
             || (id->cyls == 4092 && id->cur_cyls == 16383)) &&
            id->sectors == 63 &&
            (id->heads == 15 || id->heads == 16) &&
            (id->lba_capacity >= 16383*63*id->heads))
                return 1;

        lba_sects = id->lba_capacity;
        chs_sects = id->cyls * id->heads * id->sectors;


        if ((lba_sects - chs_sects) < chs_sects/10)
                return 1;


        head = ((lba_sects >> 16) & 0xffff);
        tail = (lba_sects & 0xffff);
        lba_sects = (head | (tail << 16));
        if ((lba_sects - chs_sects) < chs_sects/10) {
                id->lba_capacity = lba_sects;
                return 1;
        }

        return 0;
}





static ide_startstop_t __ide_do_rw_disk(ide_drive_t *drive, struct request *rq, sector_t block)
{
        ide_hwif_t *hwif = ((ide_hwif_t *)((drive)->hwif));
        unsigned int dma = drive->using_dma;
        u8 lba48 = (drive->addressing == 1) ? 1 : 0;
        task_ioreg_t command = 0x00;
        ata_nsector_t nsectors;

        nsectors.all = (u16) rq->nr_sectors;

        if (hwif->no_lba48_dma && lba48 && dma) {
                if (block + rq->nr_sectors > 1ULL << 28)
                        dma = 0;
                else
                        lba48 = 0;
        }

        if (!dma) {
                ide_init_sg_cmd(drive, rq);
                ide_map_sg(drive, rq);
        }

        if ((((ide_hwif_t *)((drive)->hwif))->io_ports[(8)]))
                hwif->OUTB(drive->ctl, (((ide_hwif_t *)((drive)->hwif))->io_ports[(8)]));



        if (drive->select.b.lba) {
                if (lba48) {
                        task_ioreg_t tasklets[10];

                        do { } while (0);

                        tasklets[0] = 0;
                        tasklets[1] = 0;
                        tasklets[2] = nsectors.b.low;
                        tasklets[3] = nsectors.b.high;
                        tasklets[4] = (task_ioreg_t) block;
                        tasklets[5] = (task_ioreg_t) (block>>8);
                        tasklets[6] = (task_ioreg_t) (block>>16);
                        tasklets[7] = (task_ioreg_t) (block>>24);
                        if (sizeof(block) == 4) {
                                tasklets[8] = (task_ioreg_t) 0;
                                tasklets[9] = (task_ioreg_t) 0;
                        } else {
                                tasklets[8] = (task_ioreg_t)((u64)block >> 32);
                                tasklets[9] = (task_ioreg_t)((u64)block >> 40);
                        }






                        hwif->OUTB(tasklets[1], (((ide_hwif_t *)((drive)->hwif))->io_ports[(1)]));
                        hwif->OUTB(tasklets[3], (((ide_hwif_t *)((drive)->hwif))->io_ports[(2)]));
                        hwif->OUTB(tasklets[7], (((ide_hwif_t *)((drive)->hwif))->io_ports[(3)]));
                        hwif->OUTB(tasklets[8], (((ide_hwif_t *)((drive)->hwif))->io_ports[(4)]));
                        hwif->OUTB(tasklets[9], (((ide_hwif_t *)((drive)->hwif))->io_ports[(5)]));

                        hwif->OUTB(tasklets[0], (((ide_hwif_t *)((drive)->hwif))->io_ports[(1)]));
                        hwif->OUTB(tasklets[2], (((ide_hwif_t *)((drive)->hwif))->io_ports[(2)]));
                        hwif->OUTB(tasklets[4], (((ide_hwif_t *)((drive)->hwif))->io_ports[(3)]));
                        hwif->OUTB(tasklets[5], (((ide_hwif_t *)((drive)->hwif))->io_ports[(4)]));
                        hwif->OUTB(tasklets[6], (((ide_hwif_t *)((drive)->hwif))->io_ports[(5)]));
                        hwif->OUTB(0x00|drive->select.all,(((ide_hwif_t *)((drive)->hwif))->io_ports[(6)]));
                } else {
                        hwif->OUTB(0x00, (((ide_hwif_t *)((drive)->hwif))->io_ports[(1)]));
                        hwif->OUTB(nsectors.b.low, (((ide_hwif_t *)((drive)->hwif))->io_ports[(2)]));
                        hwif->OUTB(block, (((ide_hwif_t *)((drive)->hwif))->io_ports[(3)]));
                        hwif->OUTB(block>>=8, (((ide_hwif_t *)((drive)->hwif))->io_ports[(4)]));
                        hwif->OUTB(block>>=8, (((ide_hwif_t *)((drive)->hwif))->io_ports[(5)]));
                        hwif->OUTB(((block>>8)&0x0f)|drive->select.all,(((ide_hwif_t *)((drive)->hwif))->io_ports[(6)]));
                }
        } else {
                unsigned int sect,head,cyl,track;
                track = (int)block / drive->sect;
                sect = (int)block % drive->sect + 1;
                hwif->OUTB(sect, (((ide_hwif_t *)((drive)->hwif))->io_ports[(3)]));
                head = track % drive->head;
                cyl = track / drive->head;

                do { } while (0);

                hwif->OUTB(0x00, (((ide_hwif_t *)((drive)->hwif))->io_ports[(1)]));
                hwif->OUTB(nsectors.b.low, (((ide_hwif_t *)((drive)->hwif))->io_ports[(2)]));
                hwif->OUTB(cyl, (((ide_hwif_t *)((drive)->hwif))->io_ports[(4)]));
                hwif->OUTB(cyl>>8, (((ide_hwif_t *)((drive)->hwif))->io_ports[(5)]));
                hwif->OUTB(head|drive->select.all,(((ide_hwif_t *)((drive)->hwif))->io_ports[(6)]));
        }

        if (dma) {
                if (!hwif->dma_setup(drive)) {
                        if (((rq)->flags & 1)) {
                                command = lba48 ? 0x35 : 0xCA;
                                if (drive->vdma)
                                        command = lba48 ? 0x34: 0x30;
                        } else {
                                command = lba48 ? 0x25 : 0xC8;
                                if (drive->vdma)
                                        command = lba48 ? 0x24: 0x20;
                        }
                        hwif->dma_exec_cmd(drive, command);
                        hwif->dma_start(drive);
                        return ide_started;
                }

                ide_init_sg_cmd(drive, rq);
        }

        if (((rq)->flags & 1) == 0) {

                if (drive->mult_count) {
                        hwif->data_phase = 0x0002;
                        command = lba48 ? 0x29 : 0xC4;
                } else {
                        hwif->data_phase = 0x0001;
                        command = lba48 ? 0x24 : 0x20;
                }

                ide_execute_command(drive, command, &task_in_intr, (10*250), ((void *)0));
                return ide_started;
        } else {
                if (drive->mult_count) {
                        hwif->data_phase = 0x0008;
                        command = lba48 ? 0x39 : 0xC5;
                } else {
                        hwif->data_phase = 0x0004;
                        command = lba48 ? 0x34 : 0x30;
                }


                hwif->OUTB(command, (((ide_hwif_t *)((drive)->hwif))->io_ports[(7)]));

                return pre_task_out_intr(drive, rq);
        }
}







static ide_startstop_t ide_do_rw_disk (ide_drive_t *drive, struct request *rq, sector_t block)
{
        ide_hwif_t *hwif = ((ide_hwif_t *)((drive)->hwif));

        do { if (__builtin_expect(!!((drive->blocked)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (310), "i" ("drivers/ide/ide-disk.c")); } while(0);

        if (!((rq)->flags & (1 << __REQ_CMD))) {
                blk_dump_rq_flags(rq, "ide_do_rw_disk - bad command");
                ide_end_request(drive, 0, 0);
                return ide_stopped;
        }

        do { } while (0);



        if (hwif->rw_disk)
                hwif->rw_disk(drive, rq);

        return __ide_do_rw_disk(drive, rq, block);
}





static unsigned long idedisk_read_native_max_address(ide_drive_t *drive)
{
        ide_task_t args;
        unsigned long addr = 0;


        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(6)] = 0x40;
        args.tfRegister[(7)] = 0xF8;
        args.command_type = 0;
        args.handler = &task_no_data_intr;

        ide_raw_taskfile(drive, &args, ((void *)0));


        if ((args.tfRegister[(7)] & 0x01) == 0) {
                addr = ((args.tfRegister[(6)] & 0x0f) << 24)
                     | ((args.tfRegister[ (5)] ) << 16)
                     | ((args.tfRegister[ (4)] ) << 8)
                     | ((args.tfRegister[(3)] ));
                addr++;
        }
        return addr;
}

static unsigned long long idedisk_read_native_max_address_ext(ide_drive_t *drive)
{
        ide_task_t args;
        unsigned long long addr = 0;


        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));

        args.tfRegister[(6)] = 0x40;
        args.tfRegister[(7)] = 0x27;
        args.command_type = 0;
        args.handler = &task_no_data_intr;

        ide_raw_taskfile(drive, &args, ((void *)0));


        if ((args.tfRegister[(7)] & 0x01) == 0) {
                u32 high = (args.hobRegister[(5)] << 16) |
                           (args.hobRegister[(4)] << 8) |
                            args.hobRegister[(3)];
                u32 low = ((args.tfRegister[(5)])<<16) |
                           ((args.tfRegister[(4)])<<8) |
                            (args.tfRegister[(3)]);
                addr = ((__u64)high << 24) | low;
                addr++;
        }
        return addr;
}





static unsigned long idedisk_set_max_address(ide_drive_t *drive, unsigned long addr_req)
{
        ide_task_t args;
        unsigned long addr_set = 0;

        addr_req--;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(3)] = ((addr_req >> 0) & 0xff);
        args.tfRegister[(4)] = ((addr_req >> 8) & 0xff);
        args.tfRegister[(5)] = ((addr_req >> 16) & 0xff);
        args.tfRegister[(6)] = ((addr_req >> 24) & 0x0f) | 0x40;
        args.tfRegister[(7)] = 0xF9;
        args.command_type = 0;
        args.handler = &task_no_data_intr;

        ide_raw_taskfile(drive, &args, ((void *)0));

        if ((args.tfRegister[(7)] & 0x01) == 0) {
                addr_set = ((args.tfRegister[(6)] & 0x0f) << 24)
                         | ((args.tfRegister[ (5)] ) << 16)
                         | ((args.tfRegister[ (4)] ) << 8)
                         | ((args.tfRegister[(3)] ));
                addr_set++;
        }
        return addr_set;
}

static unsigned long long idedisk_set_max_address_ext(ide_drive_t *drive, unsigned long long addr_req)
{
        ide_task_t args;
        unsigned long long addr_set = 0;

        addr_req--;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(3)] = ((addr_req >> 0) & 0xff);
        args.tfRegister[(4)] = ((addr_req >>= 8) & 0xff);
        args.tfRegister[(5)] = ((addr_req >>= 8) & 0xff);
        args.tfRegister[(6)] = 0x40;
        args.tfRegister[(7)] = 0x37;
        args.hobRegister[(3)] = (addr_req >>= 8) & 0xff;
        args.hobRegister[(4)] = (addr_req >>= 8) & 0xff;
        args.hobRegister[(5)] = (addr_req >>= 8) & 0xff;
        args.hobRegister[(6)] = 0x40;
        args.hobRegister[(7)]= (drive->ctl|0x80);
        args.command_type = 0;
        args.handler = &task_no_data_intr;

        ide_raw_taskfile(drive, &args, ((void *)0));

        if ((args.tfRegister[(7)] & 0x01) == 0) {
                u32 high = (args.hobRegister[(5)] << 16) |
                           (args.hobRegister[(4)] << 8) |
                            args.hobRegister[(3)];
                u32 low = ((args.tfRegister[(5)])<<16) |
                           ((args.tfRegister[(4)])<<8) |
                            (args.tfRegister[(3)]);
                addr_set = ((__u64)high << 24) | low;
                addr_set++;
        }
        return addr_set;
}

static unsigned long long sectors_to_MB(unsigned long long n)
{
        n <<= 9;
        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (1000000); asm("":"=a" (__low), "=d" (__high):"A" (n)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (n):"a" (__low),"d" (__high)); __mod; });
        return n;
}






static inline __attribute__((always_inline)) int idedisk_supports_hpa(const struct hd_driveid *id)
{
        return (id->command_set_1 & 0x0400) && (id->cfs_enable_1 & 0x0400);
}




static inline __attribute__((always_inline)) int idedisk_supports_lba48(const struct hd_driveid *id)
{
        return (id->command_set_2 & 0x0400) && (id->cfs_enable_2 & 0x0400)
               && id->lba_capacity_2;
}

static inline __attribute__((always_inline)) void idedisk_check_hpa(ide_drive_t *drive)
{
        unsigned long long capacity, set_max;
        int lba48 = idedisk_supports_lba48(drive->id);

        capacity = drive->capacity64;
        if (lba48)
                set_max = idedisk_read_native_max_address_ext(drive);
        else
                set_max = idedisk_read_native_max_address(drive);

        if (set_max <= capacity)
                return;

        printk("<6>" "%s: Host Protected Area detected.\n"
                         "\tcurrent capacity is %llu sectors (%llu MB)\n"
                         "\tnative  capacity is %llu sectors (%llu MB)\n",
                         drive->name,
                         capacity, sectors_to_MB(capacity),
                         set_max, sectors_to_MB(set_max));

        if (lba48)
                set_max = idedisk_set_max_address_ext(drive, set_max);
        else
                set_max = idedisk_set_max_address(drive, set_max);
        if (set_max) {
                drive->capacity64 = set_max;
                printk("<6>" "%s: Host Protected Area disabled.\n",
                                 drive->name);
        }
}
# 526 "drivers/ide/ide-disk.c"
static void init_idedisk_capacity (ide_drive_t *drive)
{
        struct hd_driveid *id = drive->id;




        int hpa = idedisk_supports_hpa(id);

        if (idedisk_supports_lba48(id)) {

                drive->select.b.lba = 1;
                drive->capacity64 = id->lba_capacity_2;
                if (hpa)
                        idedisk_check_hpa(drive);
        } else if ((id->capability & 2) && lba_capacity_is_ok(id)) {

                drive->select.b.lba = 1;
                drive->capacity64 = id->lba_capacity;
                if (hpa)
                        idedisk_check_hpa(drive);
        } else {

                drive->capacity64 = drive->cyl * drive->head * drive->sect;
        }
}

static sector_t idedisk_capacity (ide_drive_t *drive)
{
        return drive->capacity64 - drive->sect0;
}



static int smart_enable(ide_drive_t *drive)
{
        ide_task_t args;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(1)] = 0xD8;
        args.tfRegister[(4)] = 0x4F;
        args.tfRegister[(5)] = 0xC2;
        args.tfRegister[(7)] = 0xB0;
        args.command_type = 0;
        args.handler = &task_no_data_intr;
        return ide_raw_taskfile(drive, &args, ((void *)0));
}

static int get_smart_values(ide_drive_t *drive, u8 *buf)
{
        ide_task_t args;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(1)] = 0xD0;
        args.tfRegister[(2)] = 0x01;
        args.tfRegister[(4)] = 0x4F;
        args.tfRegister[(5)] = 0xC2;
        args.tfRegister[(7)] = 0xB0;
        args.command_type = 2;
        args.data_phase = 0x0001;
        args.handler = &task_in_intr;
        (void) smart_enable(drive);
        return ide_raw_taskfile(drive, &args, buf);
}

static int get_smart_thresholds(ide_drive_t *drive, u8 *buf)
{
        ide_task_t args;
        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(1)] = 0xD1;
        args.tfRegister[(2)] = 0x01;
        args.tfRegister[(4)] = 0x4F;
        args.tfRegister[(5)] = 0xC2;
        args.tfRegister[(7)] = 0xB0;
        args.command_type = 2;
        args.data_phase = 0x0001;
        args.handler = &task_in_intr;
        (void) smart_enable(drive);
        return ide_raw_taskfile(drive, &args, buf);
}

static int proc_idedisk_read_cache
        (char *page, char **start, off_t off, int count, int *eof, void *data)
{
        ide_drive_t *drive = (ide_drive_t *) data;
        char *out = page;
        int len;

        if (drive->id_read)
                len = sprintf(out,"%i\n", drive->id->buf_size / 2);
        else
                len = sprintf(out,"(none)\n");
        { len -= off; if (len < count) { *eof = 1; if (len <= 0) return 0; } else len = count; *start = page + off; return len; };
}

static int proc_idedisk_read_capacity
        (char *page, char **start, off_t off, int count, int *eof, void *data)
{
        ide_drive_t*drive = (ide_drive_t *)data;
        int len;

        len = sprintf(page,"%llu\n", (long long)idedisk_capacity(drive));
        { len -= off; if (len < count) { *eof = 1; if (len <= 0) return 0; } else len = count; *start = page + off; return len; };
}

static int proc_idedisk_read_smart_thresholds
        (char *page, char **start, off_t off, int count, int *eof, void *data)
{
        ide_drive_t *drive = (ide_drive_t *)data;
        int len = 0, i = 0;

        if (!get_smart_thresholds(drive, page)) {
                unsigned short *val = (unsigned short *) page;
                char *out = ((char *)val) + ((512 / 4) * 4);
                page = out;
                do {
                        out += sprintf(out, "%04x%c", (( __u16)(__le16)(*val)), (++i & 7) ? ' ' : '\n');
                        val += 1;
                } while (i < ((512 / 4) * 2));
                len = out - page;
        }
        { len -= off; if (len < count) { *eof = 1; if (len <= 0) return 0; } else len = count; *start = page + off; return len; };
}

static int proc_idedisk_read_smart_values
        (char *page, char **start, off_t off, int count, int *eof, void *data)
{
        ide_drive_t *drive = (ide_drive_t *)data;
        int len = 0, i = 0;

        if (!get_smart_values(drive, page)) {
                unsigned short *val = (unsigned short *) page;
                char *out = ((char *)val) + ((512 / 4) * 4);
                page = out;
                do {
                        out += sprintf(out, "%04x%c", (( __u16)(__le16)(*val)), (++i & 7) ? ' ' : '\n');
                        val += 1;
                } while (i < ((512 / 4) * 2));
                len = out - page;
        }
        { len -= off; if (len < count) { *eof = 1; if (len <= 0) return 0; } else len = count; *start = page + off; return len; };
}

static ide_proc_entry_t idedisk_proc[] = {
        { "cache", 0100000|(00400|00040|00004), proc_idedisk_read_cache, ((void *)0) },
        { "capacity", 0100000|(00400|00040|00004), proc_idedisk_read_capacity, ((void *)0) },
        { "geometry", 0100000|(00400|00040|00004), proc_ide_read_geometry, ((void *)0) },
        { "smart_values", 0100000|00400, proc_idedisk_read_smart_values, ((void *)0) },
        { "smart_thresholds", 0100000|00400, proc_idedisk_read_smart_thresholds, ((void *)0) },
        { ((void *)0), 0, ((void *)0), ((void *)0) }
};







static void idedisk_end_flush(request_queue_t *q, struct request *flush_rq)
{
        ide_drive_t *drive = q->queuedata;
        struct request *rq = flush_rq->end_io_data;
        int good_sectors = rq->hard_nr_sectors;
        int bad_sectors;
        sector_t sector;

        if (flush_rq->errors & 0x04) {
                printk("<3>" "%s: barrier support doesn't work\n", drive->name);
                blk_queue_ordered(drive->queue, QUEUE_ORDERED_NONE);
                blk_queue_issue_flush_fn(drive->queue, ((void *)0));
                good_sectors = 0;
        } else if (flush_rq->errors) {
                good_sectors = 0;
                if (((rq)->flags & (1 << __REQ_BAR_PREFLUSH))) {
                        sector = ide_get_error_location(drive,flush_rq->buffer);
                        if ((sector >= rq->hard_sector) &&
                            (sector < rq->hard_sector + rq->hard_nr_sectors))
                                good_sectors = sector - rq->hard_sector;
                }
        }

        if (flush_rq->errors)
                printk("<3>" "%s: failed barrier write: "
                                "sector=%Lx(good=%d/bad=%d)\n",
                                drive->name, (unsigned long long)rq->sector,
                                good_sectors,
                                (int) (rq->hard_nr_sectors-good_sectors));

        bad_sectors = rq->hard_nr_sectors - good_sectors;

        if (good_sectors)
                __ide_end_request(drive, rq, 1, good_sectors);
        if (bad_sectors)
                __ide_end_request(drive, rq, 0, bad_sectors);
}

static int idedisk_prepare_flush(request_queue_t *q, struct request *rq)
{
        ide_drive_t *drive = q->queuedata;

        if (!drive->wcache)
                return 0;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(rq->cmd))) ? __constant_c_and_count_memset(((rq->cmd)),((0x01010101UL*(unsigned char)(0))),((sizeof(rq->cmd)))) : __constant_c_memset(((rq->cmd)),((0x01010101UL*(unsigned char)(0))),((sizeof(rq->cmd))))) : (__builtin_constant_p((sizeof(rq->cmd))) ? __memset_generic((((rq->cmd))),(((0))),(((sizeof(rq->cmd))))) : __memset_generic(((rq->cmd)),((0)),((sizeof(rq->cmd))))));

        if ((((drive->id)->cfs_enable_2 & 0x2400) == 0x2400) &&
            (drive->capacity64 >= (1UL << 28)))
                rq->cmd[0] = 0xEA;
        else
                rq->cmd[0] = 0xE7;


        rq->flags |= (1 << __REQ_DRIVE_TASK) | (1 << __REQ_SOFTBARRIER);
        rq->buffer = rq->cmd;
        return 1;
}

static int idedisk_issue_flush(request_queue_t *q, struct gendisk *disk,
                               sector_t *error_sector)
{
        ide_drive_t *drive = q->queuedata;
        struct request *rq;
        int ret;

        if (!drive->wcache)
                return 0;

        rq = blk_get_request(q, 1, 0x10u);

        idedisk_prepare_flush(q, rq);

        ret = blk_execute_rq(q, disk, rq, 0);




        if (ret && error_sector)
                *error_sector = ide_get_error_location(drive, rq->cmd);

        blk_put_request(rq);
        return ret;
}





static int set_multcount(ide_drive_t *drive, int arg)
{
        struct request rq;

        if (drive->special.b.set_multmode)
                return -16;
        ide_init_drive_cmd (&rq);
        rq.flags = (1 << __REQ_DRIVE_CMD);
        drive->mult_req = arg;
        drive->special.b.set_multmode = 1;
        (void) ide_do_drive_cmd (drive, &rq, ide_wait);
        return (drive->mult_count == arg) ? 0 : -5;
}

static int set_nowerr(ide_drive_t *drive, int arg)
{
        if (ide_spin_wait_hwgroup(drive))
                return -16;
        drive->nowerr = arg;
        drive->bad_wstat = arg ? (0x80 | 0x01) : ((0x80 | 0x01) | 0x20);
        _spin_unlock_irq(&ide_lock);
        return 0;
}

static int write_cache(ide_drive_t *drive, int arg)
{
        ide_task_t args;
        int err;

        if (!((drive->id)->cfs_enable_2 & 0x3000))
                return 1;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(1)] = (arg) ?
                        0x02 : 0x82;
        args.tfRegister[(7)] = 0xEF;
        args.command_type = 0;
        args.handler = &task_no_data_intr;

        err = ide_raw_taskfile(drive, &args, ((void *)0));
        if (err)
                return err;

        drive->wcache = arg;
        return 0;
}

static int do_idedisk_flushcache (ide_drive_t *drive)
{
        ide_task_t args;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        if ((((drive->id)->cfs_enable_2 & 0x2400) == 0x2400))
                args.tfRegister[(7)] = 0xEA;
        else
                args.tfRegister[(7)] = 0xE7;
        args.command_type = 0;
        args.handler = &task_no_data_intr;
        return ide_raw_taskfile(drive, &args, ((void *)0));
}

static int set_acoustic (ide_drive_t *drive, int arg)
{
        ide_task_t args;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
        args.tfRegister[(1)] = (arg) ? 0x42 :
                                                          0xC2;
        args.tfRegister[(2)] = arg;
        args.tfRegister[(7)] = 0xEF;
        args.command_type = 0;
        args.handler = &task_no_data_intr;
        ide_raw_taskfile(drive, &args, ((void *)0));
        drive->acoustic = arg;
        return 0;
}







static int set_lba_addressing(ide_drive_t *drive, int arg)
{
        drive->addressing = 0;

        if (((ide_hwif_t *)((drive)->hwif))->no_lba48)
                return 0;

        if (!idedisk_supports_lba48(drive->id))
                return -5;
        drive->addressing = arg;
        return 0;
}

static void idedisk_add_settings(ide_drive_t *drive)
{
        struct hd_driveid *id = drive->id;

        ide_add_setting(drive, "bios_cyl", ((1 << 0) | (1 << 1)), -1, -1, 0, 0, 65535, 1, 1, &drive->bios_cyl, ((void *)0));
        ide_add_setting(drive, "bios_head", ((1 << 0) | (1 << 1)), -1, -1, 2, 0, 255, 1, 1, &drive->bios_head, ((void *)0));
        ide_add_setting(drive, "bios_sect", ((1 << 0) | (1 << 1)), -1, -1, 2, 0, 63, 1, 1, &drive->bios_sect, ((void *)0));
        ide_add_setting(drive, "address", ((1 << 0) | (1 << 1)), 0x0310, 0x032f, 1, 0, 2, 1, 1, &drive->addressing, set_lba_addressing);
        ide_add_setting(drive, "bswap", (1 << 0), -1, -1, 2, 0, 1, 1, 1, &drive->bswap, ((void *)0));
        ide_add_setting(drive, "multcount", id ? ((1 << 0) | (1 << 1)) : (1 << 0), 0x0304, 0x0321, 2, 0, id ? id->max_multsect : 0, 1, 1, &drive->mult_count, set_multcount);
        ide_add_setting(drive, "nowerr", ((1 << 0) | (1 << 1)), 0x030a, 0x0325, 2, 0, 1, 1, 1, &drive->nowerr, set_nowerr);
        ide_add_setting(drive, "lun", ((1 << 0) | (1 << 1)), -1, -1, 0, 0, 7, 1, 1, &drive->lun, ((void *)0));
        ide_add_setting(drive, "wcache", ((1 << 0) | (1 << 1)), 0x030e, 0x032b, 2, 0, 1, 1, 1, &drive->wcache, write_cache);
        ide_add_setting(drive, "acoustic", ((1 << 0) | (1 << 1)), 0x030f, 0x032c, 2, 0, 254, 1, 1, &drive->acoustic, set_acoustic);
        ide_add_setting(drive, "failures", ((1 << 0) | (1 << 1)), -1, -1, 0, 0, 65535, 1, 1, &drive->failures, ((void *)0));
        ide_add_setting(drive, "max_failures", ((1 << 0) | (1 << 1)), -1, -1, 0, 0, 65535, 1, 1, &drive->max_failures, ((void *)0));
}

static void idedisk_setup (ide_drive_t *drive)
{
        struct hd_driveid *id = drive->id;
        unsigned long long capacity;
        int barrier;

        idedisk_add_settings(drive);

        if (drive->id_read == 0)
                return;





        if (drive->removable && !(drive->is_flash)) {



                if (id->model[0] != 'W' || id->model[1] != 'D') {
                        drive->doorlocking = 1;
                }
        }

        (void)set_lba_addressing(drive, 1);

        if (drive->addressing == 1) {
                ide_hwif_t *hwif = ((ide_hwif_t *)((drive)->hwif));
                int max_s = 2048;

                if (max_s > hwif->rqsize)
                        max_s = hwif->rqsize;

                blk_queue_max_sectors(drive->queue, max_s);
        }

        printk("<6>" "%s: max request size: %dKiB\n", drive->name, drive->queue->max_sectors / 2);


        init_idedisk_capacity (drive);


        if (drive->addressing == 0 && drive->capacity64 > 1ULL << 28) {
                printk("<4>" "%s: cannot use LBA48 - full capacity "
                       "%llu sectors (%llu MB)\n",
                       drive->name, (unsigned long long)drive->capacity64,
                       sectors_to_MB(drive->capacity64));
                drive->capacity64 = 1ULL << 28;
        }

        if (drive->hwif->no_lba48_dma && drive->addressing) {
                if (drive->capacity64 > 1ULL << 28) {
                        printk("<6>" "%s: cannot use LBA48 DMA - PIO mode will"
                                         " be used for accessing sectors > %u\n",
                                         drive->name, 1 << 28);
                } else
                        drive->addressing = 0;
        }





        capacity = idedisk_capacity (drive);
        if (!drive->forced_geom) {

                if (idedisk_supports_lba48(drive->id)) {

                        drive->bios_sect = 63;
                        drive->bios_head = 255;
                }

                if (drive->bios_sect && drive->bios_head) {
                        unsigned int cap0 = capacity;
                        unsigned int cylsz, cyl;

                        if (cap0 != capacity)
                                drive->bios_cyl = 65535;
                        else {
                                cylsz = drive->bios_sect * drive->bios_head;
                                cyl = cap0 / cylsz;
                                if (cyl > 65535)
                                        cyl = 65535;
                                if (cyl > drive->bios_cyl)
                                        drive->bios_cyl = cyl;
                        }
                }
        }
        printk("<6>" "%s: %llu sectors (%llu MB)",
                         drive->name, capacity, sectors_to_MB(capacity));


        if (id->buf_size)
                printk (" w/%dKiB Cache", id->buf_size/2);

        printk(", CHS=%d/%d/%d",
               drive->bios_cyl, drive->bios_head, drive->bios_sect);
        if (drive->using_dma)
                ide_dma_verbose(drive);
        printk("\n");

        drive->no_io_32bit = id->dword_io ? 1 : 0;


        if ((id->csfo & 1) || (id->cfs_enable_1 & (1 << 5)))
                drive->wcache = 1;

        write_cache(drive, 1);
# 1003 "drivers/ide/ide-disk.c"
        barrier = 0;
        if (((id)->cfs_enable_2 & 0x3000))
                barrier = 1;
        if (drive->addressing == 1) {

                if (capacity > (1ULL << 28) && !(((id)->cfs_enable_2 & 0x2400) == 0x2400))
                        barrier = 0;
        }

        printk("<6>" "%s: cache flushes %ssupported\n",
                drive->name, barrier ? "" : "not ");
        if (barrier) {
                blk_queue_ordered(drive->queue, QUEUE_ORDERED_FLUSH);
                drive->queue->prepare_flush_fn = idedisk_prepare_flush;
                drive->queue->end_flush_fn = idedisk_end_flush;
                blk_queue_issue_flush_fn(drive->queue, idedisk_issue_flush);
        }
}

static void ide_cacheflush_p(ide_drive_t *drive)
{
        if (!drive->wcache || !((drive->id)->cfs_enable_2 & 0x3000))
                return;

        if (do_idedisk_flushcache(drive))
                printk("<6>" "%s: wcache flush failed!\n", drive->name);
}

static int ide_disk_remove(struct device *dev)
{
        ide_drive_t *drive = ({ const typeof( ((ide_drive_t *)0)->gendev ) *__mptr = (dev); (ide_drive_t *)( (char *)__mptr - ((size_t) &((ide_drive_t *)0)->gendev) );});
        struct ide_disk_obj *idkp = drive->driver_data;
        struct gendisk *g = idkp->disk;

        ide_cacheflush_p(drive);

        ide_unregister_subdriver(drive, idkp->driver);

        del_gendisk(g);

        ide_disk_put(idkp);

        return 0;
}

static void ide_disk_release(struct kref *kref)
{
        struct ide_disk_obj *idkp = ({ const typeof( ((struct ide_disk_obj *)0)->kref ) *__mptr = (kref); (struct ide_disk_obj *)( (char *)__mptr - ((size_t) &((struct ide_disk_obj *)0)->kref) );});
        ide_drive_t *drive = idkp->drive;
        struct gendisk *g = idkp->disk;

        drive->driver_data = ((void *)0);
        drive->devfs_name[0] = '\0';
        g->private_data = ((void *)0);
        put_disk(g);
        kfree(idkp);
}

static int ide_disk_probe(struct device *dev);

static void ide_device_shutdown(struct device *dev)
{
        ide_drive_t *drive = ({ const typeof( ((ide_drive_t *)0)->gendev ) *__mptr = (dev); (ide_drive_t *)( (char *)__mptr - ((size_t) &((ide_drive_t *)0)->gendev) );});
# 1081 "drivers/ide/ide-disk.c"
        if (system_state == SYSTEM_RESTART) {

                ide_cacheflush_p(drive);
                return;
        }

        printk("Shutdown: %s\n", drive->name);
        dev->bus->suspend(dev, ((struct pm_message){ .event = 2, }));
}

static ide_driver_t idedisk_driver = {
        .owner = ((struct module *)0),
        .gen_driver = {
                .name = "ide-disk",
                .bus = &ide_bus_type,
                .probe = ide_disk_probe,
                .remove = ide_disk_remove,
                .shutdown = ide_device_shutdown,
        },
        .version = "1.18",
        .media = 0x20,
        .supports_dsc_overlap = 0,
        .do_request = ide_do_rw_disk,
        .end_request = ide_end_request,
        .error = __ide_error,
        .abort = __ide_abort,
        .proc = idedisk_proc,
};

static int idedisk_open(struct inode *inode, struct file *filp)
{
        struct gendisk *disk = inode->i_bdev->bd_disk;
        struct ide_disk_obj *idkp;
        ide_drive_t *drive;

        if (!(idkp = ide_disk_get(disk)))
                return -6;

        drive = idkp->drive;

        drive->usage++;
        if (drive->removable && drive->usage == 1) {
                ide_task_t args;
                (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
                args.tfRegister[(7)] = 0xDE;
                args.command_type = 0;
                args.handler = &task_no_data_intr;
                check_disk_change(inode->i_bdev);





                if (drive->doorlocking && ide_raw_taskfile(drive, &args, ((void *)0)))
                        drive->doorlocking = 0;
        }
        return 0;
}

static int idedisk_release(struct inode *inode, struct file *filp)
{
        struct gendisk *disk = inode->i_bdev->bd_disk;
        struct ide_disk_obj *idkp = ({ const typeof( ((struct ide_disk_obj *)0)->driver ) *__mptr = ((disk)->private_data); (struct ide_disk_obj *)( (char *)__mptr - ((size_t) &((struct ide_disk_obj *)0)->driver) );});
        ide_drive_t *drive = idkp->drive;

        if (drive->usage == 1)
                ide_cacheflush_p(drive);
        if (drive->removable && drive->usage == 1) {
                ide_task_t args;
                (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(ide_task_t))) ? __constant_c_and_count_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t)))) : __constant_c_memset(((&args)),((0x01010101UL*(unsigned char)(0))),((sizeof(ide_task_t))))) : (__builtin_constant_p((sizeof(ide_task_t))) ? __memset_generic((((&args))),(((0))),(((sizeof(ide_task_t))))) : __memset_generic(((&args)),((0)),((sizeof(ide_task_t))))));
                args.tfRegister[(7)] = 0xDF;
                args.command_type = 0;
                args.handler = &task_no_data_intr;
                if (drive->doorlocking && ide_raw_taskfile(drive, &args, ((void *)0)))
                        drive->doorlocking = 0;
        }
        drive->usage--;

        ide_disk_put(idkp);

        return 0;
}

static int idedisk_ioctl(struct inode *inode, struct file *file,
                        unsigned int cmd, unsigned long arg)
{
        struct block_device *bdev = inode->i_bdev;
        struct ide_disk_obj *idkp = ({ const typeof( ((struct ide_disk_obj *)0)->driver ) *__mptr = ((bdev->bd_disk)->private_data); (struct ide_disk_obj *)( (char *)__mptr - ((size_t) &((struct ide_disk_obj *)0)->driver) );});
        return generic_ide_ioctl(idkp->drive, file, bdev, cmd, arg);
}

static int idedisk_media_changed(struct gendisk *disk)
{
        struct ide_disk_obj *idkp = ({ const typeof( ((struct ide_disk_obj *)0)->driver ) *__mptr = ((disk)->private_data); (struct ide_disk_obj *)( (char *)__mptr - ((size_t) &((struct ide_disk_obj *)0)->driver) );});
        ide_drive_t *drive = idkp->drive;


        if (drive->attach) {
                drive->attach = 0;
                return 0;
        }

        return drive->removable;
}

static int idedisk_revalidate_disk(struct gendisk *disk)
{
        struct ide_disk_obj *idkp = ({ const typeof( ((struct ide_disk_obj *)0)->driver ) *__mptr = ((disk)->private_data); (struct ide_disk_obj *)( (char *)__mptr - ((size_t) &((struct ide_disk_obj *)0)->driver) );});
        set_capacity(disk, idedisk_capacity(idkp->drive));
        return 0;
}

static struct block_device_operations idedisk_ops = {
        .owner = ((struct module *)0),
        .open = idedisk_open,
        .release = idedisk_release,
        .ioctl = idedisk_ioctl,
        .media_changed = idedisk_media_changed,
        .revalidate_disk= idedisk_revalidate_disk
};

;

static int ide_disk_probe(struct device *dev)
{
        ide_drive_t *drive = ({ const typeof( ((ide_drive_t *)0)->gendev ) *__mptr = (dev); (ide_drive_t *)( (char *)__mptr - ((size_t) &((ide_drive_t *)0)->gendev) );});
        struct ide_disk_obj *idkp;
        struct gendisk *g;


        if (!strstr("ide-disk", drive->driver_req))
                goto failed;
        if (!drive->present)
                goto failed;
        if (drive->media != 0x20)
                goto failed;

        idkp = kmalloc(sizeof(*idkp), (0x10u | 0x40u | 0x80u));
        if (!idkp)
                goto failed;

        g = alloc_disk_node(1 << 6,
                        hwif_to_node(drive->hwif));
        if (!g)
                goto out_free_idkp;

        ide_init_disk(g, drive);

        ide_register_subdriver(drive, &idedisk_driver);

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(*idkp))) ? __constant_c_and_count_memset(((idkp)),((0x01010101UL*(unsigned char)(0))),((sizeof(*idkp)))) : __constant_c_memset(((idkp)),((0x01010101UL*(unsigned char)(0))),((sizeof(*idkp))))) : (__builtin_constant_p((sizeof(*idkp))) ? __memset_generic((((idkp))),(((0))),(((sizeof(*idkp))))) : __memset_generic(((idkp)),((0)),((sizeof(*idkp))))));

        kref_init(&idkp->kref);

        idkp->drive = drive;
        idkp->driver = &idedisk_driver;
        idkp->disk = g;

        g->private_data = &idkp->driver;

        drive->driver_data = idkp;

        idedisk_setup(drive);
        if ((!drive->head || drive->head > 16) && !drive->select.b.lba) {
                printk("<3>" "%s: INVALID GEOMETRY: %d PHYSICAL HEADS?\n",
                        drive->name, drive->head);
                drive->attach = 0;
        } else
                drive->attach = 1;

        g->minors = 1 << 6;
        strcpy(g->devfs_name, drive->devfs_name);
        g->driverfs_dev = &drive->gendev;
        g->flags = drive->removable ? 1 : 0;
        set_capacity(g, idedisk_capacity(drive));
        g->fops = &idedisk_ops;
        add_disk(g);
        return 0;

out_free_idkp:
        kfree(idkp);
failed:
        return -19;
}

static void __attribute__((__unused__)) __attribute__ ((__section__(".exit.text"))) idedisk_exit (void)
{
        driver_unregister(&idedisk_driver.gen_driver);
}

static int idedisk_init (void)
{
        return driver_register(&idedisk_driver.gen_driver);
}

static initcall_t __initcall_idedisk_init __attribute__((__unused__)) __attribute__((__section__(".initcall" "6" ".init"))) = idedisk_init;;
static exitcall_t __exitcall_idedisk_exit __attribute__((__unused__)) __attribute__ ((__section__ (".exitcall.exit"))) = idedisk_exit;;
;

struct device dev;
void * f(void * arg) {
  idedisk_init();
  ide_disk_probe(&dev);
  idedisk_exit();
  return 0;
}

typedef int pthread_t;
typedef int pthread_attr_t;

int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                   void * (*start_routine) (void*), void *arg);

int main() {
  pthread_t t;
  pthread_create(&t, (void*)0, f, (void*)0);
  pthread_create(&t, (void*)0, f, (void*)0);
  return 0;
}
