//#include <pthread.h>

typedef struct {
        unsigned long fds_bits [(1024/(8 * sizeof(unsigned long)))];
} __kernel_fd_set;

typedef void (*__kernel_sighandler_t)(int);

typedef int __kernel_key_t;
typedef int __kernel_mqd_t;

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



typedef unsigned short umode_t;

typedef __signed__ char __s8;
typedef unsigned char __u8;

typedef __signed__ short __s16;
typedef unsigned short __u16;

typedef __signed__ int __s32;
typedef unsigned int __u32;


typedef __signed__ long long __s64;
typedef unsigned long long __u64;
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
typedef __kernel_loff_t loff_t;
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
typedef unsigned long sector_t;
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
typedef struct __user_cap_header_struct {
        __u32 version;
        int pid;
} *cap_user_header_t;

typedef struct __user_cap_data_struct {
        __u32 effective;
        __u32 permitted;
        __u32 inheritable;
} *cap_user_data_t;









struct restart_block {
        long (*fn)(struct restart_block *);
        unsigned long arg0, arg1, arg2, arg3;
};

extern long do_no_restart_syscall(struct restart_block *parm);

static int generic_fls(int x) { return 32; }

static void set_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btsl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static void __set_bit(int nr, volatile unsigned long * addr)
{
        __asm__(
                "btsl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static void clear_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btrl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}

static void __clear_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__(
                "btrl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static void __change_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__(
                "btcl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static void change_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btcl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static int test_and_set_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;

        __asm__ __volatile__( "lock ; "
                "btsl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static int __test_and_set_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;

        __asm__(
                "btsl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
        return oldbit;
}
static int test_and_clear_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;

        __asm__ __volatile__( "lock ; "
                "btrl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static int __test_and_clear_bit(int nr, volatile unsigned long *addr)
{
        int oldbit;

        __asm__(
                "btrl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
        return oldbit;
}


static int __test_and_change_bit(int nr, volatile unsigned long *addr)
{
        int oldbit;

        __asm__ __volatile__(
                "btcl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static int test_and_change_bit(int nr, volatile unsigned long* addr)
{
        int oldbit;

        __asm__ __volatile__( "lock ; "
                "btcl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static int constant_test_bit(int nr, const volatile unsigned long *addr)
{
        return ((1UL << (nr & 31)) & (addr[nr >> 5])) != 0;
}

static int variable_test_bit(int nr, const volatile unsigned long * addr)
{
        int oldbit;

        __asm__ __volatile__(
                "btl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit)
                :"m" ((*(volatile long *) addr)),"Ir" (nr));
        return oldbit;
}
static int find_first_zero_bit(const unsigned long *addr, unsigned size)
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







static unsigned long __ffs(unsigned long word)
{
        __asm__("bsfl %1,%0"
                :"=r" (word)
                :"rm" (word));
        return word;
}
static int find_first_bit(const unsigned long *addr, unsigned size)
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







static unsigned long ffz(unsigned long word)
{
        __asm__("bsfl %1,%0"
                :"=r" (word)
                :"r" (~word));
        return word;
}
static int sched_find_first_bit(const unsigned long *b)
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
static int ffs(int x)
{
        int r;

        __asm__("bsfl %1,%0\n\t"
                "jnz 1f\n\t"
                "movl $-1,%0\n"
                "1:" : "=r" (r) : "rm" (x));
        return r+1;
}

static int get_bitmask_order(unsigned int count)
{
        int order;

        order = generic_fls(count);
        return order;
}






static unsigned int generic_hweight32(unsigned int w)
{
        unsigned int res = (w & 0x55555555) + ((w >> 1) & 0x55555555);
        res = (res & 0x33333333) + ((res >> 2) & 0x33333333);
        res = (res & 0x0F0F0F0F) + ((res >> 4) & 0x0F0F0F0F);
        res = (res & 0x00FF00FF) + ((res >> 8) & 0x00FF00FF);
        return (res & 0x0000FFFF) + ((res >> 16) & 0x0000FFFF);
}

static unsigned int generic_hweight16(unsigned int w)
{
        unsigned int res = (w & 0x5555) + ((w >> 1) & 0x5555);
        res = (res & 0x3333) + ((res >> 2) & 0x3333);
        res = (res & 0x0F0F) + ((res >> 4) & 0x0F0F);
        return (res & 0x00FF) + ((res >> 8) & 0x00FF);
}

static unsigned int generic_hweight8(unsigned int w)
{
        unsigned int res = (w & 0x55) + ((w >> 1) & 0x55);
        res = (res & 0x33) + ((res >> 2) & 0x33);
        return (res & 0x0F) + ((res >> 4) & 0x0F);
}

static unsigned long generic_hweight64(__u64 w)
{

        return generic_hweight32((unsigned int)(w >> 32)) +
                                generic_hweight32((unsigned int)w);
}

static unsigned long hweight_long(unsigned long w)
{
        return sizeof(w) == 4 ? generic_hweight32(w) : generic_hweight64(w);
}







static __u32 rol32(__u32 word, unsigned int shift)
{
        return (word << shift) | (word >> (32 - shift));
}







static __u32 ror32(__u32 word, unsigned int shift)
{
        return (word >> shift) | (word << (32 - shift));
}
extern int nx_enabled;

extern unsigned long long __supported_pte_mask;
typedef struct { unsigned long pte_low, pte_high; } pte_t;
typedef struct { unsigned long long pmd; } pmd_t;
typedef struct { unsigned long long pgd; } pgd_t;
typedef struct { unsigned long long pgprot; } pgprot_t;
extern unsigned int __VMALLOC_RESERVE;

extern int sysctl_legacy_va_layout;

extern int page_is_ram(unsigned long pagenr);
static __attribute__((__const__)) int get_order(unsigned long size)
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
        unsigned long flags;
        unsigned long screen_bitmap;
        unsigned long cpu_type;
        struct revectored_struct int_revectored;
        struct revectored_struct int21_revectored;
        struct vm86plus_info_struct vm86plus;
        struct pt_regs *regs32;
};

void handle_vm86_fault(struct kernel_vm86_regs *, long);
int handle_vm86_trap(struct kernel_vm86_regs *, long, int);



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



static void wrmsrl (unsigned long msr, unsigned long long val)
{
        unsigned long lo, hi;
        lo = (unsigned long) val;
        hi = val >> 32;
        __asm__ __volatile__("wrmsr" : : "c" (msr), "a" (lo), "d" (hi));
}




typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;





static __attribute__((__const__)) __u32 ___arch__swab32(__u32 x)
{



        __asm__("xchgb %b0,%h0\n\t"
                "rorl $16,%0\n\t"
                "xchgb %b0,%h0"
                :"=q" (x)
                : "0" (x));

        return x;
}

static __attribute__((__const__)) __u64 ___arch__swab64(__u64 val)
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
static __attribute__((__const__)) __u16 __fswab16(__u16 x)
{
        return ({ __u16 __tmp = (x) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); });
}
static __u16 __swab16p(const __u16 *x)
{
        return ({ __u16 __tmp = (*(x)) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); });
}
static void __swab16s(__u16 *addr)
{
        do { *(addr) = ({ __u16 __tmp = (*((addr))) ; ({ __u16 __x = (__tmp); ((__u16)( (((__u16)(__x) & (__u16)0x00ffU) << 8) | (((__u16)(__x) & (__u16)0xff00U) >> 8) )); }); }); } while (0);
}

static __attribute__((__const__)) __u32 __fswab32(__u32 x)
{
        return ___arch__swab32(x);
}
static __u32 __swab32p(const __u32 *x)
{
        return ___arch__swab32(*(x));
}
static void __swab32s(__u32 *addr)
{
        do { *(addr) = ___arch__swab32(*((addr))); } while (0);
}


static __attribute__((__const__)) __u64 __fswab64(__u64 x)
{





        return ___arch__swab64(x);

}
static __u64 __swab64p(const __u64 *x)
{
        return ___arch__swab64(*(x));
}
static void __swab64s(__u64 *addr)
{
        do { *(addr) = ___arch__swab64(*((addr))); } while (0);
}
static __le64 __cpu_to_le64p(const __u64 *p)
{
        return ( __le64)*p;
}
static __u64 __le64_to_cpup(const __le64 *p)
{
        return ( __u64)*p;
}
static __le32 __cpu_to_le32p(const __u32 *p)
{
        return ( __le32)*p;
}
static __u32 __le32_to_cpup(const __le32 *p)
{
        return ( __u32)*p;
}
static __le16 __cpu_to_le16p(const __u16 *p)
{
        return ( __le16)*p;
}
static __u16 __le16_to_cpup(const __le16 *p)
{
        return ( __u16)*p;
}
static __be64 __cpu_to_be64p(const __u64 *p)
{
        return ( __be64)__swab64p(p);
}
static __u64 __be64_to_cpup(const __be64 *p)
{
        return __swab64p((__u64 *)p);
}
static __be32 __cpu_to_be32p(const __u32 *p)
{
        return ( __be32)__swab32p(p);
}
static __u32 __be32_to_cpup(const __be32 *p)
{
        return __swab32p((__u32 *)p);
}
static __be16 __cpu_to_be16p(const __u16 *p)
{
        return ( __be16)__swab16p(p);
}
static __u16 __be16_to_cpup(const __be16 *p)
{
        return __swab16p((__u16 *)p);
}
extern __u32 ntohl(__be32);
extern __be32 htonl(__u32);
extern __u16 ntohs(__be16);
extern __be16 htons(__u16);

extern const char linux_banner[];
extern int console_printk[];






struct completion;
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
unsigned long int_sqrt(unsigned long);

static int __attribute__((pure)) long_log2(unsigned long x)
{
        int r = 0;
        for (x >>= 1; x > 0; x >>= 1)
                r++;
        return r;
}

static unsigned long __attribute__((__const__)) roundup_pow_of_two(unsigned long x)
{
        return (1UL << generic_fls(x - 1));
}

extern int printk_ratelimit(void);
extern int __printk_ratelimit(int ratelimit_jiffies, int ratelimit_burst);

static void console_silent(void)
{
        (console_printk[0]) = 0;
}

static void console_verbose(void)
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
extern void dump_stack(void);
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






struct task_struct;
extern struct task_struct * __switch_to(struct task_struct *prev, struct task_struct *next) __attribute__((regparm(3)));
static unsigned long _get_base(char * addr)
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
static unsigned long get_limit(unsigned long segment)
{
        unsigned long __limit;
        __asm__("lsll %1,%0"
                :"=r" (__limit):"r" (segment));
        return __limit+1;
}







struct __xchg_dummy { unsigned long a[100]; };
static void __set_64bit (unsigned long long * ptr,
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

static void __set_64bit_constant (unsigned long long *ptr,
                                                 unsigned long long value)
{
        __set_64bit(ptr,(unsigned int)(value), (unsigned int)((value)>>32ULL));
}



static void __set_64bit_var (unsigned long long *ptr,
                         unsigned long long value)
{
        __set_64bit(ptr,*(((unsigned int*)&(value))+0), *(((unsigned int*)&(value))+1));
}
static unsigned long __xchg(unsigned long x, volatile void * ptr, int size)
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
static unsigned long __cmpxchg(volatile void *ptr, unsigned long old,
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
void disable_hlt(void);
void enable_hlt(void);

extern int es7000_plat;
void cpu_idle_wait(void);

extern unsigned long arch_align_stack(unsigned long sp);
















extern unsigned long __per_cpu_offset[8];


extern int tsc_disable;

struct desc_struct {
        unsigned long a,b;
};
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
static void cpuid(unsigned int op, unsigned int *eax, unsigned int *ebx, unsigned int *ecx, unsigned int *edx)
{
        __asm__("cpuid"
                : "=a" (*eax),
                  "=b" (*ebx),
                  "=c" (*ecx),
                  "=d" (*edx)
                : "0" (op), "c"(0));
}


static void cpuid_count(int op, int count, int *eax, int *ebx, int *ecx,
                int *edx)
{
        __asm__("cpuid"
                : "=a" (*eax),
                  "=b" (*ebx),
                  "=c" (*ecx),
                  "=d" (*edx)
                : "0" (op), "c" (count));
}




static unsigned int cpuid_eax(unsigned int op)
{
        unsigned int eax;

        __asm__("cpuid"
                : "=a" (eax)
                : "0" (op)
                : "bx", "cx", "dx");
        return eax;
}
static unsigned int cpuid_ebx(unsigned int op)
{
        unsigned int eax, ebx;

        __asm__("cpuid"
                : "=a" (eax), "=b" (ebx)
                : "0" (op)
                : "cx", "dx" );
        return ebx;
}
static unsigned int cpuid_ecx(unsigned int op)
{
        unsigned int eax, ecx;

        __asm__("cpuid"
                : "=a" (eax), "=c" (ecx)
                : "0" (op)
                : "bx", "dx" );
        return ecx;
}
static unsigned int cpuid_edx(unsigned int op)
{
        unsigned int eax, edx;

        __asm__("cpuid"
                : "=a" (eax), "=d" (edx)
                : "0" (op)
                : "bx", "cx");
        return edx;
}
extern unsigned long mmu_cr4_features;

static void set_in_cr4 (unsigned long mask)
{
        unsigned cr4;
        mmu_cr4_features |= mask;
        cr4 = ({ unsigned int __dummy; __asm__( "movl %%cr4,%0\n\t" :"=r" (__dummy)); __dummy; });
        cr4 |= mask;
        __asm__ __volatile__("movl %0,%%cr4": :"r" (cr4));;
}

static void clear_in_cr4 (unsigned long mask)
{
        unsigned cr4;
        mmu_cr4_features &= ~mask;
        cr4 = ({ unsigned int __dummy; __asm__( "movl %%cr4,%0\n\t" :"=r" (__dummy)); __dummy; });
        cr4 &= ~mask;
        __asm__ __volatile__("movl %0,%%cr4": :"r" (cr4));;
}
static void serialize_cpu(void)
{
         __asm__ __volatile__ ("cpuid" : : : "ax", "bx", "cx", "dx");
}

static void __monitor(const void *eax, unsigned long ecx,
                unsigned long edx)
{

        asm volatile(
                ".byte 0x0f,0x01,0xc8;"
                : :"a" (eax), "c" (ecx), "d"(edx));
}

static void __mwait(unsigned long eax, unsigned long ecx)
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
static void load_esp0(struct tss_struct *tss, struct thread_struct *thread)
{
        tss->esp0 = thread->esp0;

        if (__builtin_expect(!!(tss->ss1 != thread->sysenter_cs), 0)) {
                tss->ss1 = thread->sysenter_cs;
                __asm__ __volatile__("wrmsr" : : "c" (0x174), "a" (thread->sysenter_cs), "d" (0));
        }
}
static void set_iopl_mask(unsigned mask)
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




static void rep_nop(void)
{
        __asm__ __volatile__("rep;nop": : :"memory");
}
static void prefetch(const void *x)
{
        asm volatile ("661:\n\t" ".byte 0x8d,0x74,0x26,0x00\n" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "prefetchnta (%1)" "\n664:\n" ".previous" :: "i" ((0*32+25)), "r" (x));



}







static void prefetchw(const void *x)
{
        asm volatile ("661:\n\t" ".byte 0x8d,0x74,0x26,0x00\n" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "prefetchw (%1)" "\n664:\n" ".previous" :: "i" ((1*32+31)), "r" (x));



}


extern void select_idle_routine(const struct cpuinfo_x86 *c);



extern unsigned long boot_option_idle_override;
extern void enable_sep_cpu(void);
extern int sysenter_setup(void);


extern void mtrr_ap_init(void);
extern void mtrr_bp_init(void);
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
static struct thread_info *current_thread_info(void)
{
        struct thread_info *ti;
        __asm__("andl %%esp,%0; ":"=r" (ti) : "0" (~((8192) - 1)));
        return ti;
}


static void set_thread_flag(int flag)
{
        set_bit(flag,&current_thread_info()->flags);
}

static void clear_thread_flag(int flag)
{
        clear_bit(flag,&current_thread_info()->flags);
}

static int test_and_set_thread_flag(int flag)
{
        return test_and_set_bit(flag,&current_thread_info()->flags);
}

static int test_and_clear_thread_flag(int flag)
{
        return test_and_clear_bit(flag,&current_thread_info()->flags);
}

static int test_thread_flag(int flag)
{
        return (__builtin_constant_p(flag) ? constant_test_bit((flag),(&current_thread_info()->flags)) : variable_test_bit((flag),(&current_thread_info()->flags)));
}

static void set_ti_thread_flag(struct thread_info *ti, int flag)
{
        set_bit(flag,&ti->flags);
}

static void clear_ti_thread_flag(struct thread_info *ti, int flag)
{
        clear_bit(flag,&ti->flags);
}

static int test_and_set_ti_thread_flag(struct thread_info *ti, int flag)
{
        return test_and_set_bit(flag,&ti->flags);
}

static int test_and_clear_ti_thread_flag(struct thread_info *ti, int flag)
{
        return test_and_clear_bit(flag,&ti->flags);
}

static int test_ti_thread_flag(struct thread_info *ti, int flag)
{
        return (__builtin_constant_p(flag) ? constant_test_bit((flag),(&ti->flags)) : variable_test_bit((flag),(&ti->flags)));
}

static void set_need_resched(void)
{
        set_thread_flag(3);
}

static void clear_need_resched(void)
{
        clear_thread_flag(3);
}








typedef struct {
        volatile unsigned int slock;
} raw_spinlock_t;



typedef struct {
        volatile unsigned int lock;
} raw_rwlock_t;




typedef struct {
        raw_spinlock_t raw_lock;







} spinlock_t;



typedef struct {
        raw_rwlock_t raw_lock;







} rwlock_t;

extern int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) generic__raw_read_trylock(raw_rwlock_t *lock);








typedef struct { volatile int counter; } atomic_t;
static void atomic_add(int i, atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "addl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}
static void atomic_sub(int i, atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "subl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}
static int atomic_sub_and_test(int i, atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "subl %2,%0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"ir" (i), "m" (v->counter) : "memory");
        return c;
}







static void atomic_inc(atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "incl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}







static void atomic_dec(atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "decl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}
static int atomic_dec_and_test(atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "decl %0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"m" (v->counter) : "memory");
        return c != 0;
}
static int atomic_inc_and_test(atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "incl %0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"m" (v->counter) : "memory");
        return c != 0;
}
static int atomic_add_negative(int i, atomic_t *v)
{
        unsigned char c;

        __asm__ __volatile__(
                "lock ; " "addl %2,%0; sets %1"
                :"=m" (v->counter), "=qm" (c)
                :"ir" (i), "m" (v->counter) : "memory");
        return c;
}
static int atomic_add_return(int i, atomic_t *v)
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

static int atomic_sub_return(int i, atomic_t *v)
{
        return atomic_add_return(-i,v);
}
static void __raw_spin_lock(raw_spinlock_t *lock)
{
        __asm__ __volatile__(
                "\n1:\t" "lock ; decb %0\n\t" "jns 3f\n" "2:\t" "rep;nop\n\t" "cmpb $0,%0\n\t" "jle 2b\n\t" "jmp 1b\n" "3:\n\t"
                :"=m" (lock->slock) : : "memory");
}

static void __raw_spin_lock_flags(raw_spinlock_t *lock, unsigned long flags)
{
        __asm__ __volatile__(
                "\n1:\t" "lock ; decb %0\n\t" "jns 4f\n\t" "2:\t" "testl $0x200, %1\n\t" "jz 3f\n\t" "sti\n\t" "3:\t" "rep;nop\n\t" "cmpb $0, %0\n\t" "jle 3b\n\t" "cli\n\t" "jmp 1b\n" "4:\n\t"
                :"=m" (lock->slock) : "r" (flags) : "memory");
}

static int __raw_spin_trylock(raw_spinlock_t *lock)
{
        char oldval;
        __asm__ __volatile__(
                "xchgb %b0,%1"
                :"=q" (oldval), "=m" (lock->slock)
                :"0" (0) : "memory");
        return oldval > 0;
}
static void __raw_spin_unlock(raw_spinlock_t *lock)
{
        char oldval = 1;

        __asm__ __volatile__(
                "xchgb %b0, %1" :"=q" (oldval), "=m" (lock->slock) :"0" (oldval) : "memory"
        );
}
static void __raw_read_lock(raw_rwlock_t *rw)
{
        do { if (__builtin_constant_p(rw)) asm volatile("lock ; " "subl $1,%0\n\t" "jns 1f\n" "pushl %%eax\n\t" "leal %0,%%eax\n\t" "call " "__read_lock_failed" "\n\t" "popl %%eax\n\t" "1:\n" :"=m" (*(volatile int *)rw) : : "memory"); else asm volatile("lock ; " "subl $1,(%0)\n\t" "jns 1f\n" "call " "__read_lock_failed" "\n\t" "1:\n" ::"a" (rw) : "memory"); } while (0);
}

static void __raw_write_lock(raw_rwlock_t *rw)
{
        do { if (__builtin_constant_p(rw)) asm volatile("lock ; " "subl $" "0x01000000" ",%0\n\t" "jz 1f\n" "pushl %%eax\n\t" "leal %0,%%eax\n\t" "call " "__write_lock_failed" "\n\t" "popl %%eax\n\t" "1:\n" :"=m" (*(volatile int *)rw) : : "memory"); else asm volatile("lock ; " "subl $" "0x01000000" ",(%0)\n\t" "jz 1f\n" "call " "__write_lock_failed" "\n\t" "1:\n" ::"a" (rw) : "memory"); } while (0);
}

static int __raw_read_trylock(raw_rwlock_t *lock)
{
        atomic_t *count = (atomic_t *)lock;
        atomic_dec(count);
        if (((count)->counter) >= 0)
                return 1;
        atomic_inc(count);
        return 0;
}

static int __raw_write_trylock(raw_rwlock_t *lock)
{
        atomic_t *count = (atomic_t *)lock;
        if (atomic_sub_and_test(0x01000000, count))
                return 1;
        atomic_add(0x01000000, count);
        return 0;
}

static void __raw_read_unlock(raw_rwlock_t *rw)
{
        asm volatile("lock ; incl %0" :"=m" (rw->lock) : : "memory");
}

static void __raw_write_unlock(raw_rwlock_t *rw)
{
        asm volatile("lock ; addl $" "0x01000000" ", %0"
                                 : "=m" (rw->lock) : : "memory");
}





void spin_lock_init(spinlock_t *lock);
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
extern int _atomic_dec_and_lock(atomic_t *atomic, spinlock_t *lock);
typedef __u32 kernel_cap_t;
extern kernel_cap_t cap_bset;
static kernel_cap_t cap_combine(kernel_cap_t a, kernel_cap_t b)
{
     kernel_cap_t dest;
     (dest) = (a) | (b);
     return dest;
}

static kernel_cap_t cap_intersect(kernel_cap_t a, kernel_cap_t b)
{
     kernel_cap_t dest;
     (dest) = (a) & (b);
     return dest;
}

static kernel_cap_t cap_drop(kernel_cap_t a, kernel_cap_t drop)
{
     kernel_cap_t dest;
     (dest) = (a) & ~(drop);
     return dest;
}

static kernel_cap_t cap_invert(kernel_cap_t c)
{
     kernel_cap_t dest;
     (dest) = ~(c);
     return dest;
}









typedef struct {
        unsigned sequence;
        spinlock_t lock;
} seqlock_t;
static void write_seqlock(seqlock_t *sl)
{
        _spin_lock(&sl->lock);
        ++sl->sequence;
        __asm__ __volatile__ ("": : :"memory");
}

static void write_sequnlock(seqlock_t *sl)
{
        __asm__ __volatile__ ("": : :"memory");
        sl->sequence++;
        _spin_unlock(&sl->lock);
}

static int write_tryseqlock(seqlock_t *sl)
{
        int ret = (_spin_trylock(&sl->lock));

        if (ret) {
                ++sl->sequence;
                __asm__ __volatile__ ("": : :"memory");
        }
        return ret;
}


static unsigned read_seqbegin(const seqlock_t *sl)
{
        unsigned ret = sl->sequence;
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return ret;
}
static int read_seqretry(const seqlock_t *sl, unsigned iv)
{
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return (iv & 1) | (sl->sequence ^ iv);
}
typedef struct seqcount {
        unsigned sequence;
} seqcount_t;





static unsigned read_seqcount_begin(const seqcount_t *s)
{
        unsigned ret = s->sequence;
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return ret;
}






static int read_seqcount_retry(const seqcount_t *s, unsigned iv)
{
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        return (iv & 1) | (s->sequence ^ iv);
}






static void write_seqcount_begin(seqcount_t *s)
{
        s->sequence++;
        __asm__ __volatile__ ("": : :"memory");
}

static void write_seqcount_end(seqcount_t *s)
{
        __asm__ __volatile__ ("": : :"memory");
        s->sequence++;
}




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
static int timespec_equal(struct timespec *a, struct timespec *b)
{
        return (a->tv_sec == b->tv_sec) && (a->tv_nsec == b->tv_nsec);
}
static unsigned long
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

static unsigned long get_seconds(void)
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

static void
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
struct itimerspec {
        struct timespec it_interval;
        struct timespec it_value;
};

struct itimerval {
        struct timeval it_interval;
        struct timeval it_value;
};

typedef unsigned long long cycles_t;

static cycles_t get_cycles (void)
{
        unsigned long long ret=0;


        if (!(__builtin_constant_p((0*32+ 4)) ? constant_test_bit(((0*32+ 4)),(boot_cpu_data.x86_capability)) : variable_test_bit(((0*32+ 4)),(boot_cpu_data.x86_capability))))
                return 0;





        return ret;
}

extern unsigned int cpu_khz;

extern int read_current_timer(unsigned long *timer_value);
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






static void ntp_clear(void)
{
        time_adjust = 0;
        time_status |= 0x0040;
        time_maxerror = (512000L << 5);
        time_esterror = (512000L << 5);
}





static int ntp_synced(void)
{
        return !(time_status & 0x0040);
}
static void
time_interpolator_reset(void)
{
}







static long
div_ll_X_l_rem(long long divs, long div, long *rem)
{
        long dum2;
      __asm__("divl %2":"=a"(dum2), "=d"(*rem)
      : "rm"(div), "A"(divs));

        return dum2;

}
extern u64 __attribute__((section(".data"))) jiffies_64;
extern unsigned long volatile __attribute__((section(".data"))) jiffies;


u64 get_jiffies_64(void);
static unsigned int jiffies_to_msecs(const unsigned long j)
{

        return ((1000L) / 250) * j;





}

static unsigned int jiffies_to_usecs(const unsigned long j)
{

        return ((1000000L) / 250) * j;





}

static unsigned long msecs_to_jiffies(const unsigned int m)
{
        if (m > jiffies_to_msecs(((~0UL >> 1)-1)))
                return ((~0UL >> 1)-1);

        return (m + ((1000L) / 250) - 1) / ((1000L) / 250);





}

static unsigned long usecs_to_jiffies(const unsigned int u)
{
        if (u > jiffies_to_usecs(((~0UL >> 1)-1)))
                return ((~0UL >> 1)-1);

        return (u + ((1000000L) / 250) - 1) / ((1000000L) / 250);





}
static unsigned long
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

static void
jiffies_to_timespec(const unsigned long jiffies, struct timespec *value)
{




        u64 nsec = (u64)jiffies * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        value->tv_sec = div_ll_X_l_rem(nsec,(1000000000L),&value->tv_nsec);
}
static unsigned long
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

static void
jiffies_to_timeval(const unsigned long jiffies, struct timeval *value)
{




        u64 nsec = (u64)jiffies * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        value->tv_sec = div_ll_X_l_rem(nsec,(1000000000L),&value->tv_usec);
        value->tv_usec /= (1000L);
}




static clock_t jiffies_to_clock_t(long x)
{



        u64 tmp = (u64)x * (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (((1000000000L) / 100)); asm("":"=a" (__low), "=d" (__high):"A" (tmp)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (tmp):"a" (__low),"d" (__high)); __mod; });
        return (long)tmp;

}

static unsigned long clock_t_to_jiffies(unsigned long x)
{





        u64 jif;


        if (x >= ~0UL / 250 * 100)
                return ~0UL;


        jif = x * (u64) 250;
        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (100); asm("":"=a" (__low), "=d" (__high):"A" (jif)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (jif):"a" (__low),"d" (__high)); __mod; });
        return jif;

}

static u64 jiffies_64_to_clock_t(u64 x)
{
        x *= (( ((1000000UL * 1000 / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (((1000000UL * 1000 % (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))) << 8) + (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250))) / 2) / (( ((1193182 / ((1193182 + 250/2) / 250)) << 8) + (((1193182 % ((1193182 + 250/2) / 250)) << 8) + ((1193182 + 250/2) / 250) / 2) / ((1193182 + 250/2) / 250)))));
        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (((1000000000L) / 100)); asm("":"=a" (__low), "=d" (__high):"A" (x)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (x):"a" (__low),"d" (__high)); __mod; });

        return x;
}

static u64 nsec_to_clock_t(u64 x)
{

        ({ unsigned long __upper, __low, __high, __mod, __base; __base = (((1000000000L) / 100)); asm("":"=a" (__low), "=d" (__high):"A" (x)); __upper = __high; if (__high) { __upper = __high % (__base); __high = __high / (__base); } asm("divl %2":"=a" (__low), "=d" (__mod):"rm" (__base), "0" (__low), "1" (__upper)); asm("":"=A" (x):"a" (__low),"d" (__high)); __mod; });
        return x;
}
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

static void rb_link_node(struct rb_node * node, struct rb_node * parent,
                                struct rb_node ** rb_link)
{
        node->rb_parent = parent;
        node->rb_color = 0;
        node->rb_left = node->rb_right = ((void *)0);

        *rb_link = node;
}








extern char * strpbrk(const char *,const char *);
extern char * strsep(char **,const char *);
extern __kernel_size_t strspn(const char *,const char *);
extern __kernel_size_t strcspn(const char *,const char *);




static char * strcpy(char * dest,const char *src)
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


static char * strncpy(char * dest,const char *src,size_t count)
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


static char * strcat(char * dest,const char * src)
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


static char * strncat(char * dest,const char * src,size_t count)
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


static int strcmp(const char * cs,const char * ct)
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


static int strncmp(const char * cs,const char * ct,size_t count)
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


static char * strchr(const char * s, int c)
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


static char * strrchr(const char * s, int c)
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


static size_t strlen(const char * s)
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

static void * __memcpy(void * to, const void * from, size_t n)
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





static void * __constant_memcpy(void * to, const void * from, size_t n)
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
void *memmove(void * dest,const void * src, size_t n);




static void * memchr(const void * cs,int c,size_t count)
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

static void * __memset_generic(void * s, char c,size_t count)
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
static void * __constant_c_memset(void * s, unsigned long c, size_t count)
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



static size_t strnlen(const char * s, size_t count)
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





static void * __constant_c_and_count_memset(void * s, unsigned long pattern, size_t count)
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
static void * memscan(void * addr, int c, size_t size)
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
size_t strlcpy(char *, const char *, size_t);
extern size_t strlcat(char *, const char *, __kernel_size_t);
extern int strnicmp(const char *, const char *, __kernel_size_t);





extern char * strnchr(const char *, size_t, int);
extern int __builtin_memcmp(const void *,const void *,__kernel_size_t);





extern char *kstrdup(const char *s, gfp_t gfp);
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







static void bitmap_zero(unsigned long *dst, int nbits)
{
        if (nbits <= 32)
                *dst = 0UL;
        else {
                int len = (((nbits)+32 -1)/32) * sizeof(unsigned long);
                (__builtin_constant_p(0) ? (__builtin_constant_p((len)) ? __constant_c_and_count_memset(((dst)),((0x01010101UL*(unsigned char)(0))),((len))) : __constant_c_memset(((dst)),((0x01010101UL*(unsigned char)(0))),((len)))) : (__builtin_constant_p((len)) ? __memset_generic((((dst))),(((0))),(((len)))) : __memset_generic(((dst)),((0)),((len)))));
        }
}

static void bitmap_fill(unsigned long *dst, int nbits)
{
        size_t nlongs = (((nbits)+32 -1)/32);
        if (nlongs > 1) {
                int len = (nlongs - 1) * sizeof(unsigned long);
                (__builtin_constant_p(0xff) ? (__builtin_constant_p((len)) ? __constant_c_and_count_memset(((dst)),((0x01010101UL*(unsigned char)(0xff))),((len))) : __constant_c_memset(((dst)),((0x01010101UL*(unsigned char)(0xff))),((len)))) : (__builtin_constant_p((len)) ? __memset_generic((((dst))),(((0xff))),(((len)))) : __memset_generic(((dst)),((0xff)),((len)))));
        }
        dst[nlongs - 1] = ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL );
}

static void bitmap_copy(unsigned long *dst, const unsigned long *src,
                        int nbits)
{
        if (nbits <= 32)
                *dst = *src;
        else {
                int len = (((nbits)+32 -1)/32) * sizeof(unsigned long);
                (__builtin_constant_p(len) ? __constant_memcpy((dst),(src),(len)) : __memcpy((dst),(src),(len)));
        }
}

static void bitmap_and(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 & *src2;
        else
                __bitmap_and(dst, src1, src2, nbits);
}

static void bitmap_or(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 | *src2;
        else
                __bitmap_or(dst, src1, src2, nbits);
}

static void bitmap_xor(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 ^ *src2;
        else
                __bitmap_xor(dst, src1, src2, nbits);
}

static void bitmap_andnot(unsigned long *dst, const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                *dst = *src1 & ~(*src2);
        else
                __bitmap_andnot(dst, src1, src2, nbits);
}

static void bitmap_complement(unsigned long *dst, const unsigned long *src,
                        int nbits)
{
        if (nbits <= 32)
                *dst = ~(*src) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL );
        else
                __bitmap_complement(dst, src, nbits);
}

static int bitmap_equal(const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                return ! ((*src1 ^ *src2) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_equal(src1, src2, nbits);
}

static int bitmap_intersects(const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                return ((*src1 & *src2) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL )) != 0;
        else
                return __bitmap_intersects(src1, src2, nbits);
}

static int bitmap_subset(const unsigned long *src1,
                        const unsigned long *src2, int nbits)
{
        if (nbits <= 32)
                return ! ((*src1 & ~(*src2)) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_subset(src1, src2, nbits);
}

static int bitmap_empty(const unsigned long *src, int nbits)
{
        if (nbits <= 32)
                return ! (*src & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_empty(src, nbits);
}

static int bitmap_full(const unsigned long *src, int nbits)
{
        if (nbits <= 32)
                return ! (~(*src) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL ));
        else
                return __bitmap_full(src, nbits);
}

static int bitmap_weight(const unsigned long *src, int nbits)
{
        return __bitmap_weight(src, nbits);
}

static void bitmap_shift_right(unsigned long *dst,
                        const unsigned long *src, int n, int nbits)
{
        if (nbits <= 32)
                *dst = *src >> n;
        else
                __bitmap_shift_right(dst, src, n, nbits);
}

static void bitmap_shift_left(unsigned long *dst,
                        const unsigned long *src, int n, int nbits)
{
        if (nbits <= 32)
                *dst = (*src << n) & ( ((nbits) % 32) ? (1UL<<((nbits) % 32))-1 : ~0UL );
        else
                __bitmap_shift_left(dst, src, n, nbits);
}


typedef struct { unsigned long bits[(((8)+32 -1)/32)]; } cpumask_t;
extern cpumask_t _unused_cpumask_arg_;


static void __cpu_set(int cpu, volatile cpumask_t *dstp)
{
        set_bit(cpu, dstp->bits);
}


static void __cpu_clear(int cpu, volatile cpumask_t *dstp)
{
        clear_bit(cpu, dstp->bits);
}


static void __cpus_setall(cpumask_t *dstp, int nbits)
{
        bitmap_fill(dstp->bits, nbits);
}


static void __cpus_clear(cpumask_t *dstp, int nbits)
{
        bitmap_zero(dstp->bits, nbits);
}





static int __cpu_test_and_set(int cpu, cpumask_t *addr)
{
        return test_and_set_bit(cpu, addr->bits);
}


static void __cpus_and(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_and(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static void __cpus_or(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_or(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static void __cpus_xor(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_xor(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static void __cpus_andnot(cpumask_t *dstp, const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        bitmap_andnot(dstp->bits, src1p->bits, src2p->bits, nbits);
}


static void __cpus_complement(cpumask_t *dstp,
                                        const cpumask_t *srcp, int nbits)
{
        bitmap_complement(dstp->bits, srcp->bits, nbits);
}


static int __cpus_equal(const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        return bitmap_equal(src1p->bits, src2p->bits, nbits);
}


static int __cpus_intersects(const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        return bitmap_intersects(src1p->bits, src2p->bits, nbits);
}


static int __cpus_subset(const cpumask_t *src1p,
                                        const cpumask_t *src2p, int nbits)
{
        return bitmap_subset(src1p->bits, src2p->bits, nbits);
}


static int __cpus_empty(const cpumask_t *srcp, int nbits)
{
        return bitmap_empty(srcp->bits, nbits);
}


static int __cpus_full(const cpumask_t *srcp, int nbits)
{
        return bitmap_full(srcp->bits, nbits);
}


static int __cpus_weight(const cpumask_t *srcp, int nbits)
{
        return bitmap_weight(srcp->bits, nbits);
}



static void __cpus_shift_right(cpumask_t *dstp,
                                        const cpumask_t *srcp, int n, int nbits)
{
        bitmap_shift_right(dstp->bits, srcp->bits, n, nbits);
}



static void __cpus_shift_left(cpumask_t *dstp,
                                        const cpumask_t *srcp, int n, int nbits)
{
        bitmap_shift_left(dstp->bits, srcp->bits, n, nbits);
}


static int __first_cpu(const cpumask_t *srcp, int nbits)
{
        return ({ int __x = (nbits); int __y = (find_first_bit(srcp->bits, nbits)); __x < __y ? __x: __y; });
}


static int __next_cpu(int n, const cpumask_t *srcp, int nbits)
{
        return ({ int __x = (nbits); int __y = (find_next_bit(srcp->bits, nbits, n+1)); __x < __y ? __x: __y; });
}
static int __cpumask_scnprintf(char *buf, int len,
                                        const cpumask_t *srcp, int nbits)
{
        return bitmap_scnprintf(buf, len, srcp->bits, nbits);
}



static int __cpumask_parse(const char *buf, int len,
                                        cpumask_t *dstp, int nbits)
{
        return bitmap_parse(buf, len, dstp->bits, nbits);
}



static int __cpulist_scnprintf(char *buf, int len,
                                        const cpumask_t *srcp, int nbits)
{
        return bitmap_scnlistprintf(buf, len, srcp->bits, nbits);
}


static int __cpulist_parse(const char *buf, cpumask_t *dstp, int nbits)
{
        return bitmap_parselist(buf, dstp->bits, nbits);
}
extern cpumask_t cpu_possible_map;
extern cpumask_t cpu_online_map;
extern cpumask_t cpu_present_map;











typedef struct { unsigned long bits[((((1 << 0))+32 -1)/32)]; } nodemask_t;
extern nodemask_t _unused_nodemask_arg_;


static void __node_set(int node, volatile nodemask_t *dstp)
{
        set_bit(node, dstp->bits);
}


static void __node_clear(int node, volatile nodemask_t *dstp)
{
        clear_bit(node, dstp->bits);
}


static void __nodes_setall(nodemask_t *dstp, int nbits)
{
        bitmap_fill(dstp->bits, nbits);
}


static void __nodes_clear(nodemask_t *dstp, int nbits)
{
        bitmap_zero(dstp->bits, nbits);
}






static int __node_test_and_set(int node, nodemask_t *addr)
{
        return test_and_set_bit(node, addr->bits);
}



static void __nodes_and(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_and(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static void __nodes_or(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_or(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static void __nodes_xor(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_xor(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static void __nodes_andnot(nodemask_t *dstp, const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        bitmap_andnot(dstp->bits, src1p->bits, src2p->bits, nbits);
}



static void __nodes_complement(nodemask_t *dstp,
                                        const nodemask_t *srcp, int nbits)
{
        bitmap_complement(dstp->bits, srcp->bits, nbits);
}



static int __nodes_equal(const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        return bitmap_equal(src1p->bits, src2p->bits, nbits);
}



static int __nodes_intersects(const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        return bitmap_intersects(src1p->bits, src2p->bits, nbits);
}



static int __nodes_subset(const nodemask_t *src1p,
                                        const nodemask_t *src2p, int nbits)
{
        return bitmap_subset(src1p->bits, src2p->bits, nbits);
}


static int __nodes_empty(const nodemask_t *srcp, int nbits)
{
        return bitmap_empty(srcp->bits, nbits);
}


static int __nodes_full(const nodemask_t *srcp, int nbits)
{
        return bitmap_full(srcp->bits, nbits);
}


static int __nodes_weight(const nodemask_t *srcp, int nbits)
{
        return bitmap_weight(srcp->bits, nbits);
}



static void __nodes_shift_right(nodemask_t *dstp,
                                        const nodemask_t *srcp, int n, int nbits)
{
        bitmap_shift_right(dstp->bits, srcp->bits, n, nbits);
}



static void __nodes_shift_left(nodemask_t *dstp,
                                        const nodemask_t *srcp, int n, int nbits)
{
        bitmap_shift_left(dstp->bits, srcp->bits, n, nbits);
}





static int __first_node(const nodemask_t *srcp)
{
        return ({ int __x = ((1 << 0)); int __y = (find_first_bit(srcp->bits, (1 << 0))); __x < __y ? __x: __y; });
}


static int __next_node(int n, const nodemask_t *srcp)
{
        return ({ int __x = ((1 << 0)); int __y = (find_next_bit(srcp->bits, (1 << 0), n+1)); __x < __y ? __x: __y; });
}
static int __first_unset_node(const nodemask_t *maskp)
{
        return ({ int __x = ((1 << 0)); int __y = (find_first_zero_bit(maskp->bits, (1 << 0))); __x < __y ? __x: __y; });

}
static int __nodemask_scnprintf(char *buf, int len,
                                        const nodemask_t *srcp, int nbits)
{
        return bitmap_scnprintf(buf, len, srcp->bits, nbits);
}



static int __nodemask_parse(const char *buf, int len,
                                        nodemask_t *dstp, int nbits)
{
        return bitmap_parse(buf, len, dstp->bits, nbits);
}



static int __nodelist_scnprintf(char *buf, int len,
                                        const nodemask_t *srcp, int nbits)
{
        return bitmap_scnlistprintf(buf, len, srcp->bits, nbits);
}


static int __nodelist_parse(const char *buf, nodemask_t *dstp, int nbits)
{
        return bitmap_parselist(buf, dstp->bits, nbits);
}
extern nodemask_t node_online_map;
extern nodemask_t node_possible_map;








static void prefetch_range(void *addr, size_t len)
{

        char *cp;
        char *end = addr + len;

        for (cp = addr; cp < end; cp += (4*(1 << (4))))
                prefetch(cp);

}
struct list_head {
        struct list_head *next, *prev;
};
static void __list_add(struct list_head *new,
                              struct list_head *prev,
                              struct list_head *next)
{
        next->prev = new;
        new->next = next;
        new->prev = prev;
        prev->next = new;
}
static void list_add(struct list_head *new, struct list_head *head)
{
        __list_add(new, head, head->next);
}
static void list_add_tail(struct list_head *new, struct list_head *head)
{
        __list_add(new, head->prev, head);
}







static void __list_add_rcu(struct list_head * new,
                struct list_head * prev, struct list_head * next)
{
        new->next = next;
        new->prev = prev;
        __asm__ __volatile__ ("": : :"memory");
        next->prev = new;
        prev->next = new;
}
static void list_add_rcu(struct list_head *new, struct list_head *head)
{
        __list_add_rcu(new, head, head->next);
}
static void list_add_tail_rcu(struct list_head *new,
                                        struct list_head *head)
{
        __list_add_rcu(new, head->prev, head);
}
static void __list_del(struct list_head * prev, struct list_head * next)
{
        next->prev = prev;
        prev->next = next;
}







static void list_del(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
        entry->next = ((void *) 0x00100100);
        entry->prev = ((void *) 0x00200200);
}
static void list_del_rcu(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
        entry->prev = ((void *) 0x00200200);
}
static void list_replace_rcu(struct list_head *old, struct list_head *new){
        new->next = old->next;
        new->prev = old->prev;
        __asm__ __volatile__ ("": : :"memory");
        new->next->prev = new;
        new->prev->next = new;
}





static void list_del_init(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
        do { (entry)->next = (entry); (entry)->prev = (entry); } while (0);
}






static void list_move(struct list_head *list, struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add(list, head);
}






static void list_move_tail(struct list_head *list,
                                  struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add_tail(list, head);
}





static int list_empty(const struct list_head *head)
{
        return head->next == head;
}
static int list_empty_careful(const struct list_head *head)
{
        struct list_head *next = head->next;
        return (next == head) && (next == head->prev);
}

static void __list_splice(struct list_head *list,
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






static void list_splice(struct list_head *list, struct list_head *head)
{
        if (!list_empty(list))
                __list_splice(list, head);
}
static void list_splice_init(struct list_head *list,
                                    struct list_head *head)
{
        if (!list_empty(list)) {
                __list_splice(list, head);
                do { (list)->next = (list); (list)->prev = (list); } while (0);
        }
}
struct hlist_head {
        struct hlist_node *first;
};

struct hlist_node {
        struct hlist_node *next, **pprev;
};






static int hlist_unhashed(const struct hlist_node *h)
{
        return !h->pprev;
}

static int hlist_empty(const struct hlist_head *h)
{
        return !h->first;
}

static void __hlist_del(struct hlist_node *n)
{
        struct hlist_node *next = n->next;
        struct hlist_node **pprev = n->pprev;
        *pprev = next;
        if (next)
                next->pprev = pprev;
}

static void hlist_del(struct hlist_node *n)
{
        __hlist_del(n);
        n->next = ((void *) 0x00100100);
        n->pprev = ((void *) 0x00200200);
}
static void hlist_del_rcu(struct hlist_node *n)
{
        __hlist_del(n);
        n->pprev = ((void *) 0x00200200);
}

static void hlist_del_init(struct hlist_node *n)
{
        if (n->pprev) {
                __hlist_del(n);
                ((n)->next = ((void *)0), (n)->pprev = ((void *)0));
        }
}

static void hlist_add_head(struct hlist_node *n, struct hlist_head *h)
{
        struct hlist_node *first = h->first;
        n->next = first;
        if (first)
                first->pprev = &n->next;
        h->first = n;
        n->pprev = &h->first;
}
static void hlist_add_head_rcu(struct hlist_node *n,
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


static void hlist_add_before(struct hlist_node *n,
                                        struct hlist_node *next)
{
        n->pprev = next->pprev;
        n->next = next;
        next->pprev = &n->next;
        *(n->pprev) = n;
}

static void hlist_add_after(struct hlist_node *n,
                                        struct hlist_node *next)
{
        next->next = n->next;
        n->next = next;
        next->pprev = &n->next;

        if(next->next)
                next->next->pprev = &next->next;
}
static void hlist_add_before_rcu(struct hlist_node *n,
                                        struct hlist_node *next)
{
        n->pprev = next->pprev;
        n->next = next;
        __asm__ __volatile__ ("": : :"memory");
        next->pprev = &n->next;
        *(n->pprev) = n;
}
static void hlist_add_after_rcu(struct hlist_node *prev,
                                       struct hlist_node *n)
{
        n->next = prev->next;
        n->pprev = &prev->next;
        __asm__ __volatile__ ("": : :"memory");
        prev->next = n;
        if (n->next)
                n->next->pprev = &n->next;
}








struct task_struct;

static struct task_struct * get_current(void)
{
        return current_thread_info()->task;
}

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
static void init_waitqueue_head(wait_queue_head_t *q)
{
        spin_lock_init(&q->lock);
        do { (&q->task_list)->next = (&q->task_list); (&q->task_list)->prev = (&q->task_list); } while (0);
}

static void init_waitqueue_entry(wait_queue_t *q, struct task_struct *p)
{
        q->flags = 0;
        q->private = p;
        q->func = default_wake_function;
}

static void init_waitqueue_func_entry(wait_queue_t *q,
                                        wait_queue_func_t func)
{
        q->flags = 0;
        q->private = ((void *)0);
        q->func = func;
}

static int waitqueue_active(wait_queue_head_t *q)
{
        return !list_empty(&q->task_list);
}
extern void add_wait_queue(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));
extern void add_wait_queue_exclusive(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));
extern void remove_wait_queue(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));

static void __add_wait_queue(wait_queue_head_t *head, wait_queue_t *new)
{
        list_add(&new->task_list, &head->task_list);
}




static void __add_wait_queue_tail(wait_queue_head_t *head,
                                                wait_queue_t *new)
{
        list_add_tail(&new->task_list, &head->task_list);
}

static void __remove_wait_queue(wait_queue_head_t *head,
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
static void add_wait_queue_exclusive_locked(wait_queue_head_t *q,
                                                   wait_queue_t * wait)
{
        wait->flags |= 0x01;
        __add_wait_queue_tail(q, wait);
}




static void remove_wait_queue_locked(wait_queue_head_t *q,
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
static int wait_on_bit(void *word, int bit,
                                int (*action)(void *), unsigned mode)
{
        if (!(__builtin_constant_p(bit) ? constant_test_bit((bit),(word)) : variable_test_bit((bit),(word))))
                return 0;
        return out_of_line_wait_on_bit(word, bit, action, mode);
}
static int wait_on_bit_lock(void *word, int bit,
                                int (*action)(void *), unsigned mode)
{
        if (!test_and_set_bit(bit, word))
                return 0;
        return out_of_line_wait_on_bit_lock(word, bit, action, mode);
}
struct rw_semaphore;


struct rwsem_waiter;
struct rw_semaphore {
        __s32 activity;
        spinlock_t wait_lock;
        struct list_head wait_list;



};
extern void init_rwsem(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __down_read(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern int __down_read_trylock(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __down_write(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern int __down_write_trylock(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __up_read(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __up_write(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern void __downgrade_write(struct rw_semaphore *sem) __attribute__((regparm(3)));
static void down_read(struct rw_semaphore *sem)
{
        do { do { } while (0); } while (0);
        ;
        __down_read(sem);
        ;
}




static int down_read_trylock(struct rw_semaphore *sem)
{
        int ret;
        ;
        ret = __down_read_trylock(sem);
        ;
        return ret;
}




static void down_write(struct rw_semaphore *sem)
{
        do { do { } while (0); } while (0);
        ;
        __down_write(sem);
        ;
}




static int down_write_trylock(struct rw_semaphore *sem)
{
        int ret;
        ;
        ret = __down_write_trylock(sem);
        ;
        return ret;
}




static void up_read(struct rw_semaphore *sem)
{
        ;
        __up_read(sem);
        ;
}




static void up_write(struct rw_semaphore *sem)
{
        ;
        __up_write(sem);
        ;
}




static void downgrade_write(struct rw_semaphore *sem)
{
        ;
        __downgrade_write(sem);
        ;
}

struct semaphore {
        atomic_t count;
        int sleepers;
        wait_queue_head_t wait;
};
static void sema_init (struct semaphore *sem, int val)
{






        (((&sem->count)->counter) = (val));
        sem->sleepers = 0;
        init_waitqueue_head(&sem->wait);
}

static void init_MUTEX (struct semaphore *sem)
{
        sema_init(sem, 1);
}

static void init_MUTEX_LOCKED (struct semaphore *sem)
{
        sema_init(sem, 0);
}

__attribute__((regparm(3))) void __down_failed(void );
__attribute__((regparm(3))) int __down_failed_interruptible(void );
__attribute__((regparm(3))) int __down_failed_trylock(void );
__attribute__((regparm(3))) void __up_wakeup(void );






static void down(struct semaphore * sem)
{
        do { do { } while (0); } while (0);
        __asm__ __volatile__(
                "# atomic down operation\n\t"
                "lock ; " "decl %0\n\t"
                "js 2f\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "tty_io" "\n\t" ".text.lock." "tty_io" ":\n\t" ".endif\n"
                "2:\tlea %0,%%eax\n\t"
                "call __down_failed\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                :"=m" (sem->count)
                :
                :"memory","ax");
}





static int down_interruptible(struct semaphore * sem)
{
        int result;

        do { do { } while (0); } while (0);
        __asm__ __volatile__(
                "# atomic interruptible down operation\n\t"
                "lock ; " "decl %1\n\t"
                "js 2f\n\t"
                "xorl %0,%0\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "tty_io" "\n\t" ".text.lock." "tty_io" ":\n\t" ".endif\n"
                "2:\tlea %1,%%eax\n\t"
                "call __down_failed_interruptible\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                :"=a" (result), "=m" (sem->count)
                :
                :"memory");
        return result;
}





static int down_trylock(struct semaphore * sem)
{
        int result;

        __asm__ __volatile__(
                "# atomic interruptible down operation\n\t"
                "lock ; " "decl %1\n\t"
                "js 2f\n\t"
                "xorl %0,%0\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "tty_io" "\n\t" ".text.lock." "tty_io" ":\n\t" ".endif\n"
                "2:\tlea %1,%%eax\n\t"
                "call __down_failed_trylock\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                :"=a" (result), "=m" (sem->count)
                :
                :"memory");
        return result;
}







static void up(struct semaphore * sem)
{
        __asm__ __volatile__(
                "# atomic up operation\n\t"
                "lock ; " "incl %0\n\t"
                "jle 2f\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "tty_io" "\n\t" ".text.lock." "tty_io" ":\n\t" ".endif\n"
                "2:\tlea %0,%%eax\n\t"
                "call __up_wakeup\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                ".subsection 0\n"
                :"=m" (sem->count)
                :
                :"memory","ax");
}

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
struct task_struct;
extern void send_sigtrap(struct task_struct *tsk, struct pt_regs *regs, int error_code);
static int user_mode(struct pt_regs *regs)
{
        return (regs->xcs & 3) != 0;
}
static int user_mode_vm(struct pt_regs *regs)
{
        return ((regs->xcs & 3) | (regs->eflags & 0x00020000)) != 0;
}
typedef struct {
        int size;
        struct semaphore sem;
        void *ldt;
} mm_context_t;









typedef unsigned long cputime_t;
typedef u64 cputime64_t;

extern void cpu_idle(void);







static int
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

static int
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
static void acpi_noirq_set(void) { }
static void acpi_disable_pci(void) { }
extern u8 x86_acpiid_to_apicid[];
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
enum fixed_addresses {
        FIX_HOLE,
        FIX_VSYSCALL,

        FIX_APIC_BASE,


        FIX_IO_APIC_BASE_0,
        FIX_IO_APIC_BASE_END = FIX_IO_APIC_BASE_0 + 64 -1,
        FIX_F00F_IDT,





        FIX_KMAP_BEGIN,
        FIX_KMAP_END = FIX_KMAP_BEGIN+(KM_TYPE_NR*8)-1,
        __end_of_permanent_fixed_addresses,


        FIX_BTMAP_END = __end_of_permanent_fixed_addresses,
        FIX_BTMAP_BEGIN = FIX_BTMAP_END + 16 - 1,
        FIX_WP_TEST,
        __end_of_fixed_addresses
};

extern void __set_fixmap (enum fixed_addresses idx,
                                        unsigned long phys, pgprot_t flags);
extern void __this_fixmap_does_not_exist(void);






static unsigned long fix_to_virt(const unsigned int idx)
{
        if (idx >= __end_of_fixed_addresses)
                __this_fixmap_does_not_exist();

        return (((unsigned long)0xfffff000) - ((idx) << 12));
}

static unsigned long virt_to_fix(const unsigned long vaddr)
{
        do { if (__builtin_expect(!!((vaddr >= ((unsigned long)0xfffff000) || vaddr < (((unsigned long)0xfffff000) - (__end_of_permanent_fixed_addresses << 12)))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (153), "i" ("include/asm/fixmap.h")); } while(0);
        return ((((unsigned long)0xfffff000) - ((vaddr)&(~((1UL << 12)-1)))) >> 12);
}





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
enum mp_bustype {
        MP_BUS_ISA = 1,
        MP_BUS_EISA,
        MP_BUS_PCI,
        MP_BUS_MCA,
        MP_BUS_NEC98
};

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
struct physid_mask
{
        unsigned long mask[(((256)+32 -1)/32)];
};

typedef struct physid_mask physid_mask_t;
extern physid_mask_t phys_cpu_present_map;

static int use_pci_vector(void) {return 0;}
static void disable_edge_ioapic_irq(unsigned int irq) { }
static void mask_and_ack_level_ioapic_irq(unsigned int irq) { }
static void end_edge_ioapic_irq (unsigned int irq) { }
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

static unsigned int io_apic_read(unsigned int apic, unsigned int reg)
{
        *((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        return *(((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4);
}

static void io_apic_write(unsigned int apic, unsigned int reg, unsigned int value)
{
        *((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        *(((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4) = value;
}







extern int sis_apic_bug;
static void io_apic_modify(unsigned int apic, unsigned int reg, unsigned int value)
{
        if (sis_apic_bug)
                *((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        *(((volatile int *)((((unsigned long)0xfffff000) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4) = value;
}


extern int skip_ioapic_setup;
extern int (*ioapic_renumber_irq)(int ioapic, int irq);





extern int assign_irq_vector(int irq);





typedef int pm_request_t;
typedef int pm_dev_t;
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
extern int enable_local_apic;
extern int apic_verbosity;

static void lapic_disable(void)
{
        enable_local_apic = -1;
        clear_bit((0*32+ 9), boot_cpu_data.x86_capability);
}

static void lapic_enable(void)
{
        enable_local_apic = 1;
}
static void apic_write(unsigned long reg, unsigned long v)
{
        *((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)) = v;
}

static void apic_write_atomic(unsigned long reg, unsigned long v)
{
        ((__typeof__(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg))))__xchg((unsigned long)(v),((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)),sizeof(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)))));
}

static unsigned long apic_read(unsigned long reg)
{
        return *((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg));
}

static void apic_wait_icr_idle(void)
{
        while ( apic_read( 0x300 ) & 0x01000 )
                rep_nop();
}

int get_physical_broadcast(void);
static void ack_APIC_irq(void)
{
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
extern cpumask_t cpu_callout_map;
extern cpumask_t cpu_callin_map;
extern cpumask_t cpu_possible_map;


static int num_booting_cpus(void)
{
        return __cpus_weight(&(cpu_callout_map), 8);
}











static unsigned get_apic_id(unsigned long x)
{
        return (((x)>>24)&0xFF);
}
static int hard_smp_processor_id(void)
{

        return get_apic_id(*(unsigned long *)((fix_to_virt(FIX_APIC_BASE))+0x20));
}


static int logical_smp_processor_id(void)
{

        return (((*(unsigned long *)((fix_to_virt(FIX_APIC_BASE))+0xD0))>>24)&0xFF);
}



extern int __cpu_disable(void);
extern void __cpu_die(unsigned int cpu);
extern void smp_send_stop(void);




extern void smp_send_reschedule(int cpu);





extern void smp_prepare_cpus(unsigned int max_cpus);




extern int __cpu_up(unsigned int cpunum);




extern void smp_cpus_done(unsigned int max_cpus);




extern int smp_call_function (void (*func) (void *info), void *info,
                              int retry, int wait);




static int on_each_cpu(void (*func) (void *info), void *info,
                              int retry, int wait)
{
        int ret = 0;

        do { } while (0);
        ret = smp_call_function(func, info, retry, wait);
        func(info);
        do { } while (0);
        return ret;
}
void smp_prepare_boot_cpu(void);



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





struct siginfo;
typedef unsigned long old_sigset_t;

typedef struct {
        unsigned long sig[(64 / 32)];
} sigset_t;
typedef void __signalfn_t(int);
typedef __signalfn_t *__sighandler_t;

typedef void __restorefn_t(void);
typedef __restorefn_t *__sigrestore_t;


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
typedef struct sigaltstack {
        void *ss_sp;
        int ss_flags;
        size_t ss_size;
} stack_t;






static void sigaddset(sigset_t *set, int _sig)
{
        __asm__("btsl %1,%0" : "=m"(*set) : "Ir"(_sig - 1) : "cc");
}

static void sigdelset(sigset_t *set, int _sig)
{
        __asm__("btrl %1,%0" : "=m"(*set) : "Ir"(_sig - 1) : "cc");
}

static int __const_sigismember(sigset_t *set, int _sig)
{
        unsigned long sig = _sig - 1;
        return 1 & (set->sig[sig / 32] >> (sig % 32));
}

static int __gen_sigismember(sigset_t *set, int _sig)
{
        int ret;
        __asm__("btl %2,%1\n\tsbbl %0,%0"
                : "=r"(ret) : "m"(*set), "Ir"(_sig-1) : "cc");
        return ret;
}






static int sigfindinword(unsigned long word)
{
        __asm__("bsfl %1,%0" : "=r"(word) : "rm"(word) : "cc");
        return word;
}

struct pt_regs;
extern int do_signal(struct pt_regs *regs, sigset_t *oldset) __attribute__((regparm(3)));









typedef union sigval {
        int sival_int;
        void *sival_ptr;
} sigval_t;
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





static void copy_siginfo(struct siginfo *to, struct siginfo *from)
{
        if (from->si_code < 0)
                (__builtin_constant_p(sizeof(*to)) ? __constant_memcpy((to),(from),(sizeof(*to))) : __memcpy((to),(from),(sizeof(*to))));
        else

                (__builtin_constant_p((3 * sizeof(int)) + sizeof(from->_sifields._sigchld)) ? __constant_memcpy((to),(from),((3 * sizeof(int)) + sizeof(from->_sifields._sigchld))) : __memcpy((to),(from),((3 * sizeof(int)) + sizeof(from->_sifields._sigchld))));
}



extern int copy_siginfo_to_user(struct siginfo *to, struct siginfo *from);
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
static void sigorsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) | (b3)); r->sig[2] = ((a2) | (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) | (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) | (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }


static void sigandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & (b3)); r->sig[2] = ((a2) & (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }


static void signandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & ~(b3)); r->sig[2] = ((a2) & ~(b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & ~(b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & ~(b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }
static void signotset(sigset_t *set) { extern void _NSIG_WORDS_is_unsupported_size(void); switch ((64 / 32)) { case 4: set->sig[3] = (~(set->sig[3])); set->sig[2] = (~(set->sig[2])); case 2: set->sig[1] = (~(set->sig[1])); case 1: set->sig[0] = (~(set->sig[0])); break; default: _NSIG_WORDS_is_unsupported_size(); } }




static void sigemptyset(sigset_t *set)
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

static void sigfillset(sigset_t *set)
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



static void sigaddsetmask(sigset_t *set, unsigned long mask)
{
        set->sig[0] |= mask;
}

static void sigdelsetmask(sigset_t *set, unsigned long mask)
{
        set->sig[0] &= ~mask;
}

static int sigtestsetmask(sigset_t *set, unsigned long mask)
{
        return (set->sig[0] & mask) != 0;
}

static void siginitset(sigset_t *set, unsigned long mask)
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

static void siginitsetinv(sigset_t *set, unsigned long mask)
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



static void init_sigpending(struct sigpending *sig)
{
        sigemptyset(&sig->signal);
        do { (&sig->list)->next = (&sig->list); (&sig->list)->prev = (&sig->list); } while (0);
}


static int valid_signal(unsigned long sig)
{
        return sig <= 64 ? 1 : 0;
}

extern int group_send_sig_info(int sig, struct siginfo *info, struct task_struct *p);
extern int __group_send_sig_info(int, struct siginfo *, struct task_struct *);
extern long do_sigpending(void *, unsigned long);
extern int sigprocmask(int, sigset_t *, sigset_t *);

struct pt_regs;
extern int get_signal_to_deliver(siginfo_t *info, struct k_sigaction *return_ka, struct pt_regs *regs, void *cookie);





extern unsigned securebits;



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

struct completion {
        unsigned int done;
        wait_queue_head_t wait;
};







static void init_completion(struct completion *x)
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
extern int attach_pid(struct task_struct *task, enum pid_type type, int nr) __attribute__((regparm(3)));

extern void detach_pid(struct task_struct *task, enum pid_type) __attribute__((regparm(3)));





extern struct pid *find_pid(enum pid_type, int) __attribute__((regparm(3)));

extern int alloc_pidmap(void);
extern void free_pidmap(int) __attribute__((regparm(3)));
extern void switch_exec_pids(struct task_struct *leader, struct task_struct *thread);



typedef struct kmem_cache_s kmem_cache_t;





typedef int (*initcall_t)(void);
typedef void (*exitcall_t)(void);

extern initcall_t __con_initcall_start[], __con_initcall_end[];
extern initcall_t __security_initcall_start[], __security_initcall_end[];


extern char saved_command_line[];
struct obs_kernel_param {
        const char *str;
        int (*setup_func)(char *);
        int early;
};
void __attribute__ ((__section__ (".init.text"))) parse_early_param(void);
struct free_area {
        struct list_head free_list;
        unsigned long nr_free;
};

struct pglist_data;
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
} __attribute__((__aligned__((1 << (4)))));
struct zone {

        unsigned long free_pages;
        unsigned long pages_min, pages_low, pages_high;
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
        int temp_priority;
        int prev_priority;


        struct zone_padding _pad2_;
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
struct zonelist {
        struct zone *zones[(1 << 0) * 3 + 1];
};
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
extern struct pglist_data *pgdat_list;

void __get_zone_counts(unsigned long *active, unsigned long *inactive,
                        unsigned long *free, struct pglist_data *pgdat);
void get_zone_counts(unsigned long *active, unsigned long *inactive,
                        unsigned long *free);
void build_all_zonelists(void);
void wakeup_kswapd(struct zone *zone, int order);
int zone_watermark_ok(struct zone *z, int order, unsigned long mark,
                int alloc_type, int can_try_harder, int gfp_high);




static void memory_present(int nid, unsigned long start, unsigned long end) {}
static struct zone *next_zone(struct zone *zone)
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
static int is_highmem_idx(int idx)
{
        return (idx == 2);
}

static int is_normal_idx(int idx)
{
        return (idx == 1);
}






static int is_highmem(struct zone *zone)
{
        return zone == zone->zone_pgdat->node_zones + 2;
}

static int is_normal(struct zone *zone)
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







extern struct pglist_data contig_page_data;
void memory_present(int nid, unsigned long start, unsigned long end);
unsigned long __attribute__ ((__section__ (".init.text"))) node_memmap_size_bytes(int, unsigned long, unsigned long);




struct vm_area_struct;
static void arch_free_page(struct page *page, int order) { }


extern struct page *
__alloc_pages(gfp_t, unsigned int, struct zonelist *) __attribute__((regparm(3)));

static struct page *alloc_pages_node(int nid, gfp_t gfp_mask,
                                                unsigned int order)
{
        if (__builtin_expect(!!(order >= 11), 0))
                return ((void *)0);

        return __alloc_pages(gfp_mask, order,
                (&contig_page_data)->node_zonelists + (gfp_mask & 0x03));
}
extern unsigned long __get_free_pages(gfp_t gfp_mask, unsigned int order) __attribute__((regparm(3)));
extern unsigned long get_zeroed_page(gfp_t gfp_mask) __attribute__((regparm(3)));







extern void __free_pages(struct page *page, unsigned int order) __attribute__((regparm(3)));
extern void free_pages(unsigned long addr, unsigned int order) __attribute__((regparm(3)));
extern void free_hot_page(struct page *page) __attribute__((regparm(3)));
extern void free_cold_page(struct page *page) __attribute__((regparm(3)));




void page_alloc_init(void);



static void drain_remote_pages(void) { };
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

static void *kmalloc(size_t size, gfp_t flags)
{
        if (__builtin_constant_p(size)) {
                int i = 0;






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







static void *kcalloc(size_t n, size_t size, gfp_t flags)
{
        if (n != 0 && size > ((int)(~0U>>1)) / n)
                return ((void *)0);
        return kzalloc(n * size, flags);
}

extern void kfree(const void *);
extern unsigned int ksize(const void *);





static void *kmem_cache_alloc_node(kmem_cache_t *cachep, int flags, int node)
{
        return kmem_cache_alloc(cachep, flags);
}
static void *kmalloc_node(size_t size, gfp_t flags, int node)
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
struct percpu_data {
        void *ptrs[8];
        void *blkp;
};
extern void *__alloc_percpu(size_t size, size_t align);
extern void free_percpu(const void *);




extern int errno;





typedef struct { int mode; } seccomp_t;

extern void __secure_computing(int);
static void secure_computing(int this_syscall)
{
        if (__builtin_expect(!!(test_thread_flag(8)), 0))
                __secure_computing(this_syscall);
}

static int has_secure_computing(struct thread_info *ti)
{
        return __builtin_expect(!!(test_ti_thread_flag(ti, 8)), 0);
}





struct exec_domain;
extern unsigned long avenrun[];
extern unsigned long total_forks;
extern int nr_threads;
extern int last_pid;
extern __typeof__(unsigned long) per_cpu__process_counts;
extern int nr_processes(void);
extern unsigned long nr_running(void);
extern unsigned long nr_uninterruptible(void);
extern unsigned long nr_iowait(void);





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
void __attribute__((regparm(3))) init_timer(struct timer_list * timer);
static int timer_pending(const struct timer_list * timer)
{
        return timer->entry.next != ((void *)0);
}

extern void add_timer_on(struct timer_list *timer, int cpu);
extern int del_timer(struct timer_list * timer);
extern int __mod_timer(struct timer_list *timer, unsigned long expires);
extern int mod_timer(struct timer_list *timer, unsigned long expires);

extern unsigned long next_timer_interrupt(void);
static void add_timer(struct timer_list * timer)
{
        __mod_timer(timer, timer->expires);
}


  extern int try_to_del_timer_sync(struct timer_list *timer);
  extern int del_timer_sync(struct timer_list *timer);







extern void init_timers(void);
extern void run_local_timers(void);
extern void it_real_fn(unsigned long);
struct sched_param {
        int sched_priority;
};
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
extern int in_sched_functions(unsigned long addr);


extern signed long schedule_timeout(signed long timeout) __attribute__((regparm(3)));
extern signed long schedule_timeout_interruptible(signed long timeout);
extern signed long schedule_timeout_uninterruptible(signed long timeout);
 __attribute__((regparm(0))) void schedule(void);

struct namespace;




extern int sysctl_max_map_count;





struct workqueue_struct;

struct work_struct {
        unsigned long pending;
        struct list_head entry;
        void (*func)(void *);
        void *data;
        void *wq_data;
        struct timer_list timer;
};
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






static int cancel_delayed_work(struct work_struct *work)
{
        int ret;

        ret = del_timer_sync(&work->timer);
        if (ret)
                clear_bit(0, &work->pending);
        return ret;
}
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






struct kioctx;
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
static struct kiocb *list_kiocb(struct list_head *h)
{
        return ({ const typeof( ((struct kiocb *)0)->ki_list ) *__mptr = (h); (struct kiocb *)( (char *)__mptr - ((size_t) &((struct kiocb *)0)->ki_list) );});
}


extern atomic_t aio_nr;
extern unsigned aio_max_nr;

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
        struct rlimit rlim[15];

        struct list_head cpu_timers[3];







};
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
enum idle_type
{
        SCHED_IDLE,
        NOT_IDLE,
        NEWLY_IDLE,
        MAX_IDLE_TYPES
};
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
extern struct group_info *groups_alloc(int gidsetsize);
extern void groups_free(struct group_info *group_info);
extern int set_current_groups(struct group_info *group_info);
extern int groups_search(struct group_info *group_info, gid_t grp);







static void prefetch_stack(struct task_struct *t) { }


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
        atomic_t fs_excl;
};

static pid_t process_group(struct task_struct *tsk)
{
        return tsk->signal->pgrp;
}
static int pid_alive(struct task_struct *p)
{
        return p->pids[PIDTYPE_PID].nr != 0;
}

extern void free_task(struct task_struct *tsk);
extern void __put_task_struct(struct task_struct *tsk);
extern int set_cpus_allowed(task_t *p, cpumask_t new_mask);
extern unsigned long long sched_clock(void);
extern unsigned long long current_sched_time(const task_t *current_task);



extern void sched_exec(void);







static void idle_task_exit(void) {}


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


static int kstack_end(void *addr)
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
static struct user_struct *get_uid(struct user_struct *u)
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

static int dequeue_signal_lock(struct task_struct *tsk, sigset_t *mask, siginfo_t *info)
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
static int on_sig_stack(unsigned long sp)
{
        return (sp - get_current()->sas_ss_sp < get_current()->sas_ss_size);
}

static int sas_ss_flags(unsigned long sp)
{
        return (get_current()->sas_ss_size == 0 ? 2
                : on_sig_stack(sp) ? 1 : 0);
}






static int capable(int cap)
{
        if (((get_current()->cap_effective) & (1 << (cap)))) {
                get_current()->flags |= 0x00000100;
                return 1;
        }
        return 0;
}





extern struct mm_struct * mm_alloc(void);


extern void __mmdrop(struct mm_struct *) __attribute__((regparm(3)));
static void mmdrop(struct mm_struct * mm)
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
extern task_t * next_thread(const task_t *p) __attribute__((regparm(3)));



static int thread_group_empty(task_t *p)
{
        return list_empty(&p->pids[PIDTYPE_TGID].pid_list);
}




extern void unhash_process(struct task_struct *p);
static void task_lock(struct task_struct *p)
{
        _spin_lock(&p->alloc_lock);
}

static void task_unlock(struct task_struct *p)
{
        _spin_unlock(&p->alloc_lock);
}




static void set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        set_ti_thread_flag(tsk->thread_info,flag);
}

static void clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        clear_ti_thread_flag(tsk->thread_info,flag);
}

static int test_and_set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_and_set_ti_thread_flag(tsk->thread_info,flag);
}

static int test_and_clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_and_clear_ti_thread_flag(tsk->thread_info,flag);
}

static int test_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_ti_thread_flag(tsk->thread_info,flag);
}

static void set_tsk_need_resched(struct task_struct *tsk)
{
        set_tsk_thread_flag(tsk,3);
}

static void clear_tsk_need_resched(struct task_struct *tsk)
{
        clear_tsk_thread_flag(tsk,3);
}

static int signal_pending(struct task_struct *p)
{
        return __builtin_expect(!!(test_tsk_thread_flag(p,2)), 0);
}

static int need_resched(void)
{
        return __builtin_expect(!!(test_thread_flag(3)), 0);
}
extern int cond_resched(void);
extern int cond_resched_lock(spinlock_t * lock);
extern int cond_resched_softirq(void);
static int lock_need_resched(spinlock_t *lock)
{
        if (0 || need_resched())
                return 1;
        return 0;
}





extern void recalc_sigpending_tsk(struct task_struct *t) __attribute__((regparm(3)));
extern void recalc_sigpending(void);

extern void signal_wake_up(struct task_struct *t, int resume_stopped);






static unsigned int task_cpu(const struct task_struct *p)
{
        return p->thread_info->cpu;
}

static void set_task_cpu(struct task_struct *p, unsigned int cpu)
{
        p->thread_info->cpu = cpu;
}
extern void arch_pick_mmap_layout(struct mm_struct *mm);
extern long sched_setaffinity(pid_t pid, cpumask_t new_mask);
extern long sched_getaffinity(pid_t pid, cpumask_t *mask);



extern void normalize_rt_tasks(void);







static int frozen(struct task_struct *p)
{
        return p->flags & 0x00010000;
}




static int freezing(struct task_struct *p)
{
        return p->flags & 0x00004000;
}





static void freeze(struct task_struct *p)
{
        p->flags |= 0x00004000;
}




static int thaw_process(struct task_struct *p)
{
        if (frozen(p)) {
                p->flags &= ~0x00010000;
                wake_up_process(p);
                return 1;
        }
        return 0;
}




static void frozen_process(struct task_struct *p)
{
        p->flags = (p->flags & ~0x00004000) | 0x00010000;
}

extern void refrigerator(void);
extern int freeze_processes(void);
extern void thaw_processes(void);

static int try_to_freeze(void)
{
        if (freezing(get_current())) {
                refrigerator();
                return 1;
        } else
                return 0;
}










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


extern int request_module(const char * name, ...) __attribute__ ((format (printf, 1, 2)));






struct key;
extern int call_usermodehelper_keys(char *path, char *argv[], char *envp[],
                                    struct key *session_keyring, int wait);

static int
call_usermodehelper(char *path, char **argv, char **envp, int wait)
{
        return call_usermodehelper_keys(path, argv, envp, ((void *)0), wait);
}

extern void usermodehelper_init(void);





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
typedef unsigned long elf_greg_t;


typedef elf_greg_t elf_gregset_t[(sizeof (struct user_regs_struct) / sizeof(elf_greg_t))];

typedef struct user_i387_struct elf_fpregset_t;
typedef struct user_fxsr_struct elf_fpxregset_t;
extern int dump_task_regs (struct task_struct *, elf_gregset_t *);
extern int dump_task_fpu (struct task_struct *, elf_fpregset_t *);
extern int dump_task_extended_fpu (struct task_struct *, struct user_fxsr_struct *);
extern void __kernel_vsyscall;
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


struct kref {
        atomic_t refcount;
};

void kref_init(struct kref *kref);
void kref_get(struct kref *kref);
int kref_put(struct kref *kref, void (*release) (struct kref *kref));
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

static const char * kobject_name(const struct kobject * kobj)
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

static struct kset * to_kset(struct kobject * kobj)
{
        return kobj ? ({ const typeof( ((struct kset *)0)->kobj ) *__mptr = (kobj); (struct kset *)( (char *)__mptr - ((size_t) &((struct kset *)0)->kobj) );}) : ((void *)0);
}

static struct kset * kset_get(struct kset * k)
{
        return k ? to_kset(kobject_get(&k->kobj)) : ((void *)0);
}

static void kset_put(struct kset * k)
{
        kobject_put(&k->kobj);
}

static struct kobj_type * get_ktype(struct kobject * k)
{
        if (k->kset && k->kset->ktype)
                return k->kset->ktype;
        else
                return k->ktype;
}

extern struct kobject * kset_find_obj(struct kset *, const char *);
struct subsystem {
        struct kset kset;
        struct rw_semaphore rwsem;
};
extern struct subsystem kernel_subsys;
extern void subsystem_init(struct subsystem *);
extern int subsystem_register(struct subsystem *);
extern void subsystem_unregister(struct subsystem *);

static struct subsystem * subsys_get(struct subsystem * s)
{
        return s ? ({ const typeof( ((struct subsystem *)0)->kset ) *__mptr = (kset_get(&s->kset)); (struct subsystem *)( (char *)__mptr - ((size_t) &((struct subsystem *)0)->kset) );}) : ((void *)0);
}

static void subsys_put(struct subsystem * s)
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





typedef struct
{
        volatile unsigned long counter;
} local_t;






static void local_inc(local_t *v)
{
        __asm__ __volatile__(
                "incl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}

static void local_dec(local_t *v)
{
        __asm__ __volatile__(
                "decl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}

static void local_add(unsigned long i, local_t *v)
{
        __asm__ __volatile__(
                "addl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}

static void local_sub(unsigned long i, local_t *v)
{
        __asm__ __volatile__(
                "subl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}





struct mod_arch_specific
{
};
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
const struct exception_table_entry *search_exception_tables(unsigned long add);

struct notifier_block;




void *__symbol_get(const char *symbol);
void *__symbol_get_gpl(const char *symbol);
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
        Elf32_Sym *symtab;
        unsigned long num_symtab;
        char *strtab;


        struct module_sect_attrs *sect_attrs;



        void *percpu;



        char *args;
};




static int module_is_live(struct module *mod)
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
static int try_module_get(struct module *module)
{
        return !module || module_is_live(module);
}
static void module_put(struct module *module)
{
}
static void __module_get(struct module *module)
{
}
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
struct obsolete_modparm {
        char name[64];
        char type[64-sizeof(void *)];
        void *addr;
};

static void MODULE_PARM_(void) { }
extern void __attribute__((deprecated)) inter_module_register(const char *,
                struct module *, const void *);
extern void __attribute__((deprecated)) inter_module_unregister(const char *);
extern const void * __attribute__((deprecated)) inter_module_get_request(const char *,
                const char *);
extern void __attribute__((deprecated)) inter_module_put(const char *);









extern int __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) __reacquire_kernel_lock(void);
extern void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) __release_kernel_lock(void);
static int reacquire_kernel_lock(struct task_struct *task)
{
        if (__builtin_expect(!!(task->lock_depth >= 0), 0))
                __reacquire_kernel_lock();
        return 0;
}

extern void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) lock_kernel(void) ;
extern void __attribute__((regparm(3))) __attribute__((section(".spinlock.text"))) unlock_kernel(void) ;







static int irq_canonicalize(int irq)
{
        return ((irq == 2) ? 9 : irq);
}

extern void release_vm86_irqs(struct task_struct *);
extern int irqbalance_disable(char *str);
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


static irq_desc_t *
irq_descp (int irq)
{
        return irq_desc + irq;
}

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

struct hw_interrupt_type;
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




static void hw_resend_irq(struct hw_interrupt_type *h, unsigned int i)
{
        if ((((i) >= 16) || ((1<<(i)) & io_apic_irqs)))
                send_IPI_self((irq_vector[i]));
}

extern int setup_irq(unsigned int irq, struct irqaction * new);


extern cpumask_t irq_affinity[224];


static void set_native_irq_info(int irq, cpumask_t mask)
{
        irq_affinity[irq] = mask;
}
extern cpumask_t pending_irq_cpumask[224];

static void set_pending_irq(unsigned int irq, cpumask_t mask)
{
        irq_desc_t *desc = irq_desc + irq;
        unsigned long flags;

        flags = _spin_lock_irqsave(&desc->lock);
        desc->move_irq = 1;
        pending_irq_cpumask[irq] = mask;
        _spin_unlock_irqrestore(&desc->lock, flags);
}

static void
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
        if (__builtin_expect(!!(!__cpus_empty(&(tmp), 8)), 0)) {
                desc->handler->disable(irq);
                desc->handler->set_affinity(irq,tmp);
                desc->handler->enable(irq);
        }
        __cpus_clear(&(pending_irq_cpumask[irq]), 8);
}
static void move_irq(int irq)
{
        move_native_irq(irq);
}

static void set_irq_info(int irq, cpumask_t mask)
{
        set_native_irq_info(irq, mask);
}
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

typedef struct {
        unsigned int __softirq_pending;
        unsigned long idle_timestamp;
        unsigned int __nmi_count;
        unsigned int apic_timer_irqs;
} __attribute__((__aligned__((1 << (4))))) irq_cpustat_t;

extern __typeof__(irq_cpustat_t) per_cpu__irq_stat;
extern irq_cpustat_t irq_stat[];




void ack_bad_irq(unsigned int irq);
extern void synchronize_irq(unsigned int irq);
static void account_user_vtime(struct task_struct *tsk)
{
}

static void account_system_vtime(struct task_struct *tsk)
{
}
extern void irq_exit(void);
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
struct tasklet_struct
{
        struct tasklet_struct *next;
        unsigned long state;
        atomic_t count;
        void (*func)(unsigned long);
        unsigned long data;
};
enum
{
        TASKLET_STATE_SCHED,
        TASKLET_STATE_RUN
};


static int tasklet_trylock(struct tasklet_struct *t)
{
        return !test_and_set_bit(TASKLET_STATE_RUN, &(t)->state);
}

static void tasklet_unlock(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        clear_bit(TASKLET_STATE_RUN, &(t)->state);
}

static void tasklet_unlock_wait(struct tasklet_struct *t)
{
        while ((__builtin_constant_p(TASKLET_STATE_RUN) ? constant_test_bit((TASKLET_STATE_RUN),(&(t)->state)) : variable_test_bit((TASKLET_STATE_RUN),(&(t)->state)))) { __asm__ __volatile__("": : :"memory"); }
}






extern void __tasklet_schedule(struct tasklet_struct *t) __attribute__((regparm(3)));

static void tasklet_schedule(struct tasklet_struct *t)
{
        if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
                __tasklet_schedule(t);
}

extern void __tasklet_hi_schedule(struct tasklet_struct *t) __attribute__((regparm(3)));

static void tasklet_hi_schedule(struct tasklet_struct *t)
{
        if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
                __tasklet_hi_schedule(t);
}


static void tasklet_disable_nosync(struct tasklet_struct *t)
{
        atomic_inc(&t->count);
        __asm__ __volatile__("": : :"memory");
}

static void tasklet_disable(struct tasklet_struct *t)
{
        tasklet_disable_nosync(t);
        tasklet_unlock_wait(t);
        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "mfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
}

static void tasklet_enable(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        atomic_dec(&t->count);
}

static void tasklet_hi_enable(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        atomic_dec(&t->count);
}

extern void tasklet_kill(struct tasklet_struct *t);
extern void tasklet_kill_immediate(struct tasklet_struct *t, unsigned int cpu);
extern void tasklet_init(struct tasklet_struct *t,
                         void (*func)(unsigned long), unsigned long data);
extern unsigned long probe_irq_on(void);
extern int probe_irq_off(unsigned long);
extern unsigned int probe_irq_mask(unsigned long);
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
struct ccw_device_id {
        __u16 match_flags;

        __u16 cu_type;
        __u16 dev_type;
        __u8 cu_model;
        __u8 dev_model;

        kernel_ulong_t driver_info;
};
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

static int __attribute__((deprecated)) check_region(unsigned long s, unsigned long n)
{
        return __check_region(&ioport_resource, s, n);
}


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

static void *
class_get_devdata (struct class_device *dev)
{
        return dev->class_data;
}

static void
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

static void *
dev_get_drvdata (struct device *dev)
{
        return dev->driver_data;
}

static void
dev_set_drvdata (struct device *dev, void *data)
{
        dev->driver_data = data;
}

static int device_is_registered(struct device *dev)
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


enum pci_mmap_state {
        pci_mmap_io,
        pci_mmap_mem
};
typedef int pci_power_t;
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
static struct pci_bus *pci_scan_bus(int bus, struct pci_ops *ops, void *sysdata)
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

static int pci_read_config_byte(struct pci_dev *dev, int where, u8 *val)
{
        return pci_bus_read_config_byte (dev->bus, dev->devfn, where, val);
}
static int pci_read_config_word(struct pci_dev *dev, int where, u16 *val)
{
        return pci_bus_read_config_word (dev->bus, dev->devfn, where, val);
}
static int pci_read_config_dword(struct pci_dev *dev, int where, u32 *val)
{
        return pci_bus_read_config_dword (dev->bus, dev->devfn, where, val);
}
static int pci_write_config_byte(struct pci_dev *dev, int where, u8 val)
{
        return pci_bus_write_config_byte (dev->bus, dev->devfn, where, val);
}
static int pci_write_config_word(struct pci_dev *dev, int where, u16 val)
{
        return pci_bus_write_config_word (dev->bus, dev->devfn, where, val);
}
static int pci_write_config_dword(struct pci_dev *dev, int where, u32 val)
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
static unsigned long virt_to_phys(volatile void * address)
{
        return ((unsigned long)(address)-((unsigned long)(0xC0000000UL)));
}
static void * phys_to_virt(unsigned long address)
{
        return ((void *)((unsigned long)(address)+((unsigned long)(0xC0000000UL))));
}






extern void * __ioremap(unsigned long offset, unsigned long size, unsigned long flags);
static void * ioremap(unsigned long offset, unsigned long size)
{
        return __ioremap(offset, size, 0);
}

extern void * ioremap_nocache(unsigned long offset, unsigned long size);
extern void iounmap(volatile void *addr);






extern void *bt_ioremap(unsigned long offset, unsigned long size);
extern void bt_iounmap(void *addr, unsigned long size);
static unsigned char readb(const volatile void *addr)
{
        return *(volatile unsigned char *) addr;
}
static unsigned short readw(const volatile void *addr)
{
        return *(volatile unsigned short *) addr;
}
static unsigned int readl(const volatile void *addr)
{
        return *(volatile unsigned int *) addr;
}







static void writeb(unsigned char b, volatile void *addr)
{
        *(volatile unsigned char *) addr = b;
}
static void writew(unsigned short b, volatile void *addr)
{
        *(volatile unsigned short *) addr = b;
}
static void writel(unsigned int b, volatile void *addr)
{
        *(volatile unsigned int *) addr = b;
}






static void memset_io(volatile void *addr, unsigned char val, int count)
{
        (__builtin_constant_p(val) ? (__builtin_constant_p((count)) ? __constant_c_and_count_memset((((void *) addr)),((0x01010101UL*(unsigned char)(val))),((count))) : __constant_c_memset((((void *) addr)),((0x01010101UL*(unsigned char)(val))),((count)))) : (__builtin_constant_p((count)) ? __memset_generic(((((void *) addr))),(((val))),(((count)))) : __memset_generic((((void *) addr)),((val)),((count)))));
}
static void memcpy_fromio(void *dst, const volatile void *src, int count)
{
        __memcpy(dst, (void *) src, count);
}
static void memcpy_toio(volatile void *dst, const void *src, int count)
{
        __memcpy((void *) dst, src, count);
}
static int check_signature(volatile void * io_addr,
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
static void flush_write_buffers(void)
{
        __asm__ __volatile__ ("lock; addl $0,0(%%esp)": : :"memory");
}
static void slow_down_io(void) {
        __asm__ __volatile__(
                "outb %%al,$0x80;"



                : : );
}
static void outb_local(unsigned char value, int port) { __asm__ __volatile__("out" "b" " %" "b" "0, %w1" : : "a"(value), "Nd"(port)); } static  __attribute__((always_)) unsigned char inb_local(int port) { unsigned char value; __asm__ __volatile__("in" "b" " %w1, %" "b" "0" : "=a"(value) : "Nd"(port)); return value; } static  __attribute__((always_)) void outb_local_p(unsigned char value, int port) { outb_local(value, port); slow_down_io(); } static  __attribute__((always_)) unsigned char inb_local_p(int port) { unsigned char value = inb_local(port); slow_down_io(); return value; } static  __attribute__((always_)) void outb(unsigned char value, int port) { outb_local(value, port); } static  __attribute__((always_)) unsigned char inb(int port) { return inb_local(port); } static  __attribute__((always_)) void outb_p(unsigned char value, int port) { outb(value, port); slow_down_io(); } static  __attribute__((always_)) unsigned char inb_p(int port) { unsigned char value = inb(port); slow_down_io(); return value; } static  __attribute__((always_)) void outsb(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "b" : "+S"(addr), "+c"(count) : "d"(port)); } static  __attribute__((always_)) void insb(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "b" : "+D"(addr), "+c"(count) : "d"(port)); }
static void outw_local(unsigned short value, int port) { __asm__ __volatile__("out" "w" " %" "w" "0, %w1" : : "a"(value), "Nd"(port)); } static  __attribute__((always_)) unsigned short inw_local(int port) { unsigned short value; __asm__ __volatile__("in" "w" " %w1, %" "w" "0" : "=a"(value) : "Nd"(port)); return value; } static  __attribute__((always_)) void outw_local_p(unsigned short value, int port) { outw_local(value, port); slow_down_io(); } static  __attribute__((always_)) unsigned short inw_local_p(int port) { unsigned short value = inw_local(port); slow_down_io(); return value; } static  __attribute__((always_)) void outw(unsigned short value, int port) { outw_local(value, port); } static  __attribute__((always_)) unsigned short inw(int port) { return inw_local(port); } static  __attribute__((always_)) void outw_p(unsigned short value, int port) { outw(value, port); slow_down_io(); } static  __attribute__((always_)) unsigned short inw_p(int port) { unsigned short value = inw(port); slow_down_io(); return value; } static  __attribute__((always_)) void outsw(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "w" : "+S"(addr), "+c"(count) : "d"(port)); } static  __attribute__((always_)) void insw(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "w" : "+D"(addr), "+c"(count) : "d"(port)); }
static void outl_local(unsigned int value, int port) { __asm__ __volatile__("out" "l" " %" "" "0, %w1" : : "a"(value), "Nd"(port)); } static  __attribute__((always_)) unsigned int inl_local(int port) { unsigned int value; __asm__ __volatile__("in" "l" " %w1, %" "" "0" : "=a"(value) : "Nd"(port)); return value; } static  __attribute__((always_)) void outl_local_p(unsigned int value, int port) { outl_local(value, port); slow_down_io(); } static  __attribute__((always_)) unsigned int inl_local_p(int port) { unsigned int value = inl_local(port); slow_down_io(); return value; } static  __attribute__((always_)) void outl(unsigned int value, int port) { outl_local(value, port); } static  __attribute__((always_)) unsigned int inl(int port) { return inl_local(port); } static  __attribute__((always_)) void outl_p(unsigned int value, int port) { outl(value, port); slow_down_io(); } static  __attribute__((always_)) unsigned int inl_p(int port) { unsigned int value = inl(port); slow_down_io(); return value; } static  __attribute__((always_)) void outsl(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "l" : "+S"(addr), "+c"(count) : "d"(port)); } static  __attribute__((always_)) void insl(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "l" : "+D"(addr), "+c"(count) : "d"(port)); }



struct scatterlist {
    struct page *page;
    unsigned int offset;
    dma_addr_t dma_address;
    unsigned int length;
};

struct dma_pool *dma_pool_create(const char *name, struct device *dev,
                        size_t size, size_t align, size_t allocation);

void dma_pool_destroy(struct dma_pool *pool);

void *dma_pool_alloc(struct dma_pool *pool, gfp_t mem_flags,
                     dma_addr_t *handle);

void dma_pool_free(struct dma_pool *pool, void *vaddr, dma_addr_t addr);
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


static void pci_scan_msi_device(struct pci_dev *dev) {}
static int pci_enable_msi(struct pci_dev *dev) {return -1;}
static void pci_disable_msi(struct pci_dev *dev) {}
static int pci_enable_msix(struct pci_dev* dev,
        struct msix_entry *entries, int nvec) {return -1;}
static void pci_disable_msix(struct pci_dev *dev) {}
static void msi_remove_pci_irq_vectors(struct pci_dev *dev) {}
static int pci_domain_nr(struct pci_bus *bus) { return 0; }
static int pci_proc_domain(struct pci_bus *bus)
{
        return 0;
}






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

static void prio_tree_iter_init(struct prio_tree_iter *iter,
                struct prio_tree_root *root, unsigned long r_index, unsigned long h_index)
{
        iter->root = root;
        iter->r_index = r_index;
        iter->h_index = h_index;
        iter->cur = ((void *)0);
}
static int prio_tree_empty(const struct prio_tree_root *root)
{
        return root->prio_tree_node == ((void *)0);
}

static int prio_tree_root(const struct prio_tree_node *node)
{
        return node->parent == node;
}

static int prio_tree_left_empty(const struct prio_tree_node *node)
{
        return node->left == node;
}

static int prio_tree_right_empty(const struct prio_tree_node *node)
{
        return node->right == node;
}


struct prio_tree_node *prio_tree_replace(struct prio_tree_root *root,
                struct prio_tree_node *old, struct prio_tree_node *node);
struct prio_tree_node *prio_tree_insert(struct prio_tree_root *root,
                struct prio_tree_node *node);
void prio_tree_remove(struct prio_tree_root *root, struct prio_tree_node *node);
struct prio_tree_node *prio_tree_next(struct prio_tree_iter *iter);



extern unsigned int __invalid_size_argument_for_IOC;
extern spinlock_t __rcuref_hash[];
static void rcuref_inc(atomic_t *rcuref)
{
        unsigned long flags;
        flags = _spin_lock_irqsave((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]));
        rcuref->counter += 1;
        _spin_unlock_irqrestore((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]), flags);
}
static void rcuref_dec(atomic_t *rcuref)
{
        unsigned long flags;
        flags = _spin_lock_irqsave((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]));
        rcuref->counter -= 1;
        _spin_unlock_irqrestore((&__rcuref_hash[(((unsigned long)rcuref)>>8) & (4 -1)]), flags);
}
static int rcuref_dec_and_test(atomic_t *rcuref)
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
static int rcuref_inc_lf(atomic_t *rcuref)
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
static int old_valid_dev(dev_t dev)
{
        return ((unsigned int) ((dev) >> 20)) < 256 && ((unsigned int) ((dev) & ((1U << 20) - 1))) < 256;
}

static u16 old_encode_dev(dev_t dev)
{
        return (((unsigned int) ((dev) >> 20)) << 8) | ((unsigned int) ((dev) & ((1U << 20) - 1)));
}

static dev_t old_decode_dev(u16 val)
{
        return ((((val >> 8) & 255) << 20) | (val & 255));
}

static int new_valid_dev(dev_t dev)
{
        return 1;
}

static u32 new_encode_dev(dev_t dev)
{
        unsigned major = ((unsigned int) ((dev) >> 20));
        unsigned minor = ((unsigned int) ((dev) & ((1U << 20) - 1)));
        return (minor & 0xff) | (major << 8) | ((minor & ~0xff) << 12);
}

static dev_t new_decode_dev(u32 dev)
{
        unsigned major = (dev & 0xfff00) >> 8;
        unsigned minor = (dev & 0xff) | ((dev >> 12) & 0xfff00);
        return (((major) << 20) | (minor));
}

static int huge_valid_dev(dev_t dev)
{
        return 1;
}

static u64 huge_encode_dev(dev_t dev)
{
        return new_encode_dev(dev);
}

static dev_t huge_decode_dev(u64 dev)
{
        return new_decode_dev(dev);
}

static int sysv_valid_dev(dev_t dev)
{
        return ((unsigned int) ((dev) >> 20)) < (1<<14) && ((unsigned int) ((dev) & ((1U << 20) - 1))) < (1<<18);
}

static u32 sysv_encode_dev(dev_t dev)
{
        return ((unsigned int) ((dev) & ((1U << 20) - 1))) | (((unsigned int) ((dev) >> 20)) << 18);
}

static unsigned sysv_major(u32 dev)
{
        return (dev >> 18) & 0x3fff;
}

static unsigned sysv_minor(u32 dev)
{
        return dev & 0x3ffff;
}
struct rcu_head {
        struct rcu_head *next;
        void (*func)(struct rcu_head *head);
};
struct rcu_ctrlblk {
        long cur;
        long completed;
        int next_pending;
} __attribute__((__aligned__(1 << (7))));


static int rcu_batch_before(long a, long b)
{
        return (a - b) < 0;
}


static int rcu_batch_after(long a, long b)
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







static void rcu_qsctr_inc(int cpu)
{
        struct rcu_data *rdp = &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_data)); (typeof(&per_cpu__rcu_data)) (__ptr + (__per_cpu_offset[cpu])); }));
        rdp->passed_quiesc = 1;
}
static void rcu_bh_qsctr_inc(int cpu)
{
        struct rcu_data *rdp = &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_bh_data)); (typeof(&per_cpu__rcu_bh_data)) (__ptr + (__per_cpu_offset[cpu])); }));
        rdp->passed_quiesc = 1;
}

static int __rcu_pending(struct rcu_ctrlblk *rcp,
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

static int rcu_pending(int cpu)
{
        return __rcu_pending(&rcu_ctrlblk, &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_data)); (typeof(&per_cpu__rcu_data)) (__ptr + (__per_cpu_offset[cpu])); }))) ||
                __rcu_pending(&rcu_bh_ctrlblk, &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__rcu_bh_data)); (typeof(&per_cpu__rcu_bh_data)) (__ptr + (__per_cpu_offset[cpu])); })));
}
extern void rcu_init(void);
extern void rcu_check_callbacks(int cpu, int user);
extern void rcu_restart_cpu(int cpu);


extern void call_rcu(struct rcu_head *head, void (*func)(struct rcu_head *head)) __attribute__((regparm(3)));

extern void call_rcu_bh(struct rcu_head *head, void (*func)(struct rcu_head *head)) __attribute__((regparm(3)));

extern void synchronize_kernel(void);
extern void synchronize_rcu(void);
void synchronize_idle(void);


struct nameidata;
struct vfsmount;
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






static unsigned long
partial_name_hash(unsigned long c, unsigned long prevhash)
{
        return (prevhash + (c << 4) + (c >> 4)) * 11;
}





static unsigned long end_name_hash(unsigned long hash)
{
        return (unsigned int) hash;
}


static unsigned int
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
extern spinlock_t dcache_lock;
static void __d_drop(struct dentry *dentry)
{
        if (!(dentry->d_flags & 0x0010)) {
                dentry->d_flags |= 0x0010;
                hlist_del_rcu(&dentry->d_hash);
        }
}

static void d_drop(struct dentry *dentry)
{
        _spin_lock(&dcache_lock);
        _spin_lock(&dentry->d_lock);
        __d_drop(dentry);
        _spin_unlock(&dentry->d_lock);
        _spin_unlock(&dcache_lock);
}

static int dname_external(struct dentry *dentry)
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
static void d_add(struct dentry *entry, struct inode *inode)
{
        d_instantiate(entry, inode);
        d_rehash(entry);
}
static struct dentry *d_add_unique(struct dentry *entry, struct inode *inode)
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
static struct dentry *dget(struct dentry *dentry)
{
        if (dentry) {
                do { if (__builtin_expect(!!((!((&dentry->d_count)->counter))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (294), "i" ("include/linux/dcache.h")); } while(0);
                atomic_inc(&dentry->d_count);
        }
        return dentry;
}

extern struct dentry * dget_locked(struct dentry *);
static int d_unhashed(struct dentry *dentry)
{
        return (dentry->d_flags & 0x0010);
}

static struct dentry *dget_parent(struct dentry *dentry)
{
        struct dentry *ret;

        _spin_lock(&dentry->d_lock);
        ret = dget(dentry->d_parent);
        _spin_unlock(&dentry->d_lock);
        return ret;
}

extern void dput(struct dentry *);

static int d_mountpoint(struct dentry *dentry)
{
        return dentry->d_mounted;
}

extern struct vfsmount *lookup_mnt(struct vfsmount *, struct dentry *);
extern struct dentry *lookup_create(struct nameidata *nd, int is_dir);

extern int sysctl_vfs_cache_pressure;




struct radix_tree_root {
        unsigned int height;
        unsigned int gfp_mask;
        struct radix_tree_node *rnode;
};
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

static void radix_tree_preload_end(void)
{
        do { } while (0);
}
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




typedef __kernel_uid32_t qid_t;
typedef __u64 qsize_t;

extern spinlock_t dq_data_lock;
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
struct if_dqinfo {
        __u64 dqi_bgrace;
        __u64 dqi_igrace;
        __u32 dqi_flags;
        __u32 dqi_valid;
};



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
struct v1_mem_dqinfo {
};
struct v2_mem_dqinfo {
        unsigned int dqi_blocks;
        unsigned int dqi_free_blk;
        unsigned int dqi_free_entry;
};
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
int register_quota_format(struct quota_format_type *fmt);
void unregister_quota_format(struct quota_format_type *fmt);

struct quota_module_name {
        int qm_fmt_id;
        char *qm_mod_name;
};




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
int mapping_tagged(struct address_space *mapping, int tag);




static int mapping_mapped(struct address_space *mapping)
{
        return !prio_tree_empty(&mapping->i_mmap) ||
                !list_empty(&mapping->i_mmap_nonlinear);
}







static int mapping_writably_mapped(struct address_space *mapping)
{
        return mapping->i_mmap_writable != 0;
}
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
static loff_t i_size_read(struct inode *inode)
{

        loff_t i_size;
        unsigned int seq;

        do {
                seq = read_seqcount_begin(&inode->i_size_seqcount);
                i_size = inode->i_size;
        } while (read_seqcount_retry(&inode->i_size_seqcount, seq));
        return i_size;
}


static void i_size_write(struct inode *inode, loff_t i_size)
{

        write_seqcount_begin(&inode->i_size_seqcount);
        inode->i_size = i_size;
        write_seqcount_end(&inode->i_size_seqcount);







}

static unsigned iminor(struct inode *inode)
{
        return ((unsigned int) ((inode->i_rdev) & ((1U << 20) - 1)));
}

static unsigned imajor(struct inode *inode)
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





static int nfs_compare_fh(const struct nfs_fh *a, const struct nfs_fh *b)
{
        return a->size != b->size || __builtin_memcmp(a->data, b->data, a->size) != 0;
}

static void nfs_copy_fh(struct nfs_fh *target, const struct nfs_fh *source)
{
        target->size = source->size;
        (__builtin_constant_p(source->size) ? __constant_memcpy((target->data),(source->data),(source->size)) : __memcpy((target->data),(source->data),(source->size)));
}
enum nfs3_stable_how {
        NFS_UNSTABLE = 0,
        NFS_DATA_SYNC = 1,
        NFS_FILE_SYNC = 2
};

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
extern struct list_head file_lock_list;




struct flock {
        short l_type;
        short l_whence;
        off_t l_start;
        off_t l_len;
        pid_t l_pid;
       
};
struct flock64 {
        short l_type;
        short l_whence;
        loff_t l_start;
        loff_t l_len;
        pid_t l_pid;
       
};

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




static void get_fs_excl(void)
{
        atomic_inc(&get_current()->fs_excl);
}

static void put_fs_excl(void)
{
        atomic_dec(&get_current()->fs_excl);
}

static int has_fs_excl(void)
{
        return ((&get_current()->fs_excl)->counter);
}





static void lock_super(struct super_block * sb)
{
        get_fs_excl();
        down(&sb->s_lock);
}

static void unlock_super(struct super_block * sb)
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
extern void __mark_inode_dirty(struct inode *, int);
static void mark_inode_dirty(struct inode *inode)
{
        __mark_inode_dirty(inode, (1 | 2 | 4));
}

static void mark_inode_dirty_sync(struct inode *inode)
{
        __mark_inode_dirty(inode, 1);
}

static void touch_atime(struct vfsmount *mnt, struct dentry *dentry)
{

        update_atime(dentry->d_inode);
}

static void file_accessed(struct file *file)
{
        if (!(file->f_flags & 01000000))
                touch_atime(file->f_vfsmnt, file->f_dentry);
}

int sync_inode(struct inode *inode, struct writeback_control *wbc);
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
static int locks_verify_locked(struct inode *inode)
{
        if ((((inode)->i_sb->s_flags & (64)) && ((inode)->i_mode & (0002000 | 00010)) == 0002000))
                return locks_mandatory_locked(inode);
        return 0;
}

extern int rw_verify_area(int, struct file *, loff_t *, size_t);

static int locks_verify_truncate(struct inode *inode,
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

static int break_lease(struct inode *inode, unsigned int mode)
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
extern int check_disk_change(struct block_device *);
extern int invalidate_inodes(struct super_block *);
extern int __invalidate_device(struct block_device *);
extern int invalidate_partition(struct gendisk *, int);
unsigned long invalidate_mapping_pages(struct address_space *mapping,
                                        unsigned long start, unsigned long end);
unsigned long invalidate_inode_pages(struct address_space *mapping);
static void invalidate_remote_inode(struct inode *inode)
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
static void put_write_access(struct inode * inode)
{
        atomic_dec(&inode->i_writecount);
}
static void allow_write_access(struct file *file)
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

static void *ERR_PTR(long error)
{
        return (void *) error;
}

static long PTR_ERR(const void *ptr)
{
        return (long) ptr;
}

static long IS_ERR(const void *ptr)
{
        return __builtin_expect(!!(((unsigned long)ptr) > (unsigned long)-1000L), 0);
}


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

static struct inode *iget(struct super_block *sb, unsigned long ino)
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
static void insert_inode_hash(struct inode *inode) {
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
static int xip_truncate_page(struct address_space *mapping, loff_t from)
{
        return 0;
}


static void do_generic_file_read(struct file * filp, loff_t *ppos,
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

static ssize_t blockdev_direct_IO(int rw, struct kiocb *iocb,
        struct inode *inode, struct block_device *bdev, const struct iovec *iov,
        loff_t offset, unsigned long nr_segs, get_blocks_t get_blocks,
        dio_iodone_t end_io)
{
        return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
                                nr_segs, get_blocks, end_io, DIO_LOCKING);
}

static ssize_t blockdev_direct_IO_no_locking(int rw, struct kiocb *iocb,
        struct inode *inode, struct block_device *bdev, const struct iovec *iov,
        loff_t offset, unsigned long nr_segs, get_blocks_t get_blocks,
        dio_iodone_t end_io)
{
        return __blockdev_direct_IO(rw, iocb, inode, bdev, iov, offset,
                                nr_segs, get_blocks, end_io, DIO_NO_LOCKING);
}

static ssize_t blockdev_direct_IO_own_locking(int rw, struct kiocb *iocb,
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

static ino_t parent_ino(struct dentry *dentry)
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

static void simple_transaction_set(struct file *file, size_t n)
{
        struct simple_transaction_argresp *ar = file->private_data;

        do { if (__builtin_expect(!!((n > ((1UL << 12) - sizeof(struct simple_transaction_argresp)))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (1692), "i" ("include/linux/fs.h")); } while(0);





        asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "mfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        ar->size = n;
}
static void __attribute__((format(printf, 1, 2)))
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
static char *alloc_secdata(void)
{
        return (char *)1;
}

static void free_secdata(void *secdata)
{ }

struct mempolicy;
struct anon_vma;


extern unsigned long max_mapnr;


extern unsigned long num_physpages;
extern void * high_memory;
extern unsigned long vmalloc_earlyreserve;
extern int page_cluster;


extern int sysctl_legacy_va_layout;





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







extern unsigned long long __PAGE_KERNEL, __PAGE_KERNEL_EXEC;
extern unsigned long pg0[];
static int pte_user(pte_t pte) { return (pte).pte_low & 0x004; }
static int pte_read(pte_t pte) { return (pte).pte_low & 0x004; }
static int pte_dirty(pte_t pte) { return (pte).pte_low & 0x040; }
static int pte_young(pte_t pte) { return (pte).pte_low & 0x020; }
static int pte_write(pte_t pte) { return (pte).pte_low & 0x002; }
static int pte_huge(pte_t pte) { return ((pte).pte_low & (0x080 | 0x001)) == (0x080 | 0x001); }




static int pte_file(pte_t pte) { return (pte).pte_low & 0x040; }

static pte_t pte_rdprotect(pte_t pte) { (pte).pte_low &= ~0x004; return pte; }
static pte_t pte_exprotect(pte_t pte) { (pte).pte_low &= ~0x004; return pte; }
static pte_t pte_mkclean(pte_t pte) { (pte).pte_low &= ~0x040; return pte; }
static pte_t pte_mkold(pte_t pte) { (pte).pte_low &= ~0x020; return pte; }
static pte_t pte_wrprotect(pte_t pte) { (pte).pte_low &= ~0x002; return pte; }
static pte_t pte_mkread(pte_t pte) { (pte).pte_low |= 0x004; return pte; }
static pte_t pte_mkexec(pte_t pte) { (pte).pte_low |= 0x004; return pte; }
static pte_t pte_mkdirty(pte_t pte) { (pte).pte_low |= 0x040; return pte; }
static pte_t pte_mkyoung(pte_t pte) { (pte).pte_low |= 0x020; return pte; }
static pte_t pte_mkwrite(pte_t pte) { (pte).pte_low |= 0x002; return pte; }
static pte_t pte_mkhuge(pte_t pte) { (pte).pte_low |= (0x080 | 0x001); return pte; }





typedef struct { pgd_t pgd; } pud_t;
static int pgd_none(pgd_t pgd) { return 0; }
static int pgd_bad(pgd_t pgd) { return 0; }
static int pgd_present(pgd_t pgd) { return 1; }
static void pgd_clear(pgd_t *pgd) { }
static pud_t * pud_offset(pgd_t * pgd, unsigned long address)
{
        return (pud_t *)pgd;
}
static int pte_x(pte_t pte)
{
        return !(((pte).pte_low | ((unsigned long long)(pte).pte_high << 32)) & (1ULL<<63));
}




static int pte_exec(pte_t pte)
{
        return pte_user(pte) && pte_x(pte);
}



static int pte_exec_kernel(pte_t pte)
{
        return pte_x(pte);
}







static void set_pte(pte_t *ptep, pte_t pte)
{
        ptep->pte_high = pte.pte_high;
        __asm__ __volatile__ ("": : :"memory");
        ptep->pte_low = pte.pte_low;
}
static void pud_clear (pud_t * pud) { }
static pte_t ptep_get_and_clear(struct mm_struct *mm, unsigned long addr, pte_t *ptep)
{
        pte_t res;


        res.pte_low = ((__typeof__(*(&ptep->pte_low)))__xchg((unsigned long)(0),(&ptep->pte_low),sizeof(*(&ptep->pte_low))));
        res.pte_high = ptep->pte_high;
        ptep->pte_high = 0;

        return res;
}

static int pte_same(pte_t a, pte_t b)
{
        return a.pte_low == b.pte_low && a.pte_high == b.pte_high;
}



static int pte_none(pte_t pte)
{
        return !pte.pte_low && !pte.pte_high;
}

static unsigned long pte_pfn(pte_t pte)
{
        return (pte.pte_low >> 12) |
                (pte.pte_high << (32 - 12));
}

extern unsigned long long __supported_pte_mask;

static pte_t pfn_pte(unsigned long page_nr, pgprot_t pgprot)
{
        pte_t pte;

        pte.pte_high = (page_nr >> (32 - 12)) | (((pgprot).pgprot) >> 32);

        pte.pte_high &= (__supported_pte_mask >> 32);
        pte.pte_low = ((page_nr << 12) | ((pgprot).pgprot)) & __supported_pte_mask;

        return pte;
}

static pmd_t pfn_pmd(unsigned long page_nr, pgprot_t pgprot)
{
        return ((pmd_t) { ((((unsigned long long)page_nr << 12) | ((pgprot).pgprot)) & __supported_pte_mask) } );

}




static int ptep_test_and_clear_dirty(struct vm_area_struct *vma, unsigned long addr, pte_t *ptep)
{
        if (!pte_dirty(*ptep))
                return 0;
        return test_and_clear_bit(6, &ptep->pte_low);
}

static int ptep_test_and_clear_young(struct vm_area_struct *vma, unsigned long addr, pte_t *ptep)
{
        if (!pte_young(*ptep))
                return 0;
        return test_and_clear_bit(5, &ptep->pte_low);
}

static pte_t ptep_get_and_clear_full(struct mm_struct *mm, unsigned long addr, pte_t *ptep, int full)
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

static void ptep_set_wrprotect(struct mm_struct *mm, unsigned long addr, pte_t *ptep)
{
        clear_bit(1, &ptep->pte_low);
}
static void clone_pgd_range(pgd_t *dst, pgd_t *src, int count)
{
       (__builtin_constant_p(count * sizeof(pgd_t)) ? __constant_memcpy((dst),(src),(count * sizeof(pgd_t))) : __memcpy((dst),(src),(count * sizeof(pgd_t))));
}
static pte_t pte_modify(pte_t pte, pgprot_t newprot)
{
        pte.pte_low &= ((~((1UL << 12)-1)) | 0x020 | 0x040);
        pte.pte_low |= ((newprot).pgprot);





        pte.pte_high &= ~(1 << (63 - 32));
        pte.pte_high |= (((newprot).pgprot) >> 32) & (__supported_pte_mask >> 32);


        return pte;
}
extern pte_t *lookup_address(unsigned long address);
 extern int set_kernel_exec(unsigned long vaddr, int enable);




extern void noexec_setup(const char *str);
void pgd_clear_bad(pgd_t *);
void pud_clear_bad(pud_t *);
void pmd_clear_bad(pmd_t *);

static int pgd_none_or_clear_bad(pgd_t *pgd)
{
        if (pgd_none(*pgd))
                return 1;
        if (__builtin_expect(!!(pgd_bad(*pgd)), 0)) {
                pgd_clear_bad(pgd);
                return 1;
        }
        return 0;
}

static int pud_none_or_clear_bad(pud_t *pud)
{
        if (0)
                return 1;
        if (__builtin_expect(!!(0), 0)) {
                pud_clear_bad(pud);
                return 1;
        }
        return 0;
}

static int pmd_none_or_clear_bad(pmd_t *pmd)
{
        if ((!((*pmd).pmd)))
                return 1;
        if (__builtin_expect(!!(((((*pmd).pmd) & (~(~((1UL << 12)-1)) & ~0x004)) != (0x001 | 0x002 | 0x020 | 0x040))), 0)) {
                pmd_clear_bad(pmd);
                return 1;
        }
        return 0;
}
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
struct page {
        page_flags_t flags;

        atomic_t _count;
        atomic_t _mapcount;



        unsigned long private;






        struct address_space *mapping;






        unsigned long index;
        struct list_head lru;
};





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
struct page;

int test_clear_page_dirty(struct page *page);
int test_clear_page_writeback(struct page *page);
int test_set_page_writeback(struct page *page);

static void clear_page_dirty(struct page *page)
{
        test_clear_page_dirty(page);
}

static void set_page_writeback(struct page *page)
{
        test_set_page_writeback(page);
}
extern void __page_cache_release(struct page *) __attribute__((regparm(3)));



static int page_count(struct page *p)
{
        if ((__builtin_constant_p(14) ? constant_test_bit((14),(&(p)->flags)) : variable_test_bit((14),(&(p)->flags))))
                p = (struct page *)p->private;
        return ((&(p)->_count)->counter) + 1;
}

static void get_page(struct page *page)
{
        if (__builtin_expect(!!((__builtin_constant_p(14) ? constant_test_bit((14),(&(page)->flags)) : variable_test_bit((14),(&(page)->flags)))), 0))
                page = (struct page *)page->private;
        atomic_inc(&page->_count);
}

void put_page(struct page *page);
static unsigned long page_zonenum(struct page *page)
{
        return (page->flags >> (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0))) & ((1UL << 2) - 1);
}

struct zone;
extern struct zone *zone_table[];

static struct zone *page_zone(struct page *page)
{
        return zone_table[(page->flags >> (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0))) &
                        ((1UL << (0 + 2)) - 1)];
}

static unsigned long page_to_nid(struct page *page)
{
        if ((0 > 0 || 0 == 0))
                return (page->flags >> ((((sizeof(page_flags_t)*8) - 0) - 0) * (0 != 0))) & ((1UL << 0) - 1);
        else
                return page_zone(page)->zone_pgdat->node_id;
}
static unsigned long page_to_section(struct page *page)
{
        return (page->flags >> (((sizeof(page_flags_t)*8) - 0) * (0 != 0))) & ((1UL << 0) - 1);
}

static void set_page_zone(struct page *page, unsigned long zone)
{
        page->flags &= ~(((1UL << 2) - 1) << (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0)));
        page->flags |= (zone & ((1UL << 2) - 1)) << (((((sizeof(page_flags_t)*8) - 0) - 0) - 2) * (2 != 0));
}
static void set_page_node(struct page *page, unsigned long node)
{
        page->flags &= ~(((1UL << 0) - 1) << ((((sizeof(page_flags_t)*8) - 0) - 0) * (0 != 0)));
        page->flags |= (node & ((1UL << 0) - 1)) << ((((sizeof(page_flags_t)*8) - 0) - 0) * (0 != 0));
}
static void set_page_section(struct page *page, unsigned long section)
{
        page->flags &= ~(((1UL << 0) - 1) << (((sizeof(page_flags_t)*8) - 0) * (0 != 0)));
        page->flags |= (section & ((1UL << 0) - 1)) << (((sizeof(page_flags_t)*8) - 0) * (0 != 0));
}

static void set_page_links(struct page *page, unsigned long zone,
        unsigned long node, unsigned long pfn)
{
        set_page_zone(page, zone);
        set_page_node(page, node);
        set_page_section(page, ((pfn) >> 0));
}



extern struct page *mem_map;


static void *lowmem_page_address(struct page *page)
{
        return ((void *)((unsigned long)(((unsigned long)((page) - mem_map)) << 12)+((unsigned long)(0xC0000000UL))));
}
void *page_address(struct page *page);
void set_page_address(struct page *page, void *virtual);
void page_address_init(void);
extern struct address_space swapper_space;
static struct address_space *page_mapping(struct page *page)
{
        struct address_space *mapping = page->mapping;

        if (__builtin_expect(!!((__builtin_constant_p(15) ? constant_test_bit((15),(&(page)->flags)) : variable_test_bit((15),(&(page)->flags)))), 0))
                mapping = &swapper_space;
        else if (__builtin_expect(!!((unsigned long)mapping & 1), 0))
                mapping = ((void *)0);
        return mapping;
}

static int PageAnon(struct page *page)
{
        return ((unsigned long)page->mapping & 1) != 0;
}





static unsigned long page_index(struct page *page)
{
        if (__builtin_expect(!!((__builtin_constant_p(15) ? constant_test_bit((15),(&(page)->flags)) : variable_test_bit((15),(&(page)->flags)))), 0))
                return page->private;
        return page->index;
}






static void reset_page_mapcount(struct page *page)
{
        (((&(page)->_mapcount)->counter) = (-1));
}

static int page_mapcount(struct page *page)
{
        return ((&(page)->_mapcount)->counter) + 1;
}




static int page_mapped(struct page *page)
{
        return ((&(page)->_mapcount)->counter) >= 0;
}
extern void show_free_areas(void);


struct page *shmem_nopage(struct vm_area_struct *vma,
                        unsigned long address, int *type);
int shmem_set_policy(struct vm_area_struct *vma, struct mempolicy *new);
struct mempolicy *shmem_get_policy(struct vm_area_struct *vma,
                                        unsigned long addr);
int shmem_lock(struct file *file, int lock, struct user_struct *user);






struct file *shmem_file_setup(char *name, loff_t size, unsigned long flags);

int shmem_zero_setup(struct vm_area_struct *);

static int can_do_mlock(void)
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

static void unmap_shared_mapping_range(struct address_space *mapping,
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

static int handle_mm_fault(struct mm_struct *mm, struct vm_area_struct *vma, unsigned long address, int write_access)
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
typedef int (*shrinker_t)(int nr_to_scan, unsigned int gfp_mask);







struct shrinker;
extern struct shrinker *set_shrinker(int, shrinker_t);
extern void remove_shrinker(struct shrinker *shrinker);
static pud_t *pud_alloc(struct mm_struct *mm, pgd_t *pgd, unsigned long address)
{
        if (pgd_none(*pgd))
                return __pud_alloc(mm, pgd, address);
        return pud_offset(pgd, address);
}

static pmd_t *pmd_alloc(struct mm_struct *mm, pud_t *pud, unsigned long address)
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




static void setup_per_cpu_pageset(void) {}



void vma_prio_tree_add(struct vm_area_struct *, struct vm_area_struct *old);
void vma_prio_tree_insert(struct vm_area_struct *, struct prio_tree_root *);
void vma_prio_tree_remove(struct vm_area_struct *, struct prio_tree_root *);
struct vm_area_struct *vma_prio_tree_next(struct vm_area_struct *vma,
        struct prio_tree_iter *iter);





static void vma_nonlinear_insert(struct vm_area_struct *vma,
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

static unsigned long do_mmap(struct file *file, unsigned long addr,
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



static struct vm_area_struct * find_vma_intersection(struct mm_struct * mm, unsigned long start_addr, unsigned long end_addr)
{
        struct vm_area_struct * vma = find_vma(mm,start_addr);

        if (vma && end_addr <= vma->vm_start)
                vma = ((void *)0);
        return vma;
}

static unsigned long vma_pages(struct vm_area_struct *vma)
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







static void vm_stat_account(struct vm_area_struct *vma)
{
        __vm_stat_account(vma->vm_mm, vma->vm_flags, vma->vm_file,
                                                        vma_pages(vma));
}

static void vm_stat_unaccount(struct vm_area_struct *vma)
{
        __vm_stat_account(vma->vm_mm, vma->vm_flags, vma->vm_file,
                                                        -vma_pages(vma));
}


extern void update_mem_hiwater(struct task_struct *tsk);


static void
kernel_map_pages(struct page *page, int numpages, int enable)
{
}


extern struct vm_area_struct *get_gate_vma(struct task_struct *tsk);




int in_gate_area_no_task(unsigned long addr);






extern unsigned int pcibios_assign_all_busses(void);





extern unsigned long pci_mem_start;





void pcibios_config_init(void);
struct pci_bus * pcibios_scan_root(int bus);

void pcibios_set_master(struct pci_dev *dev);
void pcibios_penalize_isa_irq(int irq, int active);
struct irq_routing_table *pcibios_get_irq_routing_table(void);
int pcibios_set_irq_routing(struct pci_dev *dev, int pin, int irq);
struct pci_dev;
static dma64_addr_t
pci_dac_page_to_dma(struct pci_dev *pdev, struct page *page, unsigned long offset, int direction)
{
        return ((dma64_addr_t) ((dma_addr_t)((unsigned long)((page) - mem_map)) << 12) +
                (dma64_addr_t) offset);
}

static struct page *
pci_dac_dma_to_page(struct pci_dev *pdev, dma64_addr_t dma_addr)
{
        return (mem_map + (dma_addr >> 12));
}

static unsigned long
pci_dac_dma_to_offset(struct pci_dev *pdev, dma64_addr_t dma_addr)
{
        return (dma_addr & ~(~((1UL << 12)-1)));
}

static void
pci_dac_dma_sync_single_for_cpu(struct pci_dev *pdev, dma64_addr_t dma_addr, size_t len, int direction)
{
}

static void
pci_dac_dma_sync_single_for_device(struct pci_dev *pdev, dma64_addr_t dma_addr, size_t len, int direction)
{
        flush_write_buffers();
}


extern int pci_mmap_page_range(struct pci_dev *dev, struct vm_area_struct *vma,
                               enum pci_mmap_state mmap_state, int write_combine);


static void pcibios_add_platform_entries(struct pci_dev *dev)
{
}


static void pci_dma_burst_advice(struct pci_dev *pdev,
                                        enum pci_dma_burst_strategy *strat,
                                        unsigned long *strategy_parameter)
{
        *strat = PCI_DMA_BURST_INFINITY;
        *strategy_parameter = ~0UL;
}











enum dma_data_direction {
        DMA_BIDIRECTIONAL = 0,
        DMA_TO_DEVICE = 1,
        DMA_FROM_DEVICE = 2,
        DMA_NONE = 3,
};
void *dma_alloc_coherent(struct device *dev, size_t size,
                           dma_addr_t *dma_handle, gfp_t flag);

void dma_free_coherent(struct device *dev, size_t size,
                         void *vaddr, dma_addr_t dma_handle);

static dma_addr_t
dma_map_single(struct device *dev, void *ptr, size_t size,
               enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (23), "i" ("include/asm/dma-mapping.h")); } while(0);
        flush_write_buffers();
        return virt_to_phys(ptr);
}

static void
dma_unmap_single(struct device *dev, dma_addr_t dma_addr, size_t size,
                 enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (32), "i" ("include/asm/dma-mapping.h")); } while(0);
}

static int
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

static dma_addr_t
dma_map_page(struct device *dev, struct page *page, unsigned long offset,
             size_t size, enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (57), "i" ("include/asm/dma-mapping.h")); } while(0);
        return ((dma_addr_t)((unsigned long)((page) - mem_map)) << 12) + offset;
}

static void
dma_unmap_page(struct device *dev, dma_addr_t dma_address, size_t size,
               enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (65), "i" ("include/asm/dma-mapping.h")); } while(0);
}


static void
dma_unmap_sg(struct device *dev, struct scatterlist *sg, int nhwentries,
             enum dma_data_direction direction)
{
        do { if (__builtin_expect(!!((direction == DMA_NONE)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (73), "i" ("include/asm/dma-mapping.h")); } while(0);
}

static void
dma_sync_single_for_cpu(struct device *dev, dma_addr_t dma_handle, size_t size,
                        enum dma_data_direction direction)
{
}

static void
dma_sync_single_for_device(struct device *dev, dma_addr_t dma_handle, size_t size,
                        enum dma_data_direction direction)
{
        flush_write_buffers();
}

static void
dma_sync_single_range_for_cpu(struct device *dev, dma_addr_t dma_handle,
                              unsigned long offset, size_t size,
                              enum dma_data_direction direction)
{
}

static void
dma_sync_single_range_for_device(struct device *dev, dma_addr_t dma_handle,
                                 unsigned long offset, size_t size,
                                 enum dma_data_direction direction)
{
        flush_write_buffers();
}

static void
dma_sync_sg_for_cpu(struct device *dev, struct scatterlist *sg, int nelems,
                    enum dma_data_direction direction)
{
}

static void
dma_sync_sg_for_device(struct device *dev, struct scatterlist *sg, int nelems,
                    enum dma_data_direction direction)
{
        flush_write_buffers();
}

static int
dma_mapping_error(dma_addr_t dma_addr)
{
        return 0;
}

static int
dma_supported(struct device *dev, u64 mask)
{





        if(mask < 0x00ffffff)
                return 0;

        return 1;
}

static int
dma_set_mask(struct device *dev, u64 mask)
{
        if(!dev->dma_mask || !dma_supported(dev, mask))
                return -5;

        *dev->dma_mask = mask;

        return 0;
}

static int
dma_get_cache_alignment(void)
{


        return (1 << 7);
}



static void
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





extern u64 dma_get_required_mask(struct device *dev);




static int
pci_dma_supported(struct pci_dev *hwdev, u64 mask)
{
        return dma_supported(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, mask);
}

static void *
pci_alloc_consistent(struct pci_dev *hwdev, size_t size,
                     dma_addr_t *dma_handle)
{
        return dma_alloc_coherent(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, size, dma_handle, (0x20u));
}

static void
pci_free_consistent(struct pci_dev *hwdev, size_t size,
                    void *vaddr, dma_addr_t dma_handle)
{
        dma_free_coherent(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, size, vaddr, dma_handle);
}

static dma_addr_t
pci_map_single(struct pci_dev *hwdev, void *ptr, size_t size, int direction)
{
        return dma_map_single(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, ptr, size, (enum dma_data_direction)direction);
}

static void
pci_unmap_single(struct pci_dev *hwdev, dma_addr_t dma_addr,
                 size_t size, int direction)
{
        dma_unmap_single(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_addr, size, (enum dma_data_direction)direction);
}

static dma_addr_t
pci_map_page(struct pci_dev *hwdev, struct page *page,
             unsigned long offset, size_t size, int direction)
{
        return dma_map_page(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, page, offset, size, (enum dma_data_direction)direction);
}

static void
pci_unmap_page(struct pci_dev *hwdev, dma_addr_t dma_address,
               size_t size, int direction)
{
        dma_unmap_page(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_address, size, (enum dma_data_direction)direction);
}

static int
pci_map_sg(struct pci_dev *hwdev, struct scatterlist *sg,
           int nents, int direction)
{
        return dma_map_sg(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nents, (enum dma_data_direction)direction);
}

static void
pci_unmap_sg(struct pci_dev *hwdev, struct scatterlist *sg,
             int nents, int direction)
{
        dma_unmap_sg(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nents, (enum dma_data_direction)direction);
}

static void
pci_dma_sync_single_for_cpu(struct pci_dev *hwdev, dma_addr_t dma_handle,
                    size_t size, int direction)
{
        dma_sync_single_for_cpu(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_handle, size, (enum dma_data_direction)direction);
}

static void
pci_dma_sync_single_for_device(struct pci_dev *hwdev, dma_addr_t dma_handle,
                    size_t size, int direction)
{
        dma_sync_single_for_device(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, dma_handle, size, (enum dma_data_direction)direction);
}

static void
pci_dma_sync_sg_for_cpu(struct pci_dev *hwdev, struct scatterlist *sg,
                int nelems, int direction)
{
        dma_sync_sg_for_cpu(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nelems, (enum dma_data_direction)direction);
}

static void
pci_dma_sync_sg_for_device(struct pci_dev *hwdev, struct scatterlist *sg,
                int nelems, int direction)
{
        dma_sync_sg_for_device(hwdev == ((void *)0) ? ((void *)0) : &hwdev->dev, sg, nelems, (enum dma_data_direction)direction);
}

static int
pci_dma_mapping_error(dma_addr_t dma_addr)
{
        return dma_mapping_error(dma_addr);
}


static void
pcibios_resource_to_bus(struct pci_dev *dev, struct pci_bus_region *region,
                         struct resource *res)
{
        region->start = res->start;
        region->end = res->end;
}

static void
pcibios_bus_to_resource(struct pci_dev *dev, struct resource *res,
                        struct pci_bus_region *region)
{
        res->start = region->start;
        res->end = region->end;
}

static struct resource *
pcibios_select_root(struct pci_dev *pdev, struct resource *res)
{
        struct resource *root = ((void *)0);

        if (res->flags & 0x00000100)
                root = &ioport_resource;
        if (res->flags & 0x00000200)
                root = &iomem_resource;

        return root;
}




static int pci_get_legacy_ide_irq(struct pci_dev *dev, int channel)
{
        return channel ? 15 : 14;
}
static void *pci_get_drvdata (struct pci_dev *pdev)
{
        return dev_get_drvdata(&pdev->dev);
}

static void pci_set_drvdata (struct pci_dev *pdev, void *data)
{
        dev_set_drvdata(&pdev->dev, data);
}




static char *pci_name(struct pci_dev *pdev)
{
        return pdev->dev.bus_id;
}






static void pci_resource_to_user(const struct pci_dev *dev, int bar,
                const struct resource *rsrc, u64 *start, u64 *end)
{
        *start = rsrc->start;
        *end = rsrc->end;
}
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
void pci_fixup_device(enum pci_fixup_pass pass, struct pci_dev *dev);

extern int pci_pci_problems;












typedef unsigned char cc_t;
typedef unsigned int speed_t;
typedef unsigned int tcflag_t;


struct termios {
        tcflag_t c_iflag;
        tcflag_t c_oflag;
        tcflag_t c_cflag;
        tcflag_t c_lflag;
        cc_t c_line;
        cc_t c_cc[19];
};

struct winsize {
        unsigned short ws_row;
        unsigned short ws_col;
        unsigned short ws_xpixel;
        unsigned short ws_ypixel;
};


struct termio {
        unsigned short c_iflag;
        unsigned short c_oflag;
        unsigned short c_cflag;
        unsigned short c_lflag;
        unsigned char c_line;
        unsigned char c_cc[8];
};





struct cdev {
        struct kobject kobj;
        struct module *owner;
        struct file_operations *ops;
        struct list_head list;
        dev_t dev;
        unsigned int count;
};

void cdev_init(struct cdev *, struct file_operations *);

struct cdev *cdev_alloc(void);

void cdev_put(struct cdev *p);

int cdev_add(struct cdev *, dev_t, unsigned);

void cdev_del(struct cdev *);

void cd_forget(struct inode *);

struct tty_struct;

struct tty_operations {
        int (*open)(struct tty_struct * tty, struct file * filp);
        void (*close)(struct tty_struct * tty, struct file * filp);
        int (*write)(struct tty_struct * tty,
                      const unsigned char *buf, int count);
        void (*put_char)(struct tty_struct *tty, unsigned char ch);
        void (*flush_chars)(struct tty_struct *tty);
        int (*write_room)(struct tty_struct *tty);
        int (*chars_in_buffer)(struct tty_struct *tty);
        int (*ioctl)(struct tty_struct *tty, struct file * file,
                    unsigned int cmd, unsigned long arg);
        void (*set_termios)(struct tty_struct *tty, struct termios * old);
        void (*throttle)(struct tty_struct * tty);
        void (*unthrottle)(struct tty_struct * tty);
        void (*stop)(struct tty_struct *tty);
        void (*start)(struct tty_struct *tty);
        void (*hangup)(struct tty_struct *tty);
        void (*break_ctl)(struct tty_struct *tty, int state);
        void (*flush_buffer)(struct tty_struct *tty);
        void (*set_ldisc)(struct tty_struct *tty);
        void (*wait_until_sent)(struct tty_struct *tty, int timeout);
        void (*send_xchar)(struct tty_struct *tty, char ch);
        int (*read_proc)(char *page, char **start, off_t off,
                          int count, int *eof, void *data);
        int (*write_proc)(struct file *file, const char *buffer,
                          unsigned long count, void *data);
        int (*tiocmget)(struct tty_struct *tty, struct file *file);
        int (*tiocmset)(struct tty_struct *tty, struct file *file,
                        unsigned int set, unsigned int clear);
};

struct tty_driver {
        int magic;
        struct cdev cdev;
        struct module *owner;
        const char *driver_name;
        const char *devfs_name;
        const char *name;
        int name_base;
        int major;
        int minor_start;
        int minor_num;
        int num;
        short type;
        short subtype;
        struct termios init_termios;
        int flags;
        int refcount;
        struct proc_dir_entry *proc_entry;
        struct tty_driver *other;




        struct tty_struct **ttys;
        struct termios **termios;
        struct termios **termios_locked;
        void *driver_state;





        int (*open)(struct tty_struct * tty, struct file * filp);
        void (*close)(struct tty_struct * tty, struct file * filp);
        int (*write)(struct tty_struct * tty,
                      const unsigned char *buf, int count);
        void (*put_char)(struct tty_struct *tty, unsigned char ch);
        void (*flush_chars)(struct tty_struct *tty);
        int (*write_room)(struct tty_struct *tty);
        int (*chars_in_buffer)(struct tty_struct *tty);
        int (*ioctl)(struct tty_struct *tty, struct file * file,
                    unsigned int cmd, unsigned long arg);
        void (*set_termios)(struct tty_struct *tty, struct termios * old);
        void (*throttle)(struct tty_struct * tty);
        void (*unthrottle)(struct tty_struct * tty);
        void (*stop)(struct tty_struct *tty);
        void (*start)(struct tty_struct *tty);
        void (*hangup)(struct tty_struct *tty);
        void (*break_ctl)(struct tty_struct *tty, int state);
        void (*flush_buffer)(struct tty_struct *tty);
        void (*set_ldisc)(struct tty_struct *tty);
        void (*wait_until_sent)(struct tty_struct *tty, int timeout);
        void (*send_xchar)(struct tty_struct *tty, char ch);
        int (*read_proc)(char *page, char **start, off_t off,
                          int count, int *eof, void *data);
        int (*write_proc)(struct file *file, const char *buffer,
                          unsigned long count, void *data);
        int (*tiocmget)(struct tty_struct *tty, struct file *file);
        int (*tiocmset)(struct tty_struct *tty, struct file *file,
                        unsigned int set, unsigned int clear);

        struct list_head tty_drivers;
};

extern struct list_head tty_drivers;

struct tty_driver *alloc_tty_driver(int lines);
void put_tty_driver(struct tty_driver *driver);
void tty_set_operations(struct tty_driver *driver, struct tty_operations *op);
struct tty_ldisc {
        int magic;
        char *name;
        int num;
        int flags;




        int (*open)(struct tty_struct *);
        void (*close)(struct tty_struct *);
        void (*flush_buffer)(struct tty_struct *tty);
        ssize_t (*chars_in_buffer)(struct tty_struct *tty);
        ssize_t (*read)(struct tty_struct * tty, struct file * file,
                        unsigned char * buf, size_t nr);
        ssize_t (*write)(struct tty_struct * tty, struct file * file,
                         const unsigned char * buf, size_t nr);
        int (*ioctl)(struct tty_struct * tty, struct file * file,
                         unsigned int cmd, unsigned long arg);
        void (*set_termios)(struct tty_struct *tty, struct termios * old);
        unsigned int (*poll)(struct tty_struct *, struct file *,
                             struct poll_table_struct *);
        int (*hangup)(struct tty_struct *tty);




        void (*receive_buf)(struct tty_struct *, const unsigned char *cp,
                               char *fp, int count);
        int (*receive_room)(struct tty_struct *);
        void (*write_wakeup)(struct tty_struct *);

        struct module *owner;

        int refcount;
};
struct screen_info {
        u8 orig_x;
        u8 orig_y;
        u16 dontuse1;
        u16 orig_video_page;
        u8 orig_video_mode;
        u8 orig_video_cols;
        u16 unused2;
        u16 orig_video_ega_bx;
        u16 unused3;
        u8 orig_video_lines;
        u8 orig_video_isVGA;
        u16 orig_video_points;


        u16 lfb_width;
        u16 lfb_height;
        u16 lfb_depth;
        u32 lfb_base;
        u32 lfb_size;
        u16 dontuse2, dontuse3;
        u16 lfb_linelength;
        u8 red_size;
        u8 red_pos;
        u8 green_size;
        u8 green_pos;
        u8 blue_size;
        u8 blue_pos;
        u8 rsvd_size;
        u8 rsvd_pos;
        u16 vesapm_seg;
        u16 vesapm_off;
        u16 pages;
        u16 vesa_attributes;
        u32 capabilities;

};

extern struct screen_info screen_info;
struct tty_flip_buffer {
        struct work_struct work;
        struct semaphore pty_sem;
        char *char_buf_ptr;
        unsigned char *flag_buf_ptr;
        int count;
        int buf_num;
        unsigned char char_buf[2*512];
        char flag_buf[2*512];
        unsigned char slop[4];
};
struct device;
struct tty_struct {
        int magic;
        struct tty_driver *driver;
        int index;
        struct tty_ldisc ldisc;
        struct semaphore termios_sem;
        struct termios *termios, *termios_locked;
        char name[64];
        int pgrp;
        int session;
        unsigned long flags;
        int count;
        struct winsize winsize;
        unsigned char stopped:1, hw_stopped:1, flow_stopped:1, packet:1;
        unsigned char low_latency:1, warned:1;
        unsigned char ctrl_status;

        struct tty_struct *link;
        struct fasync_struct *fasync;
        struct tty_flip_buffer flip;
        int max_flip_cnt;
        int alt_speed;
        wait_queue_head_t write_wait;
        wait_queue_head_t read_wait;
        struct work_struct hangup_work;
        void *disc_data;
        void *driver_data;
        struct list_head tty_files;







        unsigned int column;
        unsigned char lnext:1, erasing:1, raw:1, real_raw:1, icanon:1;
        unsigned char closing:1;
        unsigned short minimum_to_wake;
        unsigned long overrun_time;
        int num_overrun;
        unsigned long process_char_map[256/(8*sizeof(unsigned long))];
        char *read_buf;
        int read_head;
        int read_tail;
        int read_cnt;
        unsigned long read_flags[4096/(8*sizeof(unsigned long))];
        int canon_data;
        unsigned long canon_head;
        unsigned int canon_column;
        struct semaphore atomic_read;
        struct semaphore atomic_write;
        unsigned char *write_buf;
        int write_cnt;
        spinlock_t read_lock;

        struct work_struct SAK_work;
};
extern void tty_write_flush(struct tty_struct *);

extern struct termios tty_std_termios;
extern int fg_console, last_console, want_console;

extern int kmsg_redirect;

extern void console_init(void);
extern int vcs_init(void);

extern int tty_paranoia_check(struct tty_struct *tty, struct inode *inode,
                              const char *routine);
extern char *tty_name(struct tty_struct *tty, char *buf);
extern void tty_wait_until_sent(struct tty_struct * tty, long timeout);
extern int tty_check_change(struct tty_struct * tty);
extern void stop_tty(struct tty_struct * tty);
extern void start_tty(struct tty_struct * tty);
extern int tty_register_ldisc(int disc, struct tty_ldisc *new_ldisc);
extern int tty_unregister_ldisc(int disc);
extern int tty_register_driver(struct tty_driver *driver);
extern int tty_unregister_driver(struct tty_driver *driver);
extern void tty_register_device(struct tty_driver *driver, unsigned index, struct device *dev);
extern void tty_unregister_device(struct tty_driver *driver, unsigned index);
extern int tty_read_raw_data(struct tty_struct *tty, unsigned char *bufp,
                             int buflen);
extern void tty_write_message(struct tty_struct *tty, char *msg);

extern int is_orphaned_pgrp(int pgrp);
extern int is_ignored(int sig);
extern int tty_signal(int sig, struct tty_struct *tty);
extern void tty_hangup(struct tty_struct * tty);
extern void tty_vhangup(struct tty_struct * tty);
extern void tty_unhangup(struct file *filp);
extern int tty_hung_up_p(struct file * filp);
extern void do_SAK(struct tty_struct *tty);
extern void disassociate_ctty(int priv);
extern void tty_flip_buffer_push(struct tty_struct *tty);
extern int tty_get_baud_rate(struct tty_struct *tty);
extern int tty_termios_baud_rate(struct termios *termios);

extern struct tty_ldisc *tty_ldisc_ref(struct tty_struct *);
extern void tty_ldisc_deref(struct tty_ldisc *);
extern struct tty_ldisc *tty_ldisc_ref_wait(struct tty_struct *);

extern struct tty_ldisc *tty_ldisc_get(int);
extern void tty_ldisc_put(int);

extern void tty_wakeup(struct tty_struct *tty);
extern void tty_ldisc_flush(struct tty_struct *tty);

struct semaphore;
extern struct semaphore tty_sem;


extern struct tty_ldisc tty_ldisc_N_TTY;


extern int n_tty_ioctl(struct tty_struct * tty, struct file * file,
                       unsigned int cmd, unsigned long arg);



extern void serial_console_init(void);



extern int pcxe_open(struct tty_struct *tty, struct file *filp);



extern void console_print(const char *);



extern int vt_ioctl(struct tty_struct *tty, struct file * file,
                    unsigned int cmd, unsigned long arg);

static dev_t tty_devnum(struct tty_struct *tty)
{
        return (((tty->driver->major) << 20) | (tty->driver->minor_start)) + tty->index;
}
static void tty_insert_flip_char(struct tty_struct *tty,
                                   unsigned char ch, char flag)
{
        if (tty->flip.count < 512) {
                tty->flip.count++;
                *tty->flip.flag_buf_ptr++ = flag;
                *tty->flip.char_buf_ptr++ = ch;
        }
}

static void tty_schedule_flip(struct tty_struct *tty)
{
        schedule_delayed_work(&tty->flip.work, 1);
}
struct async_icount {
        __u32 cts, dsr, rng, dcd, tx, rx;
        __u32 frame, parity, overrun, brk;
        __u32 buf_overrun;
};
struct serial_struct {
        int type;
        int line;
        unsigned int port;
        int irq;
        int flags;
        int xmit_fifo_size;
        int custom_divisor;
        int baud_base;
        unsigned short close_delay;
        char io_type;
        char reserved_char[1];
        int hub6;
        unsigned short closing_wait;
        unsigned short closing_wait2;
        unsigned char *iomem_base;
        unsigned short iomem_reg_shift;
        unsigned int port_high;
        unsigned long iomap_base;
};
struct serial_uart_config {
        char *name;
        int dfl_xmit_fifo_size;
        int flags;
};
struct serial_multiport_struct {
        int irq;
        int port1;
        unsigned char mask1, match1;
        int port2;
        unsigned char mask2, match2;
        int port3;
        unsigned char mask3, match3;
        int port4;
        unsigned char mask4, match4;
        int port_monitor;
        int reserved[32];
};





struct serial_icounter_struct {
        int cts, dsr, rng, dcd;
        int rx, tx;
        int frame, overrun, parity, brk;
        int buf_overrun;
        int reserved[9];
};






struct uart_port;
extern int early_serial_setup(struct uart_port *port);
extern int early_serial_console_init(char *options);
extern int serial8250_start_console(struct uart_port *port, char *options);



extern int ptrace_readdata(struct task_struct *tsk, unsigned long src, char *dst, int len);
extern int ptrace_writedata(struct task_struct *tsk, char *src, unsigned long dst, int len);
extern int ptrace_attach(struct task_struct *tsk);
extern int ptrace_detach(struct task_struct *, unsigned int);
extern void ptrace_disable(struct task_struct *);
extern int ptrace_check_attach(struct task_struct *task, int kill);
extern int ptrace_request(struct task_struct *child, long request, long addr, long data);
extern void ptrace_notify(int exit_code);
extern void __ptrace_link(struct task_struct *child,
                          struct task_struct *new_parent);
extern void __ptrace_unlink(struct task_struct *child);
extern void ptrace_untrace(struct task_struct *child);
extern int ptrace_may_attach(struct task_struct *task);

static void ptrace_link(struct task_struct *child,
                               struct task_struct *new_parent)
{
        if (__builtin_expect(!!(child->ptrace), 0))
                __ptrace_link(child, new_parent);
}
static void ptrace_unlink(struct task_struct *child)
{
        if (__builtin_expect(!!(child->ptrace), 0))
                __ptrace_unlink(child);
}



extern unsigned long loops_per_jiffy;

extern void __bad_udelay(void);
extern void __bad_ndelay(void);

extern void __udelay(unsigned long usecs);
extern void __ndelay(unsigned long nsecs);
extern void __const_udelay(unsigned long usecs);
extern void __delay(unsigned long loops);
void calibrate_delay(void);
void msleep(unsigned int msecs);
unsigned long msleep_interruptible(unsigned int msecs);

static void ssleep(unsigned int seconds)
{
        msleep(seconds * 1000);
}

struct __kernel_sockaddr_storage {
        unsigned short ss_family;

        char __data[128 - sizeof(unsigned short)];


} __attribute__ ((aligned((__alignof__ (struct sockaddr *)))));








struct iovec
{
        void *iov_base;
        __kernel_size_t iov_len;
};



struct kvec {
        void *iov_base;
        size_t iov_len;
};
static size_t iov_length(const struct iovec *iov, unsigned long nr_segs)
{
        unsigned long seg;
        size_t ret = 0;

        for (seg = 0; seg < nr_segs; seg++)
                ret += iov[seg].iov_len;
        return ret;
}

unsigned long iov_shorten(struct iovec *iov, unsigned long nr_segs, size_t to);



extern int sysctl_somaxconn;
extern void sock_init(void);

struct seq_file;
extern void socket_seq_show(struct seq_file *seq);


typedef unsigned short sa_family_t;





struct sockaddr {
        sa_family_t sa_family;
        char sa_data[14];
};

struct linger {
        int l_onoff;
        int l_linger;
};
struct msghdr {
        void * msg_name;
        int msg_namelen;
        struct iovec * msg_iov;
        __kernel_size_t msg_iovlen;
        void * msg_control;
        __kernel_size_t msg_controllen;
        unsigned msg_flags;
};







struct cmsghdr {
        __kernel_size_t cmsg_len;
        int cmsg_level;
        int cmsg_type;
};
static struct cmsghdr * __cmsg_nxthdr(void *__ctl, __kernel_size_t __size,
                                               struct cmsghdr *__cmsg)
{
        struct cmsghdr * __ptr;

        __ptr = (struct cmsghdr*)(((unsigned char *) __cmsg) + ( ((__cmsg->cmsg_len)+sizeof(long)-1) & ~(sizeof(long)-1) ));
        if ((unsigned long)((char*)(__ptr+1) - (char *) __ctl) > __size)
                return (struct cmsghdr *)0;

        return __ptr;
}

static struct cmsghdr * cmsg_nxthdr (struct msghdr *__msg, struct cmsghdr *__cmsg)
{
        return __cmsg_nxthdr(__msg->msg_control, __msg->msg_controllen, __cmsg);
}






struct ucred {
        __u32 pid;
        __u32 uid;
        __u32 gid;
};
extern int memcpy_fromiovec(unsigned char *kdata, struct iovec *iov, int len);
extern int memcpy_fromiovecend(unsigned char *kdata, struct iovec *iov,
                                int offset, int len);
extern int csum_partial_copy_fromiovecend(unsigned char *kdata,
                                          struct iovec *iov,
                                          int offset,
                                          unsigned int len, int *csump);

extern int verify_iovec(struct msghdr *m, struct iovec *iov, char *address, int mode);
extern int memcpy_toiovec(struct iovec *v, unsigned char *kdata, int len);
extern int move_addr_to_user(void *kaddr, int klen, void *uaddr, int *ulen);
extern int move_addr_to_kernel(void *uaddr, int ulen, void *kaddr);
extern int put_cmsg(struct msghdr*, int level, int type, int len, void *data);






typedef struct {
        unsigned int clock_rate;
        unsigned int clock_type;
        unsigned short loopback;
} sync_serial_settings;

typedef struct {
        unsigned int clock_rate;
        unsigned int clock_type;
        unsigned short loopback;
        unsigned int slot_map;
} te1_settings;

typedef struct {
        unsigned short encoding;
        unsigned short parity;
} raw_hdlc_proto;

typedef struct {
        unsigned int t391;
        unsigned int t392;
        unsigned int n391;
        unsigned int n392;
        unsigned int n393;
        unsigned short lmi;
        unsigned short dce;
} fr_proto;

typedef struct {
        unsigned int dlci;
} fr_proto_pvc;

typedef struct {
        unsigned int dlci;
        char master[16];
}fr_proto_pvc_info;

typedef struct {
    unsigned int interval;
    unsigned int timeout;
} cisco_proto;
struct ifmap
{
        unsigned long mem_start;
        unsigned long mem_end;
        unsigned short base_addr;
        unsigned char irq;
        unsigned char dma;
        unsigned char port;

};

struct if_settings
{
        unsigned int type;
        unsigned int size;
        union {

                raw_hdlc_proto *raw_hdlc;
                cisco_proto *cisco;
                fr_proto *fr;
                fr_proto_pvc *fr_pvc;
                fr_proto_pvc_info *fr_pvc_info;


                sync_serial_settings *sync;
                te1_settings *te1;
        } ifs_ifsu;
};
struct ifreq
{

        union
        {
                char ifrn_name[16];
        } ifr_ifrn;

        union {
                struct sockaddr ifru_addr;
                struct sockaddr ifru_dstaddr;
                struct sockaddr ifru_broadaddr;
                struct sockaddr ifru_netmask;
                struct sockaddr ifru_hwaddr;
                short ifru_flags;
                int ifru_ivalue;
                int ifru_mtu;
                struct ifmap ifru_map;
                char ifru_slave[16];
                char ifru_newname[16];
                void * ifru_data;
                struct if_settings ifru_settings;
        } ifr_ifru;
};
struct ifconf
{
        int ifc_len;
        union
        {
                char *ifcu_buf;
                struct ifreq *ifcu_req;
        } ifc_ifcu;
};
struct ethhdr {
        unsigned char h_dest[6];
        unsigned char h_source[6];
        unsigned short h_proto;
} __attribute__((packed));









void global_flush_tlb(void);
int change_page_attr(struct page *page, int numpages, pgprot_t prot);



extern unsigned long pgkern_mask;
extern void flush_tlb_all(void);
extern void flush_tlb_current_task(void);
extern void flush_tlb_mm(struct mm_struct *);
extern void flush_tlb_page(struct vm_area_struct *, unsigned long);



static void flush_tlb_range(struct vm_area_struct * vma, unsigned long start, unsigned long end)
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






static void flush_tlb_pgtables(struct mm_struct *mm,
                                      unsigned long start, unsigned long end)
{

}


extern unsigned long highstart_pfn, highend_pfn;

extern pte_t *kmap_pte;
extern pgprot_t kmap_prot;
extern pte_t *pkmap_page_table;
extern void * kmap_high(struct page *page) __attribute__((regparm(3)));
extern void kunmap_high(struct page *page) __attribute__((regparm(3)));

void *kmap(struct page *page);
void kunmap(struct page *page);
void *kmap_atomic(struct page *page, enum km_type type);
void kunmap_atomic(void *kvaddr, enum km_type type);
void *kmap_atomic_pfn(unsigned long pfn, enum km_type type);
struct page *kmap_atomic_to_page(void *ptr);


unsigned int nr_free_highpages(void);
static void clear_user_highpage(struct page *page, unsigned long vaddr)
{
        void *addr = kmap_atomic(page, KM_USER0);
        (__builtin_constant_p(0) ? (__builtin_constant_p(((1UL << 12))) ? __constant_c_and_count_memset((((void *)(addr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12)))) : __constant_c_memset((((void *)(addr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12))))) : (__builtin_constant_p(((1UL << 12))) ? __memset_generic(((((void *)(addr)))),(((0))),((((1UL << 12))))) : __memset_generic((((void *)(addr))),((0)),(((1UL << 12))))));
        kunmap_atomic(addr, KM_USER0);

        __asm__ __volatile__ ("": : :"memory");
}
static void clear_highpage(struct page *page)
{
        void *kaddr = kmap_atomic(page, KM_USER0);
        (__builtin_constant_p(0) ? (__builtin_constant_p(((1UL << 12))) ? __constant_c_and_count_memset((((void *)(kaddr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12)))) : __constant_c_memset((((void *)(kaddr))),((0x01010101UL*(unsigned char)(0))),(((1UL << 12))))) : (__builtin_constant_p(((1UL << 12))) ? __memset_generic(((((void *)(kaddr)))),(((0))),((((1UL << 12))))) : __memset_generic((((void *)(kaddr))),((0)),(((1UL << 12))))));
        kunmap_atomic(kaddr, KM_USER0);
}




static void memclear_highpage_flush(struct page *page, unsigned int offset, unsigned int size)
{
        void *kaddr;

        do { if (__builtin_expect(!!((offset + size > (1UL << 12))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (73), "i" ("include/linux/highmem.h")); } while(0);

        kaddr = kmap_atomic(page, KM_USER0);
        (__builtin_constant_p(0) ? (__builtin_constant_p((size)) ? __constant_c_and_count_memset((((char *)kaddr + offset)),((0x01010101UL*(unsigned char)(0))),((size))) : __constant_c_memset((((char *)kaddr + offset)),((0x01010101UL*(unsigned char)(0))),((size)))) : (__builtin_constant_p((size)) ? __memset_generic(((((char *)kaddr + offset))),(((0))),(((size)))) : __memset_generic((((char *)kaddr + offset)),((0)),((size)))));
        do { } while (0);
        kunmap_atomic(kaddr, KM_USER0);
}

static void copy_user_highpage(struct page *to, struct page *from, unsigned long vaddr)
{
        char *vfrom, *vto;

        vfrom = kmap_atomic(from, KM_USER0);
        vto = kmap_atomic(to, KM_USER1);
        (__builtin_constant_p((1UL << 12)) ? __constant_memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))) : __memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))));
        kunmap_atomic(vfrom, KM_USER0);
        kunmap_atomic(vto, KM_USER1);

        __asm__ __volatile__ ("": : :"memory");
}

static void copy_highpage(struct page *to, struct page *from)
{
        char *vfrom, *vto;

        vfrom = kmap_atomic(from, KM_USER0);
        vto = kmap_atomic(to, KM_USER1);
        (__builtin_constant_p((1UL << 12)) ? __constant_memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))) : __memcpy(((void *)(vto)),((void *)(vfrom)),((1UL << 12))));
        kunmap_atomic(vfrom, KM_USER0);
        kunmap_atomic(vto, KM_USER1);
}



struct pollfd {
        int fd;
        short events;
        short revents;
};







struct exception_table_entry
{
        unsigned long insn, fixup;
};

extern int fixup_exception(struct pt_regs *regs);
extern void __get_user_1(void);
extern void __get_user_2(void);
extern void __get_user_4(void);
extern void __put_user_bad(void);





extern void __put_user_1(void);
extern void __put_user_2(void);
extern void __put_user_4(void);
extern void __put_user_8(void);
struct __large_struct { unsigned long buf[100]; };
extern long __get_user_bad(void);
unsigned long __copy_to_user_ll(void *to,
                                const void *from, unsigned long n);
unsigned long __copy_from_user_ll(void *to,
                                const void *from, unsigned long n);
static unsigned long
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

static unsigned long
__copy_to_user(void *to, const void *from, unsigned long n)
{
       do { do { } while (0); } while (0);
       return __copy_to_user_inatomic(to, from, n);
}
static unsigned long
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

static unsigned long
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
long strnlen_user(const char *str, long n);
unsigned long clear_user(void *mem, unsigned long len);
unsigned long __clear_user(void *mem, unsigned long len);

struct poll_table_struct;




typedef void (*poll_queue_proc)(struct file *, wait_queue_head_t *, struct poll_table_struct *);

typedef struct poll_table_struct {
        poll_queue_proc qproc;
} poll_table;

static void poll_wait(struct file * filp, wait_queue_head_t * wait_address, poll_table *p)
{
        if (p && wait_address)
                p->qproc(filp, wait_address, p);
}

static void init_poll_funcptr(poll_table *pt, poll_queue_proc qproc)
{
        pt->qproc = qproc;
}




struct poll_wqueues {
        poll_table pt;
        struct poll_table_page * table;
        int error;
};

extern void poll_initwait(struct poll_wqueues *pwq);
extern void poll_freewait(struct poll_wqueues *pwq);





typedef struct {
        unsigned long *in, *out, *ex;
        unsigned long *res_in, *res_out, *res_ex;
} fd_set_bits;
static 
int get_fd_set(unsigned long nr, void *ufdset, unsigned long *fdset)
{
        nr = ((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long));
        if (ufdset)
                return copy_from_user(fdset, ufdset, nr) ? -14 : 0;

        (__builtin_constant_p(0) ? (__builtin_constant_p((nr)) ? __constant_c_and_count_memset(((fdset)),((0x01010101UL*(unsigned char)(0))),((nr))) : __constant_c_memset(((fdset)),((0x01010101UL*(unsigned char)(0))),((nr)))) : (__builtin_constant_p((nr)) ? __memset_generic((((fdset))),(((0))),(((nr)))) : __memset_generic(((fdset)),((0)),((nr)))));
        return 0;
}

static unsigned long
set_fd_set(unsigned long nr, void *ufdset, unsigned long *fdset)
{
        if (ufdset)
                return __copy_to_user(ufdset, fdset, ((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)));
        return 0;
}

static 
void zero_fd_set(unsigned long nr, unsigned long *fdset)
{
        (__builtin_constant_p(0) ? (__builtin_constant_p((((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)))) ? __constant_c_and_count_memset(((fdset)),((0x01010101UL*(unsigned char)(0))),((((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long))))) : __constant_c_memset(((fdset)),((0x01010101UL*(unsigned char)(0))),((((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)))))) : (__builtin_constant_p((((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)))) ? __memset_generic((((fdset))),(((0))),(((((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)))))) : __memset_generic(((fdset)),((0)),((((((nr)+(8*sizeof(long))-1)/(8*sizeof(long)))*sizeof(long)))))));
}

extern int do_select(int n, fd_set_bits *fds, long *timeout);
struct poll_table_struct;
struct inode;
typedef enum {
        SS_FREE = 0,
        SS_UNCONNECTED,
        SS_CONNECTING,
        SS_CONNECTED,
        SS_DISCONNECTING
} socket_state;
enum sock_type {
        SOCK_STREAM = 1,
        SOCK_DGRAM = 2,
        SOCK_RAW = 3,
        SOCK_RDM = 4,
        SOCK_SEQPACKET = 5,
        SOCK_DCCP = 6,
        SOCK_PACKET = 10,
};
struct socket {
        socket_state state;
        unsigned long flags;
        struct proto_ops *ops;
        struct fasync_struct *fasync_list;
        struct file *file;
        struct sock *sk;
        wait_queue_head_t wait;
        short type;
};

struct vm_area_struct;
struct page;
struct kiocb;
struct sockaddr;
struct msghdr;
struct module;

struct proto_ops {
        int family;
        struct module *owner;
        int (*release) (struct socket *sock);
        int (*bind) (struct socket *sock,
                                      struct sockaddr *myaddr,
                                      int sockaddr_len);
        int (*connect) (struct socket *sock,
                                      struct sockaddr *vaddr,
                                      int sockaddr_len, int flags);
        int (*socketpair)(struct socket *sock1,
                                      struct socket *sock2);
        int (*accept) (struct socket *sock,
                                      struct socket *newsock, int flags);
        int (*getname) (struct socket *sock,
                                      struct sockaddr *addr,
                                      int *sockaddr_len, int peer);
        unsigned int (*poll) (struct file *file, struct socket *sock,
                                      struct poll_table_struct *wait);
        int (*ioctl) (struct socket *sock, unsigned int cmd,
                                      unsigned long arg);
        int (*listen) (struct socket *sock, int len);
        int (*shutdown) (struct socket *sock, int flags);
        int (*setsockopt)(struct socket *sock, int level,
                                      int optname, char *optval, int optlen);
        int (*getsockopt)(struct socket *sock, int level,
                                      int optname, char *optval, int *optlen);
        int (*sendmsg) (struct kiocb *iocb, struct socket *sock,
                                      struct msghdr *m, size_t total_len);
        int (*recvmsg) (struct kiocb *iocb, struct socket *sock,
                                      struct msghdr *m, size_t total_len,
                                      int flags);
        int (*mmap) (struct file *file, struct socket *sock,
                                      struct vm_area_struct * vma);
        ssize_t (*sendpage) (struct socket *sock, struct page *page,
                                      int offset, size_t size, int flags);
};

struct net_proto_family {
        int family;
        int (*create)(struct socket *sock, int protocol);


        short authentication;
        short encryption;
        short encrypt_net;
        struct module *owner;
};

struct iovec;
struct kvec;

extern int sock_wake_async(struct socket *sk, int how, int band);
extern int sock_register(struct net_proto_family *fam);
extern int sock_unregister(int family);
extern int sock_create(int family, int type, int proto,
                                 struct socket **res);
extern int sock_create_kern(int family, int type, int proto,
                                      struct socket **res);
extern int sock_create_lite(int family, int type, int proto,
                                      struct socket **res);
extern void sock_release(struct socket *sock);
extern int sock_sendmsg(struct socket *sock, struct msghdr *msg,
                                  size_t len);
extern int sock_recvmsg(struct socket *sock, struct msghdr *msg,
                                  size_t size, int flags);
extern int sock_map_fd(struct socket *sock);
extern struct socket *sockfd_lookup(int fd, int *err);

extern int net_ratelimit(void);
extern unsigned long net_random(void);
extern void net_srandom(unsigned long);
extern void net_random_init(void);

extern int kernel_sendmsg(struct socket *sock, struct msghdr *msg,
                                    struct kvec *vec, size_t num, size_t len);
extern int kernel_recvmsg(struct socket *sock, struct msghdr *msg,
                                    struct kvec *vec, size_t num,
                                    size_t len, int flags);
struct file;






struct __sysctl_args {
        int *name;
        int nlen;
        void *oldval;
        size_t *oldlenp;
        void *newval;
        size_t newlen;
        unsigned long __unused[4];
};
enum
{
        CTL_KERN=1,
        CTL_VM=2,
        CTL_NET=3,
        CTL_PROC=4,
        CTL_FS=5,
        CTL_DEBUG=6,
        CTL_DEV=7,
        CTL_BUS=8,
        CTL_ABI=9,
        CTL_CPU=10
};


enum
{
        CTL_BUS_ISA=1
};


enum
{
        INOTIFY_MAX_USER_INSTANCES=1,
        INOTIFY_MAX_USER_WATCHES=2,
        INOTIFY_MAX_QUEUED_EVENTS=3
};


enum
{
        KERN_OSTYPE=1,
        KERN_OSRELEASE=2,
        KERN_OSREV=3,
        KERN_VERSION=4,
        KERN_SECUREMASK=5,
        KERN_PROF=6,
        KERN_NODENAME=7,
        KERN_DOMAINNAME=8,

        KERN_CAP_BSET=14,
        KERN_PANIC=15,
        KERN_REALROOTDEV=16,

        KERN_SPARC_REBOOT=21,
        KERN_CTLALTDEL=22,
        KERN_PRINTK=23,
        KERN_NAMETRANS=24,
        KERN_PPC_HTABRECLAIM=25,
        KERN_PPC_ZEROPAGED=26,
        KERN_PPC_POWERSAVE_NAP=27,
        KERN_MODPROBE=28,
        KERN_SG_BIG_BUFF=29,
        KERN_ACCT=30,
        KERN_PPC_L2CR=31,

        KERN_RTSIGNR=32,
        KERN_RTSIGMAX=33,

        KERN_SHMMAX=34,
        KERN_MSGMAX=35,
        KERN_MSGMNB=36,
        KERN_MSGPOOL=37,
        KERN_SYSRQ=38,
        KERN_MAX_THREADS=39,
        KERN_RANDOM=40,
        KERN_SHMALL=41,
        KERN_MSGMNI=42,
        KERN_SEM=43,
        KERN_SPARC_STOP_A=44,
        KERN_SHMMNI=45,
        KERN_OVERFLOWUID=46,
        KERN_OVERFLOWGID=47,
        KERN_SHMPATH=48,
        KERN_HOTPLUG=49,
        KERN_IEEE_EMULATION_WARNINGS=50,
        KERN_S390_USER_DEBUG_LOGGING=51,
        KERN_CORE_USES_PID=52,
        KERN_TAINTED=53,
        KERN_CADPID=54,
        KERN_PIDMAX=55,
        KERN_CORE_PATTERN=56,
        KERN_PANIC_ON_OOPS=57,
        KERN_HPPA_PWRSW=58,
        KERN_HPPA_UNALIGNED=59,
        KERN_PRINTK_RATELIMIT=60,
        KERN_PRINTK_RATELIMIT_BURST=61,
        KERN_PTY=62,
        KERN_NGROUPS_MAX=63,
        KERN_SPARC_SCONS_PWROFF=64,
        KERN_HZ_TIMER=65,
        KERN_UNKNOWN_NMI_PANIC=66,
        KERN_BOOTLOADER_TYPE=67,
        KERN_RANDOMIZE=68,
        KERN_SETUID_DUMPABLE=69,
        KERN_SPIN_RETRY=70,
};



enum
{
        VM_UNUSED1=1,
        VM_UNUSED2=2,
        VM_UNUSED3=3,
        VM_UNUSED4=4,
        VM_OVERCOMMIT_MEMORY=5,
        VM_UNUSED5=6,
        VM_UNUSED7=7,
        VM_UNUSED8=8,
        VM_UNUSED9=9,
        VM_PAGE_CLUSTER=10,
        VM_DIRTY_BACKGROUND=11,
        VM_DIRTY_RATIO=12,
        VM_DIRTY_WB_CS=13,
        VM_DIRTY_EXPIRE_CS=14,
        VM_NR_PDFLUSH_THREADS=15,
        VM_OVERCOMMIT_RATIO=16,
        VM_PAGEBUF=17,
        VM_HUGETLB_PAGES=18,
        VM_SWAPPINESS=19,
        VM_LOWMEM_RESERVE_RATIO=20,
        VM_MIN_FREE_KBYTES=21,
        VM_MAX_MAP_COUNT=22,
        VM_LAPTOP_MODE=23,
        VM_BLOCK_DUMP=24,
        VM_HUGETLB_GROUP=25,
        VM_VFS_CACHE_PRESSURE=26,
        VM_LEGACY_VA_LAYOUT=27,
        VM_SWAP_TOKEN_TIMEOUT=28,
};



enum
{
        NET_CORE=1,
        NET_ETHER=2,
        NET_802=3,
        NET_UNIX=4,
        NET_IPV4=5,
        NET_IPX=6,
        NET_ATALK=7,
        NET_NETROM=8,
        NET_AX25=9,
        NET_BRIDGE=10,
        NET_ROSE=11,
        NET_IPV6=12,
        NET_X25=13,
        NET_TR=14,
        NET_DECNET=15,
        NET_ECONET=16,
        NET_SCTP=17,
        NET_LLC=18,
};


enum
{
        RANDOM_POOLSIZE=1,
        RANDOM_ENTROPY_COUNT=2,
        RANDOM_READ_THRESH=3,
        RANDOM_WRITE_THRESH=4,
        RANDOM_BOOT_ID=5,
        RANDOM_UUID=6
};


enum
{
        PTY_MAX=1,
        PTY_NR=2
};


enum
{
        BUS_ISA_MEM_BASE=1,
        BUS_ISA_PORT_BASE=2,
        BUS_ISA_PORT_SHIFT=3
};


enum
{
        NET_CORE_WMEM_MAX=1,
        NET_CORE_RMEM_MAX=2,
        NET_CORE_WMEM_DEFAULT=3,
        NET_CORE_RMEM_DEFAULT=4,

        NET_CORE_MAX_BACKLOG=6,
        NET_CORE_FASTROUTE=7,
        NET_CORE_MSG_COST=8,
        NET_CORE_MSG_BURST=9,
        NET_CORE_OPTMEM_MAX=10,
        NET_CORE_HOT_LIST_LENGTH=11,
        NET_CORE_DIVERT_VERSION=12,
        NET_CORE_NO_CONG_THRESH=13,
        NET_CORE_NO_CONG=14,
        NET_CORE_LO_CONG=15,
        NET_CORE_MOD_CONG=16,
        NET_CORE_DEV_WEIGHT=17,
        NET_CORE_SOMAXCONN=18,
        NET_CORE_BUDGET=19,
};







enum
{
        NET_UNIX_DESTROY_DELAY=1,
        NET_UNIX_DELETE_DELAY=2,
        NET_UNIX_MAX_DGRAM_QLEN=3,
};


enum
{

        NET_IPV4_FORWARD=8,
        NET_IPV4_DYNADDR=9,

        NET_IPV4_CONF=16,
        NET_IPV4_NEIGH=17,
        NET_IPV4_ROUTE=18,
        NET_IPV4_FIB_HASH=19,
        NET_IPV4_NETFILTER=20,

        NET_IPV4_TCP_TIMESTAMPS=33,
        NET_IPV4_TCP_WINDOW_SCALING=34,
        NET_IPV4_TCP_SACK=35,
        NET_IPV4_TCP_RETRANS_COLLAPSE=36,
        NET_IPV4_DEFAULT_TTL=37,
        NET_IPV4_AUTOCONFIG=38,
        NET_IPV4_NO_PMTU_DISC=39,
        NET_IPV4_TCP_SYN_RETRIES=40,
        NET_IPV4_IPFRAG_HIGH_THRESH=41,
        NET_IPV4_IPFRAG_LOW_THRESH=42,
        NET_IPV4_IPFRAG_TIME=43,
        NET_IPV4_TCP_MAX_KA_PROBES=44,
        NET_IPV4_TCP_KEEPALIVE_TIME=45,
        NET_IPV4_TCP_KEEPALIVE_PROBES=46,
        NET_IPV4_TCP_RETRIES1=47,
        NET_IPV4_TCP_RETRIES2=48,
        NET_IPV4_TCP_FIN_TIMEOUT=49,
        NET_IPV4_IP_MASQ_DEBUG=50,
        NET_TCP_SYNCOOKIES=51,
        NET_TCP_STDURG=52,
        NET_TCP_RFC1337=53,
        NET_TCP_SYN_TAILDROP=54,
        NET_TCP_MAX_SYN_BACKLOG=55,
        NET_IPV4_LOCAL_PORT_RANGE=56,
        NET_IPV4_ICMP_ECHO_IGNORE_ALL=57,
        NET_IPV4_ICMP_ECHO_IGNORE_BROADCASTS=58,
        NET_IPV4_ICMP_SOURCEQUENCH_RATE=59,
        NET_IPV4_ICMP_DESTUNREACH_RATE=60,
        NET_IPV4_ICMP_TIMEEXCEED_RATE=61,
        NET_IPV4_ICMP_PARAMPROB_RATE=62,
        NET_IPV4_ICMP_ECHOREPLY_RATE=63,
        NET_IPV4_ICMP_IGNORE_BOGUS_ERROR_RESPONSES=64,
        NET_IPV4_IGMP_MAX_MEMBERSHIPS=65,
        NET_TCP_TW_RECYCLE=66,
        NET_IPV4_ALWAYS_DEFRAG=67,
        NET_IPV4_TCP_KEEPALIVE_INTVL=68,
        NET_IPV4_INET_PEER_THRESHOLD=69,
        NET_IPV4_INET_PEER_MINTTL=70,
        NET_IPV4_INET_PEER_MAXTTL=71,
        NET_IPV4_INET_PEER_GC_MINTIME=72,
        NET_IPV4_INET_PEER_GC_MAXTIME=73,
        NET_TCP_ORPHAN_RETRIES=74,
        NET_TCP_ABORT_ON_OVERFLOW=75,
        NET_TCP_SYNACK_RETRIES=76,
        NET_TCP_MAX_ORPHANS=77,
        NET_TCP_MAX_TW_BUCKETS=78,
        NET_TCP_FACK=79,
        NET_TCP_REORDERING=80,
        NET_TCP_ECN=81,
        NET_TCP_DSACK=82,
        NET_TCP_MEM=83,
        NET_TCP_WMEM=84,
        NET_TCP_RMEM=85,
        NET_TCP_APP_WIN=86,
        NET_TCP_ADV_WIN_SCALE=87,
        NET_IPV4_NONLOCAL_BIND=88,
        NET_IPV4_ICMP_RATELIMIT=89,
        NET_IPV4_ICMP_RATEMASK=90,
        NET_TCP_TW_REUSE=91,
        NET_TCP_FRTO=92,
        NET_TCP_LOW_LATENCY=93,
        NET_IPV4_IPFRAG_SECRET_INTERVAL=94,
        NET_IPV4_IGMP_MAX_MSF=96,
        NET_TCP_NO_METRICS_SAVE=97,
        NET_TCP_DEFAULT_WIN_SCALE=105,
        NET_TCP_MODERATE_RCVBUF=106,
        NET_TCP_TSO_WIN_DIVISOR=107,
        NET_TCP_BIC_BETA=108,
        NET_IPV4_ICMP_ERRORS_USE_INBOUND_IFADDR=109,
        NET_TCP_CONG_CONTROL=110,
};

enum {
        NET_IPV4_ROUTE_FLUSH=1,
        NET_IPV4_ROUTE_MIN_DELAY=2,
        NET_IPV4_ROUTE_MAX_DELAY=3,
        NET_IPV4_ROUTE_GC_THRESH=4,
        NET_IPV4_ROUTE_MAX_SIZE=5,
        NET_IPV4_ROUTE_GC_MIN_INTERVAL=6,
        NET_IPV4_ROUTE_GC_TIMEOUT=7,
        NET_IPV4_ROUTE_GC_INTERVAL=8,
        NET_IPV4_ROUTE_REDIRECT_LOAD=9,
        NET_IPV4_ROUTE_REDIRECT_NUMBER=10,
        NET_IPV4_ROUTE_REDIRECT_SILENCE=11,
        NET_IPV4_ROUTE_ERROR_COST=12,
        NET_IPV4_ROUTE_ERROR_BURST=13,
        NET_IPV4_ROUTE_GC_ELASTICITY=14,
        NET_IPV4_ROUTE_MTU_EXPIRES=15,
        NET_IPV4_ROUTE_MIN_PMTU=16,
        NET_IPV4_ROUTE_MIN_ADVMSS=17,
        NET_IPV4_ROUTE_SECRET_INTERVAL=18,
        NET_IPV4_ROUTE_GC_MIN_INTERVAL_MS=19,
};

enum
{
        NET_PROTO_CONF_ALL=-2,
        NET_PROTO_CONF_DEFAULT=-3


};

enum
{
        NET_IPV4_CONF_FORWARDING=1,
        NET_IPV4_CONF_MC_FORWARDING=2,
        NET_IPV4_CONF_PROXY_ARP=3,
        NET_IPV4_CONF_ACCEPT_REDIRECTS=4,
        NET_IPV4_CONF_SECURE_REDIRECTS=5,
        NET_IPV4_CONF_SEND_REDIRECTS=6,
        NET_IPV4_CONF_SHARED_MEDIA=7,
        NET_IPV4_CONF_RP_FILTER=8,
        NET_IPV4_CONF_ACCEPT_SOURCE_ROUTE=9,
        NET_IPV4_CONF_BOOTP_RELAY=10,
        NET_IPV4_CONF_LOG_MARTIANS=11,
        NET_IPV4_CONF_TAG=12,
        NET_IPV4_CONF_ARPFILTER=13,
        NET_IPV4_CONF_MEDIUM_ID=14,
        NET_IPV4_CONF_NOXFRM=15,
        NET_IPV4_CONF_NOPOLICY=16,
        NET_IPV4_CONF_FORCE_IGMP_VERSION=17,
        NET_IPV4_CONF_ARP_ANNOUNCE=18,
        NET_IPV4_CONF_ARP_IGNORE=19,
        NET_IPV4_CONF_PROMOTE_SECONDARIES=20,
        __NET_IPV4_CONF_MAX
};


enum
{
        NET_IPV4_NF_CONNTRACK_MAX=1,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_SYN_SENT=2,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_SYN_RECV=3,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_ESTABLISHED=4,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_FIN_WAIT=5,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_CLOSE_WAIT=6,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_LAST_ACK=7,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_TIME_WAIT=8,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_CLOSE=9,
        NET_IPV4_NF_CONNTRACK_UDP_TIMEOUT=10,
        NET_IPV4_NF_CONNTRACK_UDP_TIMEOUT_STREAM=11,
        NET_IPV4_NF_CONNTRACK_ICMP_TIMEOUT=12,
        NET_IPV4_NF_CONNTRACK_GENERIC_TIMEOUT=13,
        NET_IPV4_NF_CONNTRACK_BUCKETS=14,
        NET_IPV4_NF_CONNTRACK_LOG_INVALID=15,
        NET_IPV4_NF_CONNTRACK_TCP_TIMEOUT_MAX_RETRANS=16,
        NET_IPV4_NF_CONNTRACK_TCP_LOOSE=17,
        NET_IPV4_NF_CONNTRACK_TCP_BE_LIBERAL=18,
        NET_IPV4_NF_CONNTRACK_TCP_MAX_RETRANS=19,
        NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_CLOSED=20,
        NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_COOKIE_WAIT=21,
        NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_COOKIE_ECHOED=22,
        NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_ESTABLISHED=23,
        NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_SENT=24,
        NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_RECD=25,
        NET_IPV4_NF_CONNTRACK_SCTP_TIMEOUT_SHUTDOWN_ACK_SENT=26,
        NET_IPV4_NF_CONNTRACK_COUNT=27,
};


enum {
        NET_IPV6_CONF=16,
        NET_IPV6_NEIGH=17,
        NET_IPV6_ROUTE=18,
        NET_IPV6_ICMP=19,
        NET_IPV6_BINDV6ONLY=20,
        NET_IPV6_IP6FRAG_HIGH_THRESH=21,
        NET_IPV6_IP6FRAG_LOW_THRESH=22,
        NET_IPV6_IP6FRAG_TIME=23,
        NET_IPV6_IP6FRAG_SECRET_INTERVAL=24,
        NET_IPV6_MLD_MAX_MSF=25,
};

enum {
        NET_IPV6_ROUTE_FLUSH=1,
        NET_IPV6_ROUTE_GC_THRESH=2,
        NET_IPV6_ROUTE_MAX_SIZE=3,
        NET_IPV6_ROUTE_GC_MIN_INTERVAL=4,
        NET_IPV6_ROUTE_GC_TIMEOUT=5,
        NET_IPV6_ROUTE_GC_INTERVAL=6,
        NET_IPV6_ROUTE_GC_ELASTICITY=7,
        NET_IPV6_ROUTE_MTU_EXPIRES=8,
        NET_IPV6_ROUTE_MIN_ADVMSS=9,
        NET_IPV6_ROUTE_GC_MIN_INTERVAL_MS=10
};

enum {
        NET_IPV6_FORWARDING=1,
        NET_IPV6_HOP_LIMIT=2,
        NET_IPV6_MTU=3,
        NET_IPV6_ACCEPT_RA=4,
        NET_IPV6_ACCEPT_REDIRECTS=5,
        NET_IPV6_AUTOCONF=6,
        NET_IPV6_DAD_TRANSMITS=7,
        NET_IPV6_RTR_SOLICITS=8,
        NET_IPV6_RTR_SOLICIT_INTERVAL=9,
        NET_IPV6_RTR_SOLICIT_DELAY=10,
        NET_IPV6_USE_TEMPADDR=11,
        NET_IPV6_TEMP_VALID_LFT=12,
        NET_IPV6_TEMP_PREFERED_LFT=13,
        NET_IPV6_REGEN_MAX_RETRY=14,
        NET_IPV6_MAX_DESYNC_FACTOR=15,
        NET_IPV6_MAX_ADDRESSES=16,
        NET_IPV6_FORCE_MLD_VERSION=17,
        __NET_IPV6_MAX
};


enum {
        NET_IPV6_ICMP_RATELIMIT=1
};


enum {
        NET_NEIGH_MCAST_SOLICIT=1,
        NET_NEIGH_UCAST_SOLICIT=2,
        NET_NEIGH_APP_SOLICIT=3,
        NET_NEIGH_RETRANS_TIME=4,
        NET_NEIGH_REACHABLE_TIME=5,
        NET_NEIGH_DELAY_PROBE_TIME=6,
        NET_NEIGH_GC_STALE_TIME=7,
        NET_NEIGH_UNRES_QLEN=8,
        NET_NEIGH_PROXY_QLEN=9,
        NET_NEIGH_ANYCAST_DELAY=10,
        NET_NEIGH_PROXY_DELAY=11,
        NET_NEIGH_LOCKTIME=12,
        NET_NEIGH_GC_INTERVAL=13,
        NET_NEIGH_GC_THRESH1=14,
        NET_NEIGH_GC_THRESH2=15,
        NET_NEIGH_GC_THRESH3=16,
        NET_NEIGH_RETRANS_TIME_MS=17,
        NET_NEIGH_REACHABLE_TIME_MS=18,
        __NET_NEIGH_MAX
};


enum {
        NET_IPX_PPROP_BROADCASTING=1,
        NET_IPX_FORWARDING=2
};


enum {
        NET_LLC2=1,
        NET_LLC_STATION=2,
};


enum {
        NET_LLC2_TIMEOUT=1,
};


enum {
        NET_LLC_STATION_ACK_TIMEOUT=1,
};


enum {
        NET_LLC2_ACK_TIMEOUT=1,
        NET_LLC2_P_TIMEOUT=2,
        NET_LLC2_REJ_TIMEOUT=3,
        NET_LLC2_BUSY_TIMEOUT=4,
};


enum {
        NET_ATALK_AARP_EXPIRY_TIME=1,
        NET_ATALK_AARP_TICK_TIME=2,
        NET_ATALK_AARP_RETRANSMIT_LIMIT=3,
        NET_ATALK_AARP_RESOLVE_TIME=4
};



enum {
        NET_NETROM_DEFAULT_PATH_QUALITY=1,
        NET_NETROM_OBSOLESCENCE_COUNT_INITIALISER=2,
        NET_NETROM_NETWORK_TTL_INITIALISER=3,
        NET_NETROM_TRANSPORT_TIMEOUT=4,
        NET_NETROM_TRANSPORT_MAXIMUM_TRIES=5,
        NET_NETROM_TRANSPORT_ACKNOWLEDGE_DELAY=6,
        NET_NETROM_TRANSPORT_BUSY_DELAY=7,
        NET_NETROM_TRANSPORT_REQUESTED_WINDOW_SIZE=8,
        NET_NETROM_TRANSPORT_NO_ACTIVITY_TIMEOUT=9,
        NET_NETROM_ROUTING_CONTROL=10,
        NET_NETROM_LINK_FAILS_COUNT=11,
        NET_NETROM_RESET=12
};


enum {
        NET_AX25_IP_DEFAULT_MODE=1,
        NET_AX25_DEFAULT_MODE=2,
        NET_AX25_BACKOFF_TYPE=3,
        NET_AX25_CONNECT_MODE=4,
        NET_AX25_STANDARD_WINDOW=5,
        NET_AX25_EXTENDED_WINDOW=6,
        NET_AX25_T1_TIMEOUT=7,
        NET_AX25_T2_TIMEOUT=8,
        NET_AX25_T3_TIMEOUT=9,
        NET_AX25_IDLE_TIMEOUT=10,
        NET_AX25_N2=11,
        NET_AX25_PACLEN=12,
        NET_AX25_PROTOCOL=13,
        NET_AX25_DAMA_SLAVE_TIMEOUT=14
};


enum {
        NET_ROSE_RESTART_REQUEST_TIMEOUT=1,
        NET_ROSE_CALL_REQUEST_TIMEOUT=2,
        NET_ROSE_RESET_REQUEST_TIMEOUT=3,
        NET_ROSE_CLEAR_REQUEST_TIMEOUT=4,
        NET_ROSE_ACK_HOLD_BACK_TIMEOUT=5,
        NET_ROSE_ROUTING_CONTROL=6,
        NET_ROSE_LINK_FAIL_TIMEOUT=7,
        NET_ROSE_MAX_VCS=8,
        NET_ROSE_WINDOW_SIZE=9,
        NET_ROSE_NO_ACTIVITY_TIMEOUT=10
};


enum {
        NET_X25_RESTART_REQUEST_TIMEOUT=1,
        NET_X25_CALL_REQUEST_TIMEOUT=2,
        NET_X25_RESET_REQUEST_TIMEOUT=3,
        NET_X25_CLEAR_REQUEST_TIMEOUT=4,
        NET_X25_ACK_HOLD_BACK_TIMEOUT=5
};


enum
{
        NET_TR_RIF_TIMEOUT=1
};


enum {
        NET_DECNET_NODE_TYPE = 1,
        NET_DECNET_NODE_ADDRESS = 2,
        NET_DECNET_NODE_NAME = 3,
        NET_DECNET_DEFAULT_DEVICE = 4,
        NET_DECNET_TIME_WAIT = 5,
        NET_DECNET_DN_COUNT = 6,
        NET_DECNET_DI_COUNT = 7,
        NET_DECNET_DR_COUNT = 8,
        NET_DECNET_DST_GC_INTERVAL = 9,
        NET_DECNET_CONF = 10,
        NET_DECNET_NO_FC_MAX_CWND = 11,
        NET_DECNET_DEBUG_LEVEL = 255
};


enum {
        NET_DECNET_CONF_LOOPBACK = -2,
        NET_DECNET_CONF_DDCMP = -3,
        NET_DECNET_CONF_PPP = -4,
        NET_DECNET_CONF_X25 = -5,
        NET_DECNET_CONF_GRE = -6,
        NET_DECNET_CONF_ETHER = -7


};


enum {
        NET_DECNET_CONF_DEV_PRIORITY = 1,
        NET_DECNET_CONF_DEV_T1 = 2,
        NET_DECNET_CONF_DEV_T2 = 3,
        NET_DECNET_CONF_DEV_T3 = 4,
        NET_DECNET_CONF_DEV_FORWARDING = 5,
        NET_DECNET_CONF_DEV_BLKSIZE = 6,
        NET_DECNET_CONF_DEV_STATE = 7
};


enum {
        NET_SCTP_RTO_INITIAL = 1,
        NET_SCTP_RTO_MIN = 2,
        NET_SCTP_RTO_MAX = 3,
        NET_SCTP_RTO_ALPHA = 4,
        NET_SCTP_RTO_BETA = 5,
        NET_SCTP_VALID_COOKIE_LIFE = 6,
        NET_SCTP_ASSOCIATION_MAX_RETRANS = 7,
        NET_SCTP_PATH_MAX_RETRANS = 8,
        NET_SCTP_MAX_INIT_RETRANSMITS = 9,
        NET_SCTP_HB_INTERVAL = 10,
        NET_SCTP_PRESERVE_ENABLE = 11,
        NET_SCTP_MAX_BURST = 12,
        NET_SCTP_ADDIP_ENABLE = 13,
        NET_SCTP_PRSCTP_ENABLE = 14,
        NET_SCTP_SNDBUF_POLICY = 15,
        NET_SCTP_SACK_TIMEOUT = 16,
};


enum {
        NET_BRIDGE_NF_CALL_ARPTABLES = 1,
        NET_BRIDGE_NF_CALL_IPTABLES = 2,
        NET_BRIDGE_NF_CALL_IP6TABLES = 3,
        NET_BRIDGE_NF_FILTER_VLAN_TAGGED = 4,
};




enum
{
        FS_NRINODE=1,
        FS_STATINODE=2,
        FS_MAXINODE=3,
        FS_NRDQUOT=4,
        FS_MAXDQUOT=5,
        FS_NRFILE=6,
        FS_MAXFILE=7,
        FS_DENTRY=8,
        FS_NRSUPER=9,
        FS_MAXSUPER=10,
        FS_OVERFLOWUID=11,
        FS_OVERFLOWGID=12,
        FS_LEASES=13,
        FS_DIR_NOTIFY=14,
        FS_LEASE_TIME=15,
        FS_DQSTATS=16,
        FS_XFS=17,
        FS_AIO_NR=18,
        FS_AIO_MAX_NR=19,
        FS_INOTIFY=20,
};


enum {
        FS_DQ_LOOKUPS = 1,
        FS_DQ_DROPS = 2,
        FS_DQ_READS = 3,
        FS_DQ_WRITES = 4,
        FS_DQ_CACHE_HITS = 5,
        FS_DQ_ALLOCATED = 6,
        FS_DQ_FREE = 7,
        FS_DQ_SYNCS = 8,
        FS_DQ_WARNINGS = 9,
};




enum {
        DEV_CDROM=1,
        DEV_HWMON=2,
        DEV_PARPORT=3,
        DEV_RAID=4,
        DEV_MAC_HID=5,
        DEV_SCSI=6,
        DEV_IPMI=7,
};


enum {
        DEV_CDROM_INFO=1,
        DEV_CDROM_AUTOCLOSE=2,
        DEV_CDROM_AUTOEJECT=3,
        DEV_CDROM_DEBUG=4,
        DEV_CDROM_LOCK=5,
        DEV_CDROM_CHECK_MEDIA=6
};


enum {
        DEV_PARPORT_DEFAULT=-3
};


enum {
        DEV_RAID_SPEED_LIMIT_MIN=1,
        DEV_RAID_SPEED_LIMIT_MAX=2
};


enum {
        DEV_PARPORT_DEFAULT_TIMESLICE=1,
        DEV_PARPORT_DEFAULT_SPINTIME=2
};


enum {
        DEV_PARPORT_SPINTIME=1,
        DEV_PARPORT_BASE_ADDR=2,
        DEV_PARPORT_IRQ=3,
        DEV_PARPORT_DMA=4,
        DEV_PARPORT_MODES=5,
        DEV_PARPORT_DEVICES=6,
        DEV_PARPORT_AUTOPROBE=16
};


enum {
        DEV_PARPORT_DEVICES_ACTIVE=-3,
};


enum {
        DEV_PARPORT_DEVICE_TIMESLICE=1,
};


enum {
        DEV_MAC_HID_KEYBOARD_SENDS_LINUX_KEYCODES=1,
        DEV_MAC_HID_KEYBOARD_LOCK_KEYCODES=2,
        DEV_MAC_HID_MOUSE_BUTTON_EMULATION=3,
        DEV_MAC_HID_MOUSE_BUTTON2_KEYCODE=4,
        DEV_MAC_HID_MOUSE_BUTTON3_KEYCODE=5,
        DEV_MAC_HID_ADB_MOUSE_SENDS_KEYCODES=6
};


enum {
        DEV_SCSI_LOGGING_LEVEL=1,
};


enum {
        DEV_IPMI_POWEROFF_POWERCYCLE=1,
};


enum
{
        ABI_DEFHANDLER_COFF=1,
        ABI_DEFHANDLER_ELF=2,
        ABI_DEFHANDLER_LCALL7=3,
        ABI_DEFHANDLER_LIBCSO=4,
        ABI_TRACE=5,
        ABI_FAKE_UTSNAME=6,
};



extern void sysctl_init(void);

typedef struct ctl_table ctl_table;

typedef int ctl_handler (ctl_table *table, int *name, int nlen,
                         void *oldval, size_t *oldlenp,
                         void *newval, size_t newlen,
                         void **context);

typedef int proc_handler (ctl_table *ctl, int write, struct file * filp,
                          void *buffer, size_t *lenp, loff_t *ppos);

extern int proc_dostring(ctl_table *, int, struct file *,
                         void *, size_t *, loff_t *);
extern int proc_dointvec(ctl_table *, int, struct file *,
                         void *, size_t *, loff_t *);
extern int proc_dointvec_bset(ctl_table *, int, struct file *,
                              void *, size_t *, loff_t *);
extern int proc_dointvec_minmax(ctl_table *, int, struct file *,
                                void *, size_t *, loff_t *);
extern int proc_dointvec_jiffies(ctl_table *, int, struct file *,
                                 void *, size_t *, loff_t *);
extern int proc_dointvec_userhz_jiffies(ctl_table *, int, struct file *,
                                        void *, size_t *, loff_t *);
extern int proc_dointvec_ms_jiffies(ctl_table *, int, struct file *,
                                    void *, size_t *, loff_t *);
extern int proc_doulongvec_minmax(ctl_table *, int, struct file *,
                                  void *, size_t *, loff_t *);
extern int proc_doulongvec_ms_jiffies_minmax(ctl_table *table, int,
                                      struct file *, void *, size_t *, loff_t *);

extern int do_sysctl (int *name, int nlen,
                      void *oldval, size_t *oldlenp,
                      void *newval, size_t newlen);

extern int do_sysctl_strategy (ctl_table *table,
                               int *name, int nlen,
                               void *oldval, size_t *oldlenp,
                               void *newval, size_t newlen, void ** context);

extern ctl_handler sysctl_string;
extern ctl_handler sysctl_intvec;
extern ctl_handler sysctl_jiffies;
extern ctl_handler sysctl_ms_jiffies;
struct ctl_table
{
        int ctl_name;
        const char *procname;
        void *data;
        int maxlen;
        mode_t mode;
        ctl_table *child;
        proc_handler *proc_handler;
        ctl_handler *strategy;
        struct proc_dir_entry *de;
        void *extra1;
        void *extra2;
};



struct ctl_table_header
{
        ctl_table *ctl_table;
        struct list_head ctl_entry;
};

struct ctl_table_header * register_sysctl_table(ctl_table * table,
                                                int insert_at_head);
void unregister_sysctl_table(struct ctl_table_header * table);
extern ctl_table net_table[];
extern int net_msg_cost;
extern int net_msg_burst;
struct ts_config;
struct ts_state
{
        unsigned int offset;
        char cb[40];
};
struct ts_ops
{
        const char *name;
        struct ts_config * (*init)(const void *, unsigned int, int);
        unsigned int (*find)(struct ts_config *,
                                        struct ts_state *);
        void (*destroy)(struct ts_config *);
        void * (*get_pattern)(struct ts_config *);
        unsigned int (*get_pattern_len)(struct ts_config *);
        struct module *owner;
        struct list_head list;
};







struct ts_config
{
        struct ts_ops *ops;
        unsigned int (*get_next_block)(unsigned int consumed,
                                                  const u8 **dst,
                                                  struct ts_config *conf,
                                                  struct ts_state *state);
        void (*finish)(struct ts_config *conf,
                                          struct ts_state *state);
};
static unsigned int textsearch_next(struct ts_config *conf,
                                           struct ts_state *state)
{
        unsigned int ret = conf->ops->find(conf, state);

        if (conf->finish)
                conf->finish(conf, state);

        return ret;
}
static unsigned int textsearch_find(struct ts_config *conf,
                                           struct ts_state *state)
{
        state->offset = 0;
        return textsearch_next(conf, state);
}





static void *textsearch_get_pattern(struct ts_config *conf)
{
        return conf->ops->get_pattern(conf);
}





static unsigned int textsearch_get_pattern_len(struct ts_config *conf)
{
        return conf->ops->get_pattern_len(conf);
}

extern int textsearch_register(struct ts_ops *);
extern int textsearch_unregister(struct ts_ops *);
extern struct ts_config *textsearch_prepare(const char *, const void *,
                                            unsigned int, int, int);
extern void textsearch_destroy(struct ts_config *conf);
extern unsigned int textsearch_find_continuous(struct ts_config *,
                                               struct ts_state *,
                                               const void *, unsigned int);





static struct ts_config *alloc_ts_config(size_t payload,
                                                gfp_t gfp_mask)
{
        struct ts_config *conf;

        conf = kmalloc((((sizeof(*conf)) + 8 -1) & ~(8 -1)) + payload, gfp_mask);
        if (conf == ((void *)0))
                return ERR_PTR(-12);

        (__builtin_constant_p(0) ? (__builtin_constant_p(((((sizeof(*conf)) + 8 -1) & ~(8 -1)) + payload)) ? __constant_c_and_count_memset(((conf)),((0x01010101UL*(unsigned char)(0))),(((((sizeof(*conf)) + 8 -1) & ~(8 -1)) + payload))) : __constant_c_memset(((conf)),((0x01010101UL*(unsigned char)(0))),(((((sizeof(*conf)) + 8 -1) & ~(8 -1)) + payload)))) : (__builtin_constant_p(((((sizeof(*conf)) + 8 -1) & ~(8 -1)) + payload)) ? __memset_generic((((conf))),(((0))),((((((sizeof(*conf)) + 8 -1) & ~(8 -1)) + payload)))) : __memset_generic(((conf)),((0)),(((((sizeof(*conf)) + 8 -1) & ~(8 -1)) + payload)))));
        return conf;
}

static void *ts_config_priv(struct ts_config *conf)
{
        return ((u8 *) conf + (((sizeof(struct ts_config)) + 8 -1) & ~(8 -1)));
}



struct in6_addr
{
        union
        {
                __u8 u6_addr8[16];
                __u16 u6_addr16[8];
                __u32 u6_addr32[4];
        } in6_u;



};
extern const struct in6_addr in6addr_loopback;


struct sockaddr_in6 {
        unsigned short int sin6_family;
        __u16 sin6_port;
        __u32 sin6_flowinfo;
        struct in6_addr sin6_addr;
        __u32 sin6_scope_id;
};

struct ipv6_mreq {

        struct in6_addr ipv6mr_multiaddr;


        int ipv6mr_ifindex;
};



struct in6_flowlabel_req
{
        struct in6_addr flr_dst;
        __u32 flr_label;
        __u8 flr_action;
        __u8 flr_share;
        __u16 flr_flags;
        __u16 flr_expires;
        __u16 flr_linger;
        __u32 __flr_pad;

};
 __attribute__((regparm(0))) unsigned int csum_partial(const unsigned char * buff, int len, unsigned int sum);
 __attribute__((regparm(0))) unsigned int csum_partial_copy_generic(const unsigned char *src, unsigned char *dst,
                                                  int len, int sum, int *src_err_ptr, int *dst_err_ptr);
static 
unsigned int csum_partial_copy_nocheck (const unsigned char *src, unsigned char *dst,
                                        int len, int sum)
{
        return csum_partial_copy_generic ( src, dst, len, sum, ((void *)0), ((void *)0));
}

static 
unsigned int csum_partial_copy_from_user(const unsigned char *src, unsigned char *dst,
                                                int len, int sum, int *err_ptr)
{
        do { do { } while (0); } while (0);
        return csum_partial_copy_generic(( unsigned char *)src, dst,
                                        len, sum, err_ptr, ((void *)0));
}
static unsigned short ip_fast_csum(unsigned char * iph,
                                          unsigned int ihl)
{
        unsigned int sum;

        __asm__ __volatile__(
            "movl (%1), %0	;\n"
            "subl $4, %2	;\n"
            "jbe 2f		;\n"
            "addl 4(%1), %0	;\n"
            "adcl 8(%1), %0	;\n"
            "adcl 12(%1), %0	;\n"
"1:	    adcl 16(%1), %0	;\n"
            "lea 4(%1), %1	;\n"
            "decl %2		;\n"
            "jne 1b		;\n"
            "adcl $0, %0	;\n"
            "movl %0, %2	;\n"
            "shrl $16, %0	;\n"
            "addw %w2, %w0	;\n"
            "adcl $0, %0	;\n"
            "notl %0		;\n"
"2:				;\n"



        : "=r" (sum), "=r" (iph), "=r" (ihl)
        : "1" (iph), "2" (ihl)
        : "memory");
        return(sum);
}





static unsigned int csum_fold(unsigned int sum)
{
        __asm__(
                "addl %1, %0		;\n"
                "adcl $0xffff, %0	;\n"
                : "=r" (sum)
                : "r" (sum << 16), "0" (sum & 0xffff0000)
        );
        return (~sum) >> 16;
}

static unsigned long csum_tcpudp_nofold(unsigned long saddr,
                                                   unsigned long daddr,
                                                   unsigned short len,
                                                   unsigned short proto,
                                                   unsigned int sum)
{
    __asm__(
        "addl %1, %0	;\n"
        "adcl %2, %0	;\n"
        "adcl %3, %0	;\n"
        "adcl $0, %0	;\n"
        : "=r" (sum)
        : "g" (daddr), "g"(saddr), "g"((ntohs(len)<<16)+proto*256), "0"(sum));
    return sum;
}





static unsigned short int csum_tcpudp_magic(unsigned long saddr,
                                                   unsigned long daddr,
                                                   unsigned short len,
                                                   unsigned short proto,
                                                   unsigned int sum)
{
        return csum_fold(csum_tcpudp_nofold(saddr,daddr,len,proto,sum));
}






static unsigned short ip_compute_csum(unsigned char * buff, int len)
{
    return csum_fold (csum_partial(buff, len, 0));
}


static unsigned short int csum_ipv6_magic(struct in6_addr *saddr,
                                                     struct in6_addr *daddr,
                                                     __u32 len,
                                                     unsigned short proto,
                                                     unsigned int sum)
{
        __asm__(
                "addl 0(%1), %0		;\n"
                "adcl 4(%1), %0		;\n"
                "adcl 8(%1), %0		;\n"
                "adcl 12(%1), %0	;\n"
                "adcl 0(%2), %0		;\n"
                "adcl 4(%2), %0		;\n"
                "adcl 8(%2), %0		;\n"
                "adcl 12(%2), %0	;\n"
                "adcl %3, %0		;\n"
                "adcl %4, %0		;\n"
                "adcl $0, %0		;\n"
                : "=&r" (sum)
                : "r" (saddr), "r" (daddr),
                  "r"(htonl(len)), "r"(htonl(proto)), "0"(sum));

        return csum_fold(sum);
}





static unsigned int csum_and_copy_to_user(const unsigned char *src,
                                                     unsigned char *dst,
                                                     int len, int sum,
                                                     int *err_ptr)
{
        do { do { } while (0); } while (0);
        if ((__builtin_expect(!!(({ unsigned long flag,sum; (void)0; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (dst),"g" ((int)(len)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0), 1)))
                return csum_partial_copy_generic(src, ( unsigned char *)dst, len, sum, ((void *)0), err_ptr);

        if (len)
                *err_ptr = -14;

        return -1;
}


static 
unsigned int csum_and_copy_from_user (const unsigned char *src, unsigned char *dst,
                                      int len, int sum, int *err_ptr)
{
        if ((__builtin_expect(!!(({ unsigned long flag,sum; (void)0; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (src),"g" ((int)(len)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0), 1)))
                return csum_partial_copy_from_user(src, dst, len, sum, err_ptr);

        if (len)
                *err_ptr = -14;

        return sum;
}
static unsigned int csum_add(unsigned int csum, unsigned int addend)
{
        csum += addend;
        return csum + (csum < addend);
}

static unsigned int csum_sub(unsigned int csum, unsigned int addend)
{
        return csum_add(csum, ~addend);
}

static unsigned int
csum_block_add(unsigned int csum, unsigned int csum2, int offset)
{
        if (offset&1)
                csum2 = ((csum2&0xFF00FF)<<8)+((csum2>>8)&0xFF00FF);
        return csum_add(csum, csum2);
}

static unsigned int
csum_block_sub(unsigned int csum, unsigned int csum2, int offset)
{
        if (offset&1)
                csum2 = ((csum2&0xFF00FF)<<8)+((csum2>>8)&0xFF00FF);
        return csum_sub(csum, csum2);
}
struct net_device;


struct nf_conntrack {
        atomic_t use;
        void (*destroy)(struct nf_conntrack *);
};


struct nf_bridge_info {
        atomic_t use;
        struct net_device *physindev;
        struct net_device *physoutdev;

        struct net_device *netoutdev;

        unsigned int mask;
        unsigned long data[32 / sizeof(unsigned long)];
};




struct sk_buff_head {

        struct sk_buff *next;
        struct sk_buff *prev;

        __u32 qlen;
        spinlock_t lock;
};

struct sk_buff;




typedef struct skb_frag_struct skb_frag_t;

struct skb_frag_struct {
        struct page *page;
        __u16 page_offset;
        __u16 size;
};




struct skb_shared_info {
        atomic_t dataref;
        unsigned int nr_frags;
        unsigned short tso_size;
        unsigned short tso_segs;
        struct sk_buff *frag_list;
        skb_frag_t frags[(65536/(1UL << 12) + 2)];
};
struct skb_timeval {
        u32 off_sec;
        u32 off_usec;
};


enum {
        SKB_FCLONE_UNAVAILABLE,
        SKB_FCLONE_ORIG,
        SKB_FCLONE_CLONE,
};
struct sk_buff {

        struct sk_buff *next;
        struct sk_buff *prev;

        struct sock *sk;
        struct skb_timeval tstamp;
        struct net_device *dev;
        struct net_device *input_dev;

        union {
                struct tcphdr *th;
                struct udphdr *uh;
                struct icmphdr *icmph;
                struct igmphdr *igmph;
                struct iphdr *ipiph;
                struct ipv6hdr *ipv6h;
                unsigned char *raw;
        } h;

        union {
                struct iphdr *iph;
                struct ipv6hdr *ipv6h;
                struct arphdr *arph;
                unsigned char *raw;
        } nh;

        union {
                unsigned char *raw;
        } mac;

        struct dst_entry *dst;
        struct sec_path *sp;







        char cb[40];

        unsigned int len,
                                data_len,
                                mac_len,
                                csum;
        __u32 priority;
        __u8 local_df:1,
                                cloned:1,
                                ip_summed:2,
                                nohdr:1,
                                nfctinfo:3;
        __u8 pkt_type:3,
                                fclone:2;
        __be16 protocol;

        void (*destructor)(struct sk_buff *skb);

        __u32 nfmark;
        struct nf_conntrack *nfct;

        __u8 ipvs_property:1;


        struct nf_bridge_info *nf_bridge;



        __u16 tc_index;







        unsigned int truesize;
        atomic_t users;
        unsigned char *head,
                                *data,
                                *tail,
                                *end;
};
extern void __kfree_skb(struct sk_buff *skb);
extern struct sk_buff *__alloc_skb(unsigned int size,
                                   gfp_t priority, int fclone);
static struct sk_buff *alloc_skb(unsigned int size,
                                        gfp_t priority)
{
        return __alloc_skb(size, priority, 0);
}

static struct sk_buff *alloc_skb_fclone(unsigned int size,
                                               gfp_t priority)
{
        return __alloc_skb(size, priority, 1);
}

extern struct sk_buff *alloc_skb_from_cache(kmem_cache_t *cp,
                                            unsigned int size,
                                            gfp_t priority);
extern void kfree_skbmem(struct sk_buff *skb);
extern struct sk_buff *skb_clone(struct sk_buff *skb,
                                 gfp_t priority);
extern struct sk_buff *skb_copy(const struct sk_buff *skb,
                                gfp_t priority);
extern struct sk_buff *pskb_copy(struct sk_buff *skb,
                                 gfp_t gfp_mask);
extern int pskb_expand_head(struct sk_buff *skb,
                                        int nhead, int ntail,
                                        gfp_t gfp_mask);
extern struct sk_buff *skb_realloc_headroom(struct sk_buff *skb,
                                            unsigned int headroom);
extern struct sk_buff *skb_copy_expand(const struct sk_buff *skb,
                                       int newheadroom, int newtailroom,
                                       gfp_t priority);
extern struct sk_buff * skb_pad(struct sk_buff *skb, int pad);

extern void skb_over_panic(struct sk_buff *skb, int len,
                                     void *here);
extern void skb_under_panic(struct sk_buff *skb, int len,
                                      void *here);

struct skb_seq_state
{
        __u32 lower_offset;
        __u32 upper_offset;
        __u32 frag_idx;
        __u32 stepped_offset;
        struct sk_buff *root_skb;
        struct sk_buff *cur_skb;
        __u8 *frag_data;
};

extern void skb_prepare_seq_read(struct sk_buff *skb,
                                           unsigned int from, unsigned int to,
                                           struct skb_seq_state *st);
extern unsigned int skb_seq_read(unsigned int consumed, const u8 **data,
                                   struct skb_seq_state *st);
extern void skb_abort_seq_read(struct skb_seq_state *st);

extern unsigned int skb_find_text(struct sk_buff *skb, unsigned int from,
                                    unsigned int to, struct ts_config *config,
                                    struct ts_state *state);
static int skb_queue_empty(const struct sk_buff_head *list)
{
        return list->next == (struct sk_buff *)list;
}
static struct sk_buff *skb_get(struct sk_buff *skb)
{
        atomic_inc(&skb->users);
        return skb;
}
static void kfree_skb(struct sk_buff *skb)
{
        if (__builtin_expect(!!(((&skb->users)->counter) == 1), 1))
                asm volatile ("661:\n\t" "lock; addl $0,0(%%esp)" "\n662:\n" ".section .altinstructions,\"a\"\n" "  .align 4\n" "  .long 661b\n" "  .long 663f\n" "  .byte %c0\n" "  .byte 662b-661b\n" "  .byte 664f-663f\n" ".previous\n" ".section .altinstr_replacement,\"ax\"\n" "663:\n\t" "lfence" "\n664:\n" ".previous" :: "i" ((0*32+26)) : "memory");
        else if (__builtin_expect(!!(!atomic_dec_and_test(&skb->users)), 1))
                return;
        __kfree_skb(skb);
}
static int skb_cloned(const struct sk_buff *skb)
{
        return skb->cloned &&
               (((&((struct skb_shared_info *)((skb)->end))->dataref)->counter) & ((1 << 16) - 1)) != 1;
}
static int skb_header_cloned(const struct sk_buff *skb)
{
        int dataref;

        if (!skb->cloned)
                return 0;

        dataref = ((&((struct skb_shared_info *)((skb)->end))->dataref)->counter);
        dataref = (dataref & ((1 << 16) - 1)) - (dataref >> 16);
        return dataref != 1;
}
static void skb_header_release(struct sk_buff *skb)
{
        do { if (__builtin_expect(!!((skb->nohdr)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (456), "i" ("include/linux/skbuff.h")); } while(0);
        skb->nohdr = 1;
        atomic_add(1 << 16, &((struct skb_shared_info *)((skb)->end))->dataref);
}
static int skb_shared(const struct sk_buff *skb)
{
        return ((&skb->users)->counter) != 1;
}
static struct sk_buff *skb_share_check(struct sk_buff *skb,
                                              gfp_t pri)
{
        do { if (__builtin_expect(!!(pri & 0x10u), 0)) do { do { } while (0); } while (0); } while (0);
        if (skb_shared(skb)) {
                struct sk_buff *nskb = skb_clone(skb, pri);
                kfree_skb(skb);
                skb = nskb;
        }
        return skb;
}
static struct sk_buff *skb_unshare(struct sk_buff *skb,
                                          gfp_t pri)
{
        do { if (__builtin_expect(!!(pri & 0x10u), 0)) do { do { } while (0); } while (0); } while (0);
        if (skb_cloned(skb)) {
                struct sk_buff *nskb = skb_copy(skb, pri);
                kfree_skb(skb);
                skb = nskb;
        }
        return skb;
}
static struct sk_buff *skb_peek(struct sk_buff_head *list_)
{
        struct sk_buff *list = ((struct sk_buff *)list_)->next;
        if (list == (struct sk_buff *)list_)
                list = ((void *)0);
        return list;
}
static struct sk_buff *skb_peek_tail(struct sk_buff_head *list_)
{
        struct sk_buff *list = ((struct sk_buff *)list_)->prev;
        if (list == (struct sk_buff *)list_)
                list = ((void *)0);
        return list;
}







static __u32 skb_queue_len(const struct sk_buff_head *list_)
{
        return list_->qlen;
}

static void skb_queue_head_init(struct sk_buff_head *list)
{
        spin_lock_init(&list->lock);
        list->prev = list->next = (struct sk_buff *)list;
        list->qlen = 0;
}
extern void skb_queue_head(struct sk_buff_head *list, struct sk_buff *newsk);
static void __skb_queue_head(struct sk_buff_head *list,
                                    struct sk_buff *newsk)
{
        struct sk_buff *prev, *next;

        list->qlen++;
        prev = (struct sk_buff *)list;
        next = prev->next;
        newsk->next = next;
        newsk->prev = prev;
        next->prev = prev->next = newsk;
}
extern void skb_queue_tail(struct sk_buff_head *list, struct sk_buff *newsk);
static void __skb_queue_tail(struct sk_buff_head *list,
                                   struct sk_buff *newsk)
{
        struct sk_buff *prev, *next;

        list->qlen++;
        next = (struct sk_buff *)list;
        prev = next->prev;
        newsk->next = next;
        newsk->prev = prev;
        next->prev = prev->next = newsk;
}
extern struct sk_buff *skb_dequeue(struct sk_buff_head *list);
static struct sk_buff *__skb_dequeue(struct sk_buff_head *list)
{
        struct sk_buff *next, *prev, *result;

        prev = (struct sk_buff *) list;
        next = prev->next;
        result = ((void *)0);
        if (next != prev) {
                result = next;
                next = next->next;
                list->qlen--;
                next->prev = prev;
                prev->next = next;
                result->next = result->prev = ((void *)0);
        }
        return result;
}





extern void skb_insert(struct sk_buff *old, struct sk_buff *newsk, struct sk_buff_head *list);
static void __skb_insert(struct sk_buff *newsk,
                                struct sk_buff *prev, struct sk_buff *next,
                                struct sk_buff_head *list)
{
        newsk->next = next;
        newsk->prev = prev;
        next->prev = prev->next = newsk;
        list->qlen++;
}




extern void skb_append(struct sk_buff *old, struct sk_buff *newsk, struct sk_buff_head *list);
static void __skb_append(struct sk_buff *old, struct sk_buff *newsk, struct sk_buff_head *list)
{
        __skb_insert(newsk, old, old->next, list);
}





extern void skb_unlink(struct sk_buff *skb, struct sk_buff_head *list);
static void __skb_unlink(struct sk_buff *skb, struct sk_buff_head *list)
{
        struct sk_buff *next, *prev;

        list->qlen--;
        next = skb->next;
        prev = skb->prev;
        skb->next = skb->prev = ((void *)0);
        next->prev = prev;
        prev->next = next;
}
extern struct sk_buff *skb_dequeue_tail(struct sk_buff_head *list);
static struct sk_buff *__skb_dequeue_tail(struct sk_buff_head *list)
{
        struct sk_buff *skb = skb_peek_tail(list);
        if (skb)
                __skb_unlink(skb, list);
        return skb;
}


static int skb_is_nonlinear(const struct sk_buff *skb)
{
        return skb->data_len;
}

static unsigned int skb_headlen(const struct sk_buff *skb)
{
        return skb->len - skb->data_len;
}

static int skb_pagelen(const struct sk_buff *skb)
{
        int i, len = 0;

        for (i = (int)((struct skb_shared_info *)((skb)->end))->nr_frags - 1; i >= 0; i--)
                len += ((struct skb_shared_info *)((skb)->end))->frags[i].size;
        return len + skb_headlen(skb);
}

static void skb_fill_page_desc(struct sk_buff *skb, int i,
                                      struct page *page, int off, int size)
{
        skb_frag_t *frag = &((struct skb_shared_info *)((skb)->end))->frags[i];

        frag->page = page;
        frag->page_offset = off;
        frag->size = size;
        ((struct skb_shared_info *)((skb)->end))->nr_frags = i + 1;
}
static unsigned char *__skb_put(struct sk_buff *skb, unsigned int len)
{
        unsigned char *tmp = skb->tail;
        do { if (__builtin_expect(!!((skb_is_nonlinear(skb))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (775), "i" ("include/linux/skbuff.h")); } while(0);
        skb->tail += len;
        skb->len += len;
        return tmp;
}
static unsigned char *skb_put(struct sk_buff *skb, unsigned int len)
{
        unsigned char *tmp = skb->tail;
        do { if (__builtin_expect(!!((skb_is_nonlinear(skb))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (793), "i" ("include/linux/skbuff.h")); } while(0);
        skb->tail += len;
        skb->len += len;
        if (__builtin_expect(!!(skb->tail>skb->end), 0))
                skb_over_panic(skb, len, ({ void *pc; __asm__("movl $1f,%0\n1:":"=g" (pc)); pc; }));
        return tmp;
}

static unsigned char *__skb_push(struct sk_buff *skb, unsigned int len)
{
        skb->data -= len;
        skb->len += len;
        return skb->data;
}
static unsigned char *skb_push(struct sk_buff *skb, unsigned int len)
{
        skb->data -= len;
        skb->len += len;
        if (__builtin_expect(!!(skb->data<skb->head), 0))
                skb_under_panic(skb, len, ({ void *pc; __asm__("movl $1f,%0\n1:":"=g" (pc)); pc; }));
        return skb->data;
}

static unsigned char *__skb_pull(struct sk_buff *skb, unsigned int len)
{
        skb->len -= len;
        do { if (__builtin_expect(!!((skb->len < skb->data_len)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (829), "i" ("include/linux/skbuff.h")); } while(0);
        return skb->data += len;
}
static unsigned char *skb_pull(struct sk_buff *skb, unsigned int len)
{
        return __builtin_expect(!!(len > skb->len), 0) ? ((void *)0) : __skb_pull(skb, len);
}

extern unsigned char *__pskb_pull_tail(struct sk_buff *skb, int delta);

static unsigned char *__pskb_pull(struct sk_buff *skb, unsigned int len)
{
        if (len > skb_headlen(skb) &&
            !__pskb_pull_tail(skb, len-skb_headlen(skb)))
                return ((void *)0);
        skb->len -= len;
        return skb->data += len;
}

static unsigned char *pskb_pull(struct sk_buff *skb, unsigned int len)
{
        return __builtin_expect(!!(len > skb->len), 0) ? ((void *)0) : __pskb_pull(skb, len);
}

static int pskb_may_pull(struct sk_buff *skb, unsigned int len)
{
        if (__builtin_expect(!!(len <= skb_headlen(skb)), 1))
                return 1;
        if (__builtin_expect(!!(len > skb->len), 0))
                return 0;
        return __pskb_pull_tail(skb, len-skb_headlen(skb)) != ((void *)0);
}







static int skb_headroom(const struct sk_buff *skb)
{
        return skb->data - skb->head;
}







static int skb_tailroom(const struct sk_buff *skb)
{
        return skb_is_nonlinear(skb) ? 0 : skb->end - skb->tail;
}
static void skb_reserve(struct sk_buff *skb, unsigned int len)
{
        skb->data += len;
        skb->tail += len;
}
extern int ___pskb_trim(struct sk_buff *skb, unsigned int len, int realloc);

static void __skb_trim(struct sk_buff *skb, unsigned int len)
{
        if (!skb->data_len) {
                skb->len = len;
                skb->tail = skb->data + len;
        } else
                ___pskb_trim(skb, len, 0);
}
static void skb_trim(struct sk_buff *skb, unsigned int len)
{
        if (skb->len > len)
                __skb_trim(skb, len);
}


static int __pskb_trim(struct sk_buff *skb, unsigned int len)
{
        if (!skb->data_len) {
                skb->len = len;
                skb->tail = skb->data+len;
                return 0;
        }
        return ___pskb_trim(skb, len, 1);
}

static int pskb_trim(struct sk_buff *skb, unsigned int len)
{
        return (len < skb->len) ? __pskb_trim(skb, len) : 0;
}
static void skb_orphan(struct sk_buff *skb)
{
        if (skb->destructor)
                skb->destructor(skb);
        skb->destructor = ((void *)0);
        skb->sk = ((void *)0);
}
extern void skb_queue_purge(struct sk_buff_head *list);
static void __skb_queue_purge(struct sk_buff_head *list)
{
        struct sk_buff *skb;
        while ((skb = __skb_dequeue(list)) != ((void *)0))
                kfree_skb(skb);
}
static struct sk_buff *__dev_alloc_skb(unsigned int length,
                                              gfp_t gfp_mask)
{
        struct sk_buff *skb = alloc_skb(length + 16, gfp_mask);
        if (__builtin_expect(!!(skb), 1))
                skb_reserve(skb, 16);
        return skb;
}
static struct sk_buff *dev_alloc_skb(unsigned int length)
{
        return __dev_alloc_skb(length, (0x20u));
}
static int skb_cow(struct sk_buff *skb, unsigned int headroom)
{
        int delta = (headroom > 16 ? headroom : 16) - skb_headroom(skb);

        if (delta < 0)
                delta = 0;

        if (delta || skb_cloned(skb))
                return pskb_expand_head(skb, (delta + 15) & ~15, 0, (0x20u));
        return 0;
}
static struct sk_buff *skb_padto(struct sk_buff *skb, unsigned int len)
{
        unsigned int size = skb->len;
        if (__builtin_expect(!!(size >= len), 1))
                return skb;
        return skb_pad(skb, len-size);
}

static int skb_add_data(struct sk_buff *skb,
                               char *from, int copy)
{
        const int off = skb->len;

        if (skb->ip_summed == 0) {
                int err = 0;
                unsigned int csum = csum_and_copy_from_user(from,
                                                            skb_put(skb, copy),
                                                            copy, 0, &err);
                if (!err) {
                        skb->csum = csum_block_add(skb->csum, csum, off);
                        return 0;
                }
        } else if (!copy_from_user(skb_put(skb, copy), from, copy))
                return 0;

        __skb_trim(skb, off);
        return -14;
}

static int skb_can_coalesce(struct sk_buff *skb, int i,
                                   struct page *page, int off)
{
        if (i) {
                struct skb_frag_struct *frag = &((struct skb_shared_info *)((skb)->end))->frags[i - 1];

                return page == frag->page &&
                       off == frag->page_offset + frag->size;
        }
        return 0;
}
extern int __skb_linearize(struct sk_buff *skb, gfp_t gfp);
static int skb_linearize(struct sk_buff *skb, gfp_t gfp)
{
        return __skb_linearize(skb, gfp);
}
static void skb_postpull_rcsum(struct sk_buff *skb,
                                         const void *start, int len)
{
        if (skb->ip_summed == 1)
                skb->csum = csum_sub(skb->csum, csum_partial(start, len, 0));
}
static int pskb_trim_rcsum(struct sk_buff *skb, unsigned int len)
{
        if (__builtin_expect(!!(len >= skb->len), 1))
                return 0;
        if (skb->ip_summed == 1)
                skb->ip_summed = 0;
        return __pskb_trim(skb, len);
}

static void *kmap_skb_frag(const skb_frag_t *frag)
{

        do { if (__builtin_expect(!!(((((current_thread_info()->preempt_count) & (((1UL << (12))-1) << ((0 + 8) + 8)))))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (1178), "i" ("include/linux/skbuff.h")); } while(0);

        do { do { (current_thread_info()->preempt_count) += ((1UL << (0 + 8))); } while (0); __asm__ __volatile__("": : :"memory"); } while (0);

        return kmap_atomic(frag->page, KM_SKB_DATA_SOFTIRQ);
}

static void kunmap_skb_frag(void *vaddr)
{
        kunmap_atomic(vaddr, KM_SKB_DATA_SOFTIRQ);

        local_bh_enable();

}







extern struct sk_buff *skb_recv_datagram(struct sock *sk, unsigned flags,
                                         int noblock, int *err);
extern unsigned int datagram_poll(struct file *file, struct socket *sock,
                                     struct poll_table_struct *wait);
extern int skb_copy_datagram_iovec(const struct sk_buff *from,
                                               int offset, struct iovec *to,
                                               int size);
extern int skb_copy_and_csum_datagram_iovec(const
                                                        struct sk_buff *skb,
                                                        int hlen,
                                                        struct iovec *iov);
extern void skb_free_datagram(struct sock *sk, struct sk_buff *skb);
extern unsigned int skb_checksum(const struct sk_buff *skb, int offset,
                                    int len, unsigned int csum);
extern int skb_copy_bits(const struct sk_buff *skb, int offset,
                                     void *to, int len);
extern int skb_store_bits(const struct sk_buff *skb, int offset,
                                      void *from, int len);
extern unsigned int skb_copy_and_csum_bits(const struct sk_buff *skb,
                                              int offset, u8 *to, int len,
                                              unsigned int csum);
extern void skb_copy_and_csum_dev(const struct sk_buff *skb, u8 *to);
extern void skb_split(struct sk_buff *skb,
                                 struct sk_buff *skb1, const u32 len);

extern void skb_release_data(struct sk_buff *skb);

static void *skb_header_pointer(const struct sk_buff *skb, int offset,
                                       int len, void *buffer)
{
        int hlen = skb_headlen(skb);

        if (hlen - offset >= len)
                return skb->data + offset;

        if (skb_copy_bits(skb, offset, buffer, len) < 0)
                return ((void *)0);

        return buffer;
}

extern void skb_init(void);
extern void skb_add_mtu(int mtu);
static void skb_get_timestamp(const struct sk_buff *skb, struct timeval *stamp)
{
        stamp->tv_sec = skb->tstamp.off_sec;
        stamp->tv_usec = skb->tstamp.off_usec;
}
static void skb_set_timestamp(struct sk_buff *skb, const struct timeval *stamp)
{
        skb->tstamp.off_sec = stamp->tv_sec;
        skb->tstamp.off_usec = stamp->tv_usec;
}

extern void __net_timestamp(struct sk_buff *skb);


static void nf_conntrack_put(struct nf_conntrack *nfct)
{
        if (nfct && atomic_dec_and_test(&nfct->use))
                nfct->destroy(nfct);
}
static void nf_conntrack_get(struct nf_conntrack *nfct)
{
        if (nfct)
                atomic_inc(&nfct->use);
}
static void nf_reset(struct sk_buff *skb)
{
        nf_conntrack_put(skb->nfct);
        skb->nfct = ((void *)0);
}


static void nf_bridge_put(struct nf_bridge_info *nf_bridge)
{
        if (nf_bridge && atomic_dec_and_test(&nf_bridge->use))
                kfree(nf_bridge);
}
static void nf_bridge_get(struct nf_bridge_info *nf_bridge)
{
        if (nf_bridge)
                atomic_inc(&nf_bridge->use);
}

static struct ethhdr *eth_hdr(const struct sk_buff *skb)
{
        return (struct ethhdr *)skb->mac.raw;
}


extern struct ctl_table ether_table[];



struct sockaddr_pkt
{
        unsigned short spkt_family;
        unsigned char spkt_device[14];
        unsigned short spkt_protocol;
};

struct sockaddr_ll
{
        unsigned short sll_family;
        unsigned short sll_protocol;
        int sll_ifindex;
        unsigned short sll_hatype;
        unsigned char sll_pkttype;
        unsigned char sll_halen;
        unsigned char sll_addr[8];
};
struct tpacket_stats
{
        unsigned int tp_packets;
        unsigned int tp_drops;
};

struct tpacket_hdr
{
        unsigned long tp_status;





        unsigned int tp_len;
        unsigned int tp_snaplen;
        unsigned short tp_mac;
        unsigned short tp_net;
        unsigned int tp_sec;
        unsigned int tp_usec;
};
struct tpacket_req
{
        unsigned int tp_block_size;
        unsigned int tp_block_nr;
        unsigned int tp_frame_size;
        unsigned int tp_frame_nr;
};

struct packet_mreq
{
        int mr_ifindex;
        unsigned short mr_type;
        unsigned short mr_alen;
        unsigned char mr_address[8];
};
struct divert_blk;
struct vlan_group;
struct ethtool_ops;
struct netpoll_info;
struct net_device_stats
{
        unsigned long rx_packets;
        unsigned long tx_packets;
        unsigned long rx_bytes;
        unsigned long tx_bytes;
        unsigned long rx_errors;
        unsigned long tx_errors;
        unsigned long rx_dropped;
        unsigned long tx_dropped;
        unsigned long multicast;
        unsigned long collisions;


        unsigned long rx_length_errors;
        unsigned long rx_over_errors;
        unsigned long rx_crc_errors;
        unsigned long rx_frame_errors;
        unsigned long rx_fifo_errors;
        unsigned long rx_missed_errors;


        unsigned long tx_aborted_errors;
        unsigned long tx_carrier_errors;
        unsigned long tx_fifo_errors;
        unsigned long tx_heartbeat_errors;
        unsigned long tx_window_errors;


        unsigned long rx_compressed;
        unsigned long tx_compressed;
};



enum {
        IF_PORT_UNKNOWN = 0,
        IF_PORT_10BASE2,
        IF_PORT_10BASET,
        IF_PORT_AUI,
        IF_PORT_100BASET,
        IF_PORT_100BASETX,
        IF_PORT_100BASEFX
};






struct neighbour;
struct neigh_parms;
struct sk_buff;

struct netif_rx_stats
{
        unsigned total;
        unsigned dropped;
        unsigned time_squeeze;
        unsigned cpu_collision;
};

extern __typeof__(struct netif_rx_stats) per_cpu__netdev_rx_stat;






struct dev_mc_list
{
        struct dev_mc_list *next;
        __u8 dmi_addr[32];
        unsigned char dmi_addrlen;
        int dmi_users;
        int dmi_gusers;
};

struct hh_cache
{
        struct hh_cache *hh_next;
        atomic_t hh_refcnt;
        unsigned short hh_type;



        int hh_len;
        int (*hh_output)(struct sk_buff *skb);
        rwlock_t hh_lock;







        unsigned long hh_data[(((48)+(16 -1))&~(16 - 1)) / sizeof(long)];
};
enum netdev_state_t
{
        __LINK_STATE_XOFF=0,
        __LINK_STATE_START,
        __LINK_STATE_PRESENT,
        __LINK_STATE_SCHED,
        __LINK_STATE_NOCARRIER,
        __LINK_STATE_RX_SCHED,
        __LINK_STATE_LINKWATCH_PENDING
};






struct netdev_boot_setup {
        char name[16];
        struct ifmap map;
};


extern int __attribute__ ((__section__ (".init.text"))) netdev_boot_setup(char *str);
struct net_device
{






        char name[16];

        struct hlist_node name_hlist;





        unsigned long mem_end;
        unsigned long mem_start;
        unsigned long base_addr;
        unsigned int irq;






        unsigned char if_port;
        unsigned char dma;

        unsigned long state;

        struct net_device *next;


        int (*init)(struct net_device *dev);




        unsigned long features;
        struct net_device *next_sched;


        int ifindex;
        int iflink;


        struct net_device_stats* (*get_stats)(struct net_device *dev);
        struct iw_statistics* (*get_wireless_stats)(struct net_device *dev);



        const struct iw_handler_def * wireless_handlers;

        struct iw_public_data * wireless_data;

        struct ethtool_ops *ethtool_ops;
        unsigned short flags;
        unsigned short gflags;
        unsigned short priv_flags;
        unsigned short padded;

        unsigned mtu;
        unsigned short type;
        unsigned short hard_header_len;

        struct net_device *master;




        unsigned char perm_addr[32];
        unsigned char addr_len;
        unsigned short dev_id;

        struct dev_mc_list *mc_list;
        int mc_count;
        int promiscuity;
        int allmulti;




        void *atalk_ptr;
        void *ip_ptr;
        void *dn_ptr;
        void *ip6_ptr;
        void *ec_ptr;
        void *ax25_ptr;




        struct list_head poll_list __attribute__((__aligned__((1 << (4)))));


        int (*poll) (struct net_device *dev, int *quota);
        int quota;
        int weight;
        unsigned long last_rx;

        unsigned char dev_addr[32];


        unsigned char broadcast[32];





        spinlock_t queue_lock __attribute__((__aligned__((1 << (4)))));
        struct Qdisc *qdisc;
        struct Qdisc *qdisc_sleeping;
        struct list_head qdisc_list;
        unsigned long tx_queue_len;


        spinlock_t ingress_lock;
        struct Qdisc *qdisc_ingress;





        spinlock_t xmit_lock __attribute__((__aligned__((1 << (4)))));



        int xmit_lock_owner;
        void *priv;
        int (*hard_start_xmit) (struct sk_buff *skb,
                                                    struct net_device *dev);

        unsigned long trans_start;

        int watchdog_timeo;
        struct timer_list watchdog_timer;





        atomic_t refcnt __attribute__((__aligned__((1 << (4)))));


        struct list_head todo_list;

        struct hlist_node index_hlist;


        enum { NETREG_UNINITIALIZED=0,
               NETREG_REGISTERING,
               NETREG_REGISTERED,
               NETREG_UNREGISTERING,
               NETREG_UNREGISTERED,
               NETREG_RELEASED,
        } reg_state;


        void (*uninit)(struct net_device *dev);

        void (*destructor)(struct net_device *dev);


        int (*open)(struct net_device *dev);
        int (*stop)(struct net_device *dev);

        int (*hard_header) (struct sk_buff *skb,
                                                struct net_device *dev,
                                                unsigned short type,
                                                void *daddr,
                                                void *saddr,
                                                unsigned len);
        int (*rebuild_header)(struct sk_buff *skb);

        void (*set_multicast_list)(struct net_device *dev);

        int (*set_mac_address)(struct net_device *dev,
                                                   void *addr);

        int (*do_ioctl)(struct net_device *dev,
                                            struct ifreq *ifr, int cmd);

        int (*set_config)(struct net_device *dev,
                                              struct ifmap *map);

        int (*hard_header_cache)(struct neighbour *neigh,
                                                     struct hh_cache *hh);
        void (*header_cache_update)(struct hh_cache *hh,
                                                       struct net_device *dev,
                                                       unsigned char * haddr);

        int (*change_mtu)(struct net_device *dev, int new_mtu);


        void (*tx_timeout) (struct net_device *dev);

        void (*vlan_rx_register)(struct net_device *dev,
                                                    struct vlan_group *grp);
        void (*vlan_rx_add_vid)(struct net_device *dev,
                                                   unsigned short vid);
        void (*vlan_rx_kill_vid)(struct net_device *dev,
                                                    unsigned short vid);

        int (*hard_header_parse)(struct sk_buff *skb,
                                                     unsigned char *haddr);
        int (*neigh_setup)(struct net_device *dev, struct neigh_parms *);

        struct netpoll_info *npinfo;


        void (*poll_controller)(struct net_device *dev);



        struct net_bridge_port *br_port;



        struct divert_blk *divert;



        struct class_device class_dev;
};




static void *netdev_priv(struct net_device *dev)
{
        return (char *)dev + ((sizeof(struct net_device)
                                        + (32 - 1))
                                & ~(32 - 1));
}







struct packet_type {
        __be16 type;
        struct net_device *dev;
        int (*func) (struct sk_buff *,
                                         struct net_device *,
                                         struct packet_type *,
                                         struct net_device *);
        void *af_packet_priv;
        struct list_head list;
};


struct notifier_block
{
        int (*notifier_call)(struct notifier_block *self, unsigned long, void *);
        struct notifier_block *next;
        int priority;
};




extern int notifier_chain_register(struct notifier_block **list, struct notifier_block *n);
extern int notifier_chain_unregister(struct notifier_block **nl, struct notifier_block *n);
extern int notifier_call_chain(struct notifier_block **n, unsigned long val, void *v);

extern struct net_device loopback_dev;
extern struct net_device *dev_base;
extern rwlock_t dev_base_lock;

extern int netdev_boot_setup_check(struct net_device *dev);
extern unsigned long netdev_boot_base(const char *prefix, int unit);
extern struct net_device *dev_getbyhwaddr(unsigned short type, char *hwaddr);
extern struct net_device *dev_getfirstbyhwtype(unsigned short type);
extern void dev_add_pack(struct packet_type *pt);
extern void dev_remove_pack(struct packet_type *pt);
extern void __dev_remove_pack(struct packet_type *pt);

extern struct net_device *dev_get_by_flags(unsigned short flags,
                                                  unsigned short mask);
extern struct net_device *dev_get_by_name(const char *name);
extern struct net_device *__dev_get_by_name(const char *name);
extern int dev_alloc_name(struct net_device *dev, const char *name);
extern int dev_open(struct net_device *dev);
extern int dev_close(struct net_device *dev);
extern int dev_queue_xmit(struct sk_buff *skb);
extern int register_netdevice(struct net_device *dev);
extern int unregister_netdevice(struct net_device *dev);
extern void free_netdev(struct net_device *dev);
extern void synchronize_net(void);
extern int register_netdevice_notifier(struct notifier_block *nb);
extern int unregister_netdevice_notifier(struct notifier_block *nb);
extern int call_netdevice_notifiers(unsigned long val, void *v);
extern struct net_device *dev_get_by_index(int ifindex);
extern struct net_device *__dev_get_by_index(int ifindex);
extern int dev_restart(struct net_device *dev);

extern int netpoll_trap(void);


typedef int gifconf_func_t(struct net_device * dev, char * bufptr, int len);
extern int register_gifconf(unsigned int family, gifconf_func_t * gifconf);
static int unregister_gifconf(unsigned int family)
{
        return register_gifconf(family, ((void *)0));
}






struct softnet_data
{
        struct net_device *output_queue;
        struct sk_buff_head input_pkt_queue;
        struct list_head poll_list;
        struct sk_buff *completion_queue;

        struct net_device backlog_dev;
};

extern __typeof__(struct softnet_data) per_cpu__softnet_data;



static void __netif_schedule(struct net_device *dev)
{
        if (!test_and_set_bit(__LINK_STATE_SCHED, &dev->state)) {
                unsigned long flags;
                struct softnet_data *sd;

                __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (flags): :"memory");
                sd = &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__softnet_data)); (typeof(&per_cpu__softnet_data)) (__ptr + (__per_cpu_offset[(current_thread_info()->cpu)])); }));
                dev->next_sched = sd->output_queue;
                sd->output_queue = dev;
                raise_softirq_irqoff(NET_TX_SOFTIRQ);
                do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc"); } while (0);
        }
}

static void netif_schedule(struct net_device *dev)
{
        if (!(__builtin_constant_p(__LINK_STATE_XOFF) ? constant_test_bit((__LINK_STATE_XOFF),(&dev->state)) : variable_test_bit((__LINK_STATE_XOFF),(&dev->state))))
                __netif_schedule(dev);
}

static void netif_start_queue(struct net_device *dev)
{
        clear_bit(__LINK_STATE_XOFF, &dev->state);
}

static void netif_wake_queue(struct net_device *dev)
{

        if (netpoll_trap())
                return;

        if (test_and_clear_bit(__LINK_STATE_XOFF, &dev->state))
                __netif_schedule(dev);
}

static void netif_stop_queue(struct net_device *dev)
{

        if (netpoll_trap())
                return;

        set_bit(__LINK_STATE_XOFF, &dev->state);
}

static int netif_queue_stopped(const struct net_device *dev)
{
        return (__builtin_constant_p(__LINK_STATE_XOFF) ? constant_test_bit((__LINK_STATE_XOFF),(&dev->state)) : variable_test_bit((__LINK_STATE_XOFF),(&dev->state)));
}

static int netif_running(const struct net_device *dev)
{
        return (__builtin_constant_p(__LINK_STATE_START) ? constant_test_bit((__LINK_STATE_START),(&dev->state)) : variable_test_bit((__LINK_STATE_START),(&dev->state)));
}





static void dev_kfree_skb_irq(struct sk_buff *skb)
{
        if (atomic_dec_and_test(&skb->users)) {
                struct softnet_data *sd;
                unsigned long flags;

                __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (flags): :"memory");
                sd = &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__softnet_data)); (typeof(&per_cpu__softnet_data)) (__ptr + (__per_cpu_offset[(current_thread_info()->cpu)])); }));
                skb->next = sd->completion_queue;
                sd->completion_queue = skb;
                raise_softirq_irqoff(NET_TX_SOFTIRQ);
                do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc"); } while (0);
        }
}




static void dev_kfree_skb_any(struct sk_buff *skb)
{
        if ((((current_thread_info()->preempt_count) & (((1UL << (12))-1) << ((0 + 8) + 8)))) || ({ unsigned long flags; do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); } while (0); !(flags & (1<<9)); }))
                dev_kfree_skb_irq(skb);
        else
                kfree_skb(skb);
}


extern int netif_rx(struct sk_buff *skb);
extern int netif_rx_ni(struct sk_buff *skb);

extern int netif_receive_skb(struct sk_buff *skb);
extern int dev_ioctl(unsigned int cmd, void *);
extern int dev_ethtool(struct ifreq *);
extern unsigned dev_get_flags(const struct net_device *);
extern int dev_change_flags(struct net_device *, unsigned);
extern int dev_change_name(struct net_device *, char *);
extern int dev_set_mtu(struct net_device *, int);
extern int dev_set_mac_address(struct net_device *,
                                            struct sockaddr *);
extern void dev_queue_xmit_nit(struct sk_buff *skb, struct net_device *dev);

extern void dev_init(void);

extern int netdev_nit;
extern int netdev_budget;


extern void netdev_run_todo(void);

static void dev_put(struct net_device *dev)
{
        atomic_dec(&dev->refcnt);
}
extern void linkwatch_fire_event(struct net_device *dev);

static int netif_carrier_ok(const struct net_device *dev)
{
        return !(__builtin_constant_p(__LINK_STATE_NOCARRIER) ? constant_test_bit((__LINK_STATE_NOCARRIER),(&dev->state)) : variable_test_bit((__LINK_STATE_NOCARRIER),(&dev->state)));
}

extern void __netdev_watchdog_up(struct net_device *dev);

extern void netif_carrier_on(struct net_device *dev);

extern void netif_carrier_off(struct net_device *dev);


static int netif_device_present(struct net_device *dev)
{
        return (__builtin_constant_p(__LINK_STATE_PRESENT) ? constant_test_bit((__LINK_STATE_PRESENT),(&dev->state)) : variable_test_bit((__LINK_STATE_PRESENT),(&dev->state)));
}

static void netif_device_detach(struct net_device *dev)
{
        if (test_and_clear_bit(__LINK_STATE_PRESENT, &dev->state) &&
            netif_running(dev)) {
                netif_stop_queue(dev);
        }
}

static void netif_device_attach(struct net_device *dev)
{
        if (!test_and_set_bit(__LINK_STATE_PRESENT, &dev->state) &&
            netif_running(dev)) {
                netif_wake_queue(dev);
                __netdev_watchdog_up(dev);
        }
}






enum {
        NETIF_MSG_DRV = 0x0001,
        NETIF_MSG_PROBE = 0x0002,
        NETIF_MSG_LINK = 0x0004,
        NETIF_MSG_TIMER = 0x0008,
        NETIF_MSG_IFDOWN = 0x0010,
        NETIF_MSG_IFUP = 0x0020,
        NETIF_MSG_RX_ERR = 0x0040,
        NETIF_MSG_TX_ERR = 0x0080,
        NETIF_MSG_TX_QUEUED = 0x0100,
        NETIF_MSG_INTR = 0x0200,
        NETIF_MSG_TX_DONE = 0x0400,
        NETIF_MSG_RX_STATUS = 0x0800,
        NETIF_MSG_PKTDATA = 0x1000,
        NETIF_MSG_HW = 0x2000,
        NETIF_MSG_WOL = 0x4000,
};
static u32 netif_msg_init(int debug_value, int default_msg_enable_bits)
{

        if (debug_value < 0 || debug_value >= (sizeof(u32) * 8))
                return default_msg_enable_bits;
        if (debug_value == 0)
                return 0;

        return (1 << debug_value) - 1;
}



static int netif_rx_schedule_prep(struct net_device *dev)
{
        return netif_running(dev) &&
                !test_and_set_bit(__LINK_STATE_RX_SCHED, &dev->state);
}





static void __netif_rx_schedule(struct net_device *dev)
{
        unsigned long flags;

        __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (flags): :"memory");
        atomic_inc(&(dev)->refcnt);
        list_add_tail(&dev->poll_list, &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__softnet_data)); (typeof(&per_cpu__softnet_data)) (__ptr + (__per_cpu_offset[(current_thread_info()->cpu)])); })).poll_list);
        if (dev->quota < 0)
                dev->quota += dev->weight;
        else
                dev->quota = dev->weight;
        do { (((*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__irq_stat)); (typeof(&per_cpu__irq_stat)) (__ptr + (__per_cpu_offset[(current_thread_info()->cpu)])); })).__softirq_pending) |= (1UL << (NET_RX_SOFTIRQ))); } while (0);
        do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc"); } while (0);
}



static void netif_rx_schedule(struct net_device *dev)
{
        if (netif_rx_schedule_prep(dev))
                __netif_rx_schedule(dev);
}




static int netif_rx_reschedule(struct net_device *dev, int undo)
{
        if (netif_rx_schedule_prep(dev)) {
                unsigned long flags;

                dev->quota += undo;

                __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (flags): :"memory");
                list_add_tail(&dev->poll_list, &(*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__softnet_data)); (typeof(&per_cpu__softnet_data)) (__ptr + (__per_cpu_offset[(current_thread_info()->cpu)])); })).poll_list);
                do { (((*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&per_cpu__irq_stat)); (typeof(&per_cpu__irq_stat)) (__ptr + (__per_cpu_offset[(current_thread_info()->cpu)])); })).__softirq_pending) |= (1UL << (NET_RX_SOFTIRQ))); } while (0);
                do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc"); } while (0);
                return 1;
        }
        return 0;
}






static void netif_rx_complete(struct net_device *dev)
{
        unsigned long flags;

        __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (flags): :"memory");
        do { if (__builtin_expect(!!((!(__builtin_constant_p(__LINK_STATE_RX_SCHED) ? constant_test_bit((__LINK_STATE_RX_SCHED),(&dev->state)) : variable_test_bit((__LINK_STATE_RX_SCHED),(&dev->state))))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (867), "i" ("include/linux/netdevice.h")); } while(0);
        list_del(&dev->poll_list);
        __asm__ __volatile__("": : :"memory");
        clear_bit(__LINK_STATE_RX_SCHED, &dev->state);
        do { ({ unsigned long __dummy; typeof(flags) __dummy2; (void)(&__dummy == &__dummy2); 1; }); __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc"); } while (0);
}

static void netif_poll_disable(struct net_device *dev)
{
        while (test_and_set_bit(__LINK_STATE_RX_SCHED, &dev->state)) {

                get_current()->state = 1;
                schedule_timeout(1);
        }
}

static void netif_poll_enable(struct net_device *dev)
{
        clear_bit(__LINK_STATE_RX_SCHED, &dev->state);
}




static void __netif_rx_complete(struct net_device *dev)
{
        do { if (__builtin_expect(!!((!(__builtin_constant_p(__LINK_STATE_RX_SCHED) ? constant_test_bit((__LINK_STATE_RX_SCHED),(&dev->state)) : variable_test_bit((__LINK_STATE_RX_SCHED),(&dev->state))))!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (893), "i" ("include/linux/netdevice.h")); } while(0);
        list_del(&dev->poll_list);
        __asm__ __volatile__("": : :"memory");
        clear_bit(__LINK_STATE_RX_SCHED, &dev->state);
}

static void netif_tx_disable(struct net_device *dev)
{
        _spin_lock_bh(&dev->xmit_lock);
        netif_stop_queue(dev);
        _spin_unlock_bh(&dev->xmit_lock);
}



extern void ether_setup(struct net_device *dev);


extern struct net_device *alloc_netdev(int sizeof_priv, const char *name,
                                       void (*setup)(struct net_device *));
extern int register_netdev(struct net_device *dev);
extern void unregister_netdev(struct net_device *dev);

extern void dev_mc_upload(struct net_device *dev);
extern int dev_mc_delete(struct net_device *dev, void *addr, int alen, int all);
extern int dev_mc_add(struct net_device *dev, void *addr, int alen, int newonly);
extern void dev_mc_discard(struct net_device *dev);
extern void dev_set_promiscuity(struct net_device *dev, int inc);
extern void dev_set_allmulti(struct net_device *dev, int inc);
extern void netdev_state_change(struct net_device *dev);
extern void netdev_features_change(struct net_device *dev);

extern void dev_load(const char *name);
extern void dev_mcast_init(void);
extern int netdev_max_backlog;
extern int weight_p;
extern int netdev_set_master(struct net_device *dev, struct net_device *master);
extern int skb_checksum_help(struct sk_buff *skb, int inward);

extern void net_enable_timestamp(void);
extern void net_disable_timestamp(void);


extern void *dev_seq_start(struct seq_file *seq, loff_t *pos);
extern void *dev_seq_next(struct seq_file *seq, void *v, loff_t *pos);
extern void dev_seq_stop(struct seq_file *seq, void *v);


extern void linkwatch_run_queue(void);










spinlock_t dma_spin_lock;

static unsigned long claim_dma_lock(void)
{
        unsigned long flags;
        flags = _spin_lock_irqsave(&dma_spin_lock);
        return flags;
}

static void release_dma_lock(unsigned long flags)
{
        _spin_unlock_irqrestore(&dma_spin_lock, flags);
}


static void enable_dma(unsigned int dmanr)
{
        if (dmanr<=3)
                outb(dmanr, 0x0A);
        else
                outb(dmanr & 3, 0xD4);
}

static void disable_dma(unsigned int dmanr)
{
        if (dmanr<=3)
                outb(dmanr | 4, 0x0A);
        else
                outb((dmanr & 3) | 4, 0xD4);
}
static void clear_dma_ff(unsigned int dmanr)
{
        if (dmanr<=3)
                outb(0, 0x0C);
        else
                outb(0, 0xD8);
}


static void set_dma_mode(unsigned int dmanr, char mode)
{
        if (dmanr<=3)
                outb(mode | dmanr, 0x0B);
        else
                outb(mode | (dmanr&3), 0xD6);
}






static void set_dma_page(unsigned int dmanr, char pagenr)
{
        switch(dmanr) {
                case 0:
                        outb(pagenr, 0x87);
                        break;
                case 1:
                        outb(pagenr, 0x83);
                        break;
                case 2:
                        outb(pagenr, 0x81);
                        break;
                case 3:
                        outb(pagenr, 0x82);
                        break;
                case 5:
                        outb(pagenr & 0xfe, 0x8B);
                        break;
                case 6:
                        outb(pagenr & 0xfe, 0x89);
                        break;
                case 7:
                        outb(pagenr & 0xfe, 0x8A);
                        break;
        }
}





static void set_dma_addr(unsigned int dmanr, unsigned int a)
{
        set_dma_page(dmanr, a>>16);
        if (dmanr <= 3) {
            outb( a & 0xff, ((dmanr&3)<<1) + 0x00 );
            outb( (a>>8) & 0xff, ((dmanr&3)<<1) + 0x00 );
        } else {
            outb( (a>>1) & 0xff, ((dmanr&3)<<2) + 0xC0 );
            outb( (a>>9) & 0xff, ((dmanr&3)<<2) + 0xC0 );
        }
}
static void set_dma_count(unsigned int dmanr, unsigned int count)
{
        count--;
        if (dmanr <= 3) {
            outb( count & 0xff, ((dmanr&3)<<1) + 1 + 0x00 );
            outb( (count>>8) & 0xff, ((dmanr&3)<<1) + 1 + 0x00 );
        } else {
            outb( (count>>1) & 0xff, ((dmanr&3)<<2) + 2 + 0xC0 );
            outb( (count>>9) & 0xff, ((dmanr&3)<<2) + 2 + 0xC0 );
        }
}
static int get_dma_residue(unsigned int dmanr)
{
        unsigned int io_port = (dmanr<=3)? ((dmanr&3)<<1) + 1 + 0x00
                                         : ((dmanr&3)<<2) + 2 + 0xC0;


        unsigned short count;

        count = 1 + inb(io_port);
        count += inb(io_port) << 8;

        return (dmanr<=3)? count : (count<<1);
}



extern int request_dma(unsigned int dmanr, const char * device_id);
extern void free_dma(unsigned int dmanr);




extern int isa_dma_bridge_buggy;




struct slcp {
        u16 state;
        u32 magic;
        u_char echoid;
        u_char confid;
};

struct sipcp {
        u16 state;
        u_char confid;
};

struct sppp
{
        struct sppp * pp_next;
        u32 pp_flags;
        u16 pp_alivecnt;
        u16 pp_loopcnt;
        u32 pp_seq;
        u32 pp_rseq;
        struct slcp lcp;
        struct sipcp ipcp;
        u32 ibytes,obytes;
        u32 ipkts,opkts;
        struct timer_list pp_timer;
        struct net_device *pp_if;
        char pp_link_state;
        spinlock_t lock;
};

struct ppp_device
{
        struct net_device *dev;
        struct sppp sppp;
};

static struct sppp *sppp_of(struct net_device *dev)
{
        struct ppp_device **ppp = dev->priv;
        do { if (__builtin_expect(!!(((*ppp)->dev != dev)!=0), 0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (63), "i" ("include/net/syncppp.h")); } while(0);
        return &(*ppp)->sppp;
}
void sppp_attach (struct ppp_device *pd);
void sppp_detach (struct net_device *dev);
void sppp_input (struct net_device *dev, struct sk_buff *m);
int sppp_do_ioctl (struct net_device *dev, struct ifreq *ifr, int cmd);
struct sk_buff *sppp_dequeue (struct net_device *dev);
int sppp_isempty (struct net_device *dev);
void sppp_flush (struct net_device *dev);
int sppp_open (struct net_device *dev);
int sppp_reopen (struct net_device *dev);
int sppp_close (struct net_device *dev);



typedef struct {
        u8 address;
        u8 control;
        u16 protocol;
}__attribute__ ((packed)) hdlc_header;



typedef struct {
        u32 type;
        u32 par1;
        u32 par2;
        u16 rel;
        u32 time;
}__attribute__ ((packed)) cisco_packet;





typedef struct pvc_device_struct {
        struct net_device *master;
        struct net_device *main;
        struct net_device *ether;
        struct pvc_device_struct *next;
        int dlci;
        int open_count;

        struct {
                unsigned int new: 1;
                unsigned int active: 1;
                unsigned int exist: 1;
                unsigned int deleted: 1;
                unsigned int fecn: 1;
                unsigned int becn: 1;
                unsigned int bandwidth;
        }state;
}pvc_device;



typedef struct hdlc_device_struct {

        struct net_device_stats stats;


        int (*attach)(struct net_device *dev,
                      unsigned short encoding, unsigned short parity);


        int (*xmit)(struct sk_buff *skb, struct net_device *dev);



        struct {
                int (*open)(struct net_device *dev);
                void (*close)(struct net_device *dev);


                void (*start)(struct net_device *dev);

                void (*stop)(struct net_device *dev);

                void (*detach)(struct hdlc_device_struct *hdlc);
                int (*netif_rx)(struct sk_buff *skb);
                unsigned short (*type_trans)(struct sk_buff *skb,
                                             struct net_device *dev);
                int id;
        }proto;

        int carrier;
        int open;
        spinlock_t state_lock;

        union {
                struct {
                        fr_proto settings;
                        pvc_device *first_pvc;
                        int dce_pvc_count;

                        struct timer_list timer;
                        unsigned long last_poll;
                        int reliable;
                        int dce_changed;
                        int request;
                        int fullrep_sent;
                        u32 last_errors;
                        u8 n391cnt;
                        u8 txseq;
                        u8 rxseq;
                }fr;

                struct {
                        cisco_proto settings;

                        struct timer_list timer;
                        unsigned long last_poll;
                        int up;
                        int request_sent;
                        u32 txseq;
                        u32 rxseq;
                }cisco;

                struct {
                        raw_hdlc_proto settings;
                }raw_hdlc;

                struct {
                        struct ppp_device pppdev;
                        struct ppp_device *syncppp_ptr;
                        int (*old_change_mtu)(struct net_device *dev,
                                              int new_mtu);
                }ppp;
        }state;
        void *priv;
}hdlc_device;



int hdlc_raw_ioctl(struct net_device *dev, struct ifreq *ifr);
int hdlc_raw_eth_ioctl(struct net_device *dev, struct ifreq *ifr);
int hdlc_cisco_ioctl(struct net_device *dev, struct ifreq *ifr);
int hdlc_ppp_ioctl(struct net_device *dev, struct ifreq *ifr);
int hdlc_fr_ioctl(struct net_device *dev, struct ifreq *ifr);
int hdlc_x25_ioctl(struct net_device *dev, struct ifreq *ifr);





int hdlc_ioctl(struct net_device *dev, struct ifreq *ifr, int cmd);


int register_hdlc_device(struct net_device *dev);
void unregister_hdlc_device(struct net_device *dev);

struct net_device *alloc_hdlcdev(void *priv);

static hdlc_device* dev_to_hdlc(struct net_device *dev)
{
        return netdev_priv(dev);
}


static pvc_device* dev_to_pvc(struct net_device *dev)
{
        return (pvc_device*)dev->priv;
}


static void debug_frame(const struct sk_buff *skb)
{
        int i;

        for (i=0; i < skb->len; i++) {
                if (i == 100) {
                        printk("...\n");
                        return;
                }
                printk(" %02X", skb->data[i]);
        }
        printk("\n");
}



int hdlc_open(struct net_device *dev);

void hdlc_close(struct net_device *dev);

void hdlc_set_carrier(int on, struct net_device *dev);


static void hdlc_proto_detach(hdlc_device *hdlc)
{
        if (hdlc->proto.detach)
                hdlc->proto.detach(hdlc);
        hdlc->proto.detach = ((void *)0);
}


static struct net_device_stats *hdlc_stats(struct net_device *dev)
{
        return &dev_to_hdlc(dev)->stats;
}


static __be16 hdlc_type_trans(struct sk_buff *skb,
                                         struct net_device *dev)
{
        hdlc_device *hdlc = dev_to_hdlc(dev);

        skb->mac.raw = skb->data;
        skb->dev = dev;

        if (hdlc->proto.type_trans)
                return hdlc->proto.type_trans(skb, dev);
        else
                return htons(0x0019);
}
typedef struct _MGSL_PARAMS
{


        unsigned long mode;
        unsigned char loopback;



        unsigned short flags;
        unsigned char encoding;
        unsigned long clock_speed;
        unsigned char addr_filter;
        unsigned short crc_type;
        unsigned char preamble_length;
        unsigned char preamble;



        unsigned long data_rate;
        unsigned char data_bits;
        unsigned char stop_bits;
        unsigned char parity;

} MGSL_PARAMS, *PMGSL_PARAMS;
struct mgsl_icount {
        __u32 cts, dsr, rng, dcd, tx, rx;
        __u32 frame, parity, overrun, brk;
        __u32 buf_overrun;
        __u32 txok;
        __u32 txunder;
        __u32 txabort;
        __u32 txtimeout;
        __u32 rxshort;
        __u32 rxlong;
        __u32 rxabort;
        __u32 rxover;
        __u32 rxcrc;
        __u32 rxok;
        __u32 exithunt;
        __u32 rxidle;
};



static MGSL_PARAMS default_params = {
        2,
        0,
        0x0001,
        3,
        0,
        0xff,
        1,
        0,
        0,
        9600,
        8,
        1,
        0
};






typedef struct _DMABUFFERENTRY
{
        u32 phys_addr;
        volatile u16 count;
        volatile u16 status;
        volatile u16 rcc;
        u16 reserved;
        u32 link;
        char *virt_addr;
        u32 phys_entry;
} DMABUFFERENTRY, *DMAPBUFFERENTRY;
struct _input_signal_events {
        int ri_up;
        int ri_down;
        int dsr_up;
        int dsr_down;
        int dcd_up;
        int dcd_down;
        int cts_up;
        int cts_down;
};



struct tx_holding_buffer {
        int buffer_size;
        unsigned char * buffer;
};






struct mgsl_struct {
        int magic;
        int flags;
        int count;
        int line;
        int hw_version;
        unsigned short close_delay;
        unsigned short closing_wait;

        struct mgsl_icount icount;

        struct tty_struct *tty;
        int timeout;
        int x_char;
        int blocked_open;
        u16 read_status_mask;
        u16 ignore_status_mask;
        unsigned char *xmit_buf;
        int xmit_head;
        int xmit_tail;
        int xmit_cnt;

        wait_queue_head_t open_wait;
        wait_queue_head_t close_wait;

        wait_queue_head_t status_event_wait_q;
        wait_queue_head_t event_wait_q;
        struct timer_list tx_timer;
        struct mgsl_struct *next_device;

        spinlock_t irq_spinlock;
        struct work_struct task;

        u32 EventMask;
        u32 RecordedEvents;

        u32 max_frame_size;

        u32 pending_bh;

        int bh_running;
        int isr_overflow;
        int bh_requested;

        int dcd_chkcount;
        int cts_chkcount;
        int dsr_chkcount;
        int ri_chkcount;

        char *buffer_list;
        unsigned long buffer_list_phys;

        unsigned int rx_buffer_count;
        DMABUFFERENTRY *rx_buffer_list;
        unsigned int current_rx_buffer;

        int num_tx_dma_buffers;
        int tx_dma_buffers_used;
        unsigned int tx_buffer_count;
        DMABUFFERENTRY *tx_buffer_list;
        int start_tx_dma_buffer;
        int current_tx_buffer;

        unsigned char *intermediate_rxbuffer;

        int num_tx_holding_buffers;
        int get_tx_holding_index;
        int put_tx_holding_index;
        int tx_holding_count;
        struct tx_holding_buffer tx_holding_buffers[5];

        int rx_enabled;
        int rx_overflow;
        int rx_rcc_underrun;

        int tx_enabled;
        int tx_active;
        u32 idle_mode;

        u16 cmr_value;
        u16 tcsr_value;

        char device_name[25];

        unsigned int bus_type;
        unsigned char bus;
        unsigned char function;

        unsigned int io_base;
        unsigned int io_addr_size;
        int io_addr_requested;

        unsigned int irq_level;
        unsigned long irq_flags;
        int irq_requested;

        unsigned int dma_level;
        int dma_requested;

        u16 mbre_bit;
        u16 loopback_bits;
        u16 usc_idle_mode;

        MGSL_PARAMS params;

        unsigned char serial_signals;

        int irq_occurred;
        unsigned int init_error;
        int fDiagnosticsmode;

        u32 last_mem_alloc;
        unsigned char* memory_base;
        u32 phys_memory_base;
        int shared_mem_requested;

        unsigned char* lcr_base;
        u32 phys_lcr_base;
        u32 lcr_offset;
        int lcr_mem_requested;

        u32 misc_ctrl_value;
        char flag_buf[4096];
        char char_buf[4096];
        int drop_rts_on_tx_done;

        int loopmode_insert_requested;
        int loopmode_send_done_requested;

        struct _input_signal_events input_signal_events;


        int netcount;
        int dosyncppp;
        spinlock_t netlock;


        struct net_device *netdev;

};
void usc_DisableMasterIrqBit( struct mgsl_struct *info );
void usc_EnableMasterIrqBit( struct mgsl_struct *info );
void usc_EnableInterrupts( struct mgsl_struct *info, u16 IrqMask );
void usc_DisableInterrupts( struct mgsl_struct *info, u16 IrqMask );
void usc_ClearIrqPendingBits( struct mgsl_struct *info, u16 IrqMask );
static u16 usc_InDmaReg( struct mgsl_struct *info, u16 Port );
static void usc_OutDmaReg( struct mgsl_struct *info, u16 Port, u16 Value );
static void usc_DmaCmd( struct mgsl_struct *info, u16 Cmd );

static u16 usc_InReg( struct mgsl_struct *info, u16 Port );
static void usc_OutReg( struct mgsl_struct *info, u16 Port, u16 Value );
static void usc_RTCmd( struct mgsl_struct *info, u16 Cmd );
void usc_RCmd( struct mgsl_struct *info, u16 Cmd );
void usc_TCmd( struct mgsl_struct *info, u16 Cmd );






static void usc_process_rxoverrun_sync( struct mgsl_struct *info );
static void usc_start_receiver( struct mgsl_struct *info );
static void usc_stop_receiver( struct mgsl_struct *info );

static void usc_start_transmitter( struct mgsl_struct *info );
static void usc_stop_transmitter( struct mgsl_struct *info );
static void usc_set_txidle( struct mgsl_struct *info );
static void usc_load_txfifo( struct mgsl_struct *info );

static void usc_enable_aux_clock( struct mgsl_struct *info, u32 DataRate );
static void usc_enable_loopback( struct mgsl_struct *info, int enable );

static void usc_get_serial_signals( struct mgsl_struct *info );
static void usc_set_serial_signals( struct mgsl_struct *info );

static void usc_reset( struct mgsl_struct *info );

static void usc_set_sync_mode( struct mgsl_struct *info );
static void usc_set_sdlc_mode( struct mgsl_struct *info );
static void usc_set_async_mode( struct mgsl_struct *info );
static void usc_enable_async_clock( struct mgsl_struct *info, u32 DataRate );

static void usc_loopback_frame( struct mgsl_struct *info );

static void mgsl_tx_timeout(unsigned long context);


static void usc_loopmode_cancel_transmit( struct mgsl_struct * info );
static void usc_loopmode_insert_request( struct mgsl_struct * info );
static int usc_loopmode_active( struct mgsl_struct * info);
static void usc_loopmode_send_done( struct mgsl_struct * info );

static int mgsl_ioctl_common(struct mgsl_struct *info, unsigned int cmd, unsigned long arg);



static void hdlcdev_tx_done(struct mgsl_struct *info);
static void hdlcdev_rx(struct mgsl_struct *info, char *buf, int size);
static int hdlcdev_init(struct mgsl_struct *info);
static void hdlcdev_exit(struct mgsl_struct *info);
static void mgsl_trace_block(struct mgsl_struct *info,const char* data, int count, int xmit);




static int mgsl_register_test( struct mgsl_struct *info );
static int mgsl_irq_test( struct mgsl_struct *info );
static int mgsl_dma_test( struct mgsl_struct *info );
static int mgsl_memory_test( struct mgsl_struct *info );
static int mgsl_adapter_test( struct mgsl_struct *info );




static int mgsl_claim_resources(struct mgsl_struct *info);
static void mgsl_release_resources(struct mgsl_struct *info);
static void mgsl_add_device(struct mgsl_struct *info);
static struct mgsl_struct* mgsl_allocate_device(void);




static void mgsl_free_rx_frame_buffers( struct mgsl_struct *info, unsigned int StartIndex, unsigned int EndIndex );
static int mgsl_get_rx_frame( struct mgsl_struct *info );
static int mgsl_get_raw_rx_frame( struct mgsl_struct *info );
static void mgsl_reset_rx_dma_buffers( struct mgsl_struct *info );
static void mgsl_reset_tx_dma_buffers( struct mgsl_struct *info );
static int num_free_tx_dma_buffers(struct mgsl_struct *info);
static void mgsl_load_tx_dma_buffer( struct mgsl_struct *info, const char *Buffer, unsigned int BufferSize);
static void mgsl_load_pci_memory(char* TargetPtr, const char* SourcePtr, unsigned short count);




static int mgsl_allocate_dma_buffers(struct mgsl_struct *info);
static void mgsl_free_dma_buffers(struct mgsl_struct *info);
static int mgsl_alloc_frame_memory(struct mgsl_struct *info, DMABUFFERENTRY *BufferList,int Buffercount);
static void mgsl_free_frame_memory(struct mgsl_struct *info, DMABUFFERENTRY *BufferList,int Buffercount);
static int mgsl_alloc_buffer_list_memory(struct mgsl_struct *info);
static void mgsl_free_buffer_list_memory(struct mgsl_struct *info);
static int mgsl_alloc_intermediate_rxbuffer_memory(struct mgsl_struct *info);
static void mgsl_free_intermediate_rxbuffer_memory(struct mgsl_struct *info);
static int mgsl_alloc_intermediate_txbuffer_memory(struct mgsl_struct *info);
static void mgsl_free_intermediate_txbuffer_memory(struct mgsl_struct *info);
static int load_next_tx_holding_buffer(struct mgsl_struct *info);
static int save_tx_buffer_request(struct mgsl_struct *info,const char *Buffer, unsigned int BufferSize);




static void mgsl_bh_handler(void* Context);
static void mgsl_bh_receive(struct mgsl_struct *info);
static void mgsl_bh_transmit(struct mgsl_struct *info);
static void mgsl_bh_status(struct mgsl_struct *info);




static void mgsl_isr_null( struct mgsl_struct *info );
static void mgsl_isr_transmit_data( struct mgsl_struct *info );
static void mgsl_isr_receive_data( struct mgsl_struct *info );
static void mgsl_isr_receive_status( struct mgsl_struct *info );
static void mgsl_isr_transmit_status( struct mgsl_struct *info );
static void mgsl_isr_io_pin( struct mgsl_struct *info );
static void mgsl_isr_misc( struct mgsl_struct *info );
static void mgsl_isr_receive_dma( struct mgsl_struct *info );
static void mgsl_isr_transmit_dma( struct mgsl_struct *info );

typedef void (*isr_dispatch_func)(struct mgsl_struct *);

static isr_dispatch_func UscIsrTable[7] =
{
        mgsl_isr_null,
        mgsl_isr_misc,
        mgsl_isr_io_pin,
        mgsl_isr_transmit_data,
        mgsl_isr_transmit_status,
        mgsl_isr_receive_data,
        mgsl_isr_receive_status
};




static int tiocmget(struct tty_struct *tty, struct file *file);
static int tiocmset(struct tty_struct *tty, struct file *file,
                    unsigned int set, unsigned int clear);
static int mgsl_get_stats(struct mgsl_struct * info, struct mgsl_icount
        *user_icount);
static int mgsl_get_params(struct mgsl_struct * info, MGSL_PARAMS *user_params);
static int mgsl_set_params(struct mgsl_struct * info, MGSL_PARAMS *new_params);
static int mgsl_get_txidle(struct mgsl_struct * info, int *idle_mode);
static int mgsl_set_txidle(struct mgsl_struct * info, int idle_mode);
static int mgsl_txenable(struct mgsl_struct * info, int enable);
static int mgsl_txabort(struct mgsl_struct * info);
static int mgsl_rxenable(struct mgsl_struct * info, int enable);
static int mgsl_wait_event(struct mgsl_struct * info, int *mask);
static int mgsl_loopmode_send_done( struct mgsl_struct * info );


static int pci_registered;




static struct mgsl_struct *mgsl_device_list;
static int mgsl_device_count;






static int break_on_load;





static int ttymajor;




static int io[10];
static int irq[10];
static int dma[10];
static int debug_level;
static int maxframe[20];
static int dosyncppp[20];
static int txdmabufs[20];
static int txholdbufs[20];

static int *__check_break_on_load(void) { return(&(break_on_load)); }; static char __param_str_break_on_load[] = "tty_io" "." "break_on_load"; static struct kernel_param const __param_break_on_load __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_break_on_load, 0, param_set_bool, param_get_bool, &break_on_load }; ;
static int *__check_ttymajor(void) { return(&(ttymajor)); }; static char __param_str_ttymajor[] = "tty_io" "." "ttymajor"; static struct kernel_param const __param_ttymajor __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_ttymajor, 0, param_set_int, param_get_int, &ttymajor }; ;
static struct kparam_array __param_arr_io = { (sizeof(io) / sizeof((io)[0])), ((void *)0), param_set_int, param_get_int, sizeof(io[0]), io }; static char __param_str_io[] = "tty_io" "." "io"; static struct kernel_param const __param_io __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_io, 0, param_array_set, param_array_get, &__param_arr_io }; ;
static struct kparam_array __param_arr_irq = { (sizeof(irq) / sizeof((irq)[0])), ((void *)0), param_set_int, param_get_int, sizeof(irq[0]), irq }; static char __param_str_irq[] = "tty_io" "." "irq"; static struct kernel_param const __param_irq __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_irq, 0, param_array_set, param_array_get, &__param_arr_irq }; ;
static struct kparam_array __param_arr_dma = { (sizeof(dma) / sizeof((dma)[0])), ((void *)0), param_set_int, param_get_int, sizeof(dma[0]), dma }; static char __param_str_dma[] = "tty_io" "." "dma"; static struct kernel_param const __param_dma __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_dma, 0, param_array_set, param_array_get, &__param_arr_dma }; ;
static int *__check_debug_level(void) { return(&(debug_level)); }; static char __param_str_debug_level[] = "tty_io" "." "debug_level"; static struct kernel_param const __param_debug_level __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_debug_level, 0, param_set_int, param_get_int, &debug_level }; ;
static struct kparam_array __param_arr_maxframe = { (sizeof(maxframe) / sizeof((maxframe)[0])), ((void *)0), param_set_int, param_get_int, sizeof(maxframe[0]), maxframe }; static char __param_str_maxframe[] = "tty_io" "." "maxframe"; static struct kernel_param const __param_maxframe __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_maxframe, 0, param_array_set, param_array_get, &__param_arr_maxframe }; ;
static struct kparam_array __param_arr_dosyncppp = { (sizeof(dosyncppp) / sizeof((dosyncppp)[0])), ((void *)0), param_set_int, param_get_int, sizeof(dosyncppp[0]), dosyncppp }; static char __param_str_dosyncppp[] = "tty_io" "." "dosyncppp"; static struct kernel_param const __param_dosyncppp __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_dosyncppp, 0, param_array_set, param_array_get, &__param_arr_dosyncppp }; ;
static struct kparam_array __param_arr_txdmabufs = { (sizeof(txdmabufs) / sizeof((txdmabufs)[0])), ((void *)0), param_set_int, param_get_int, sizeof(txdmabufs[0]), txdmabufs }; static char __param_str_txdmabufs[] = "tty_io" "." "txdmabufs"; static struct kernel_param const __param_txdmabufs __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_txdmabufs, 0, param_array_set, param_array_get, &__param_arr_txdmabufs }; ;
static struct kparam_array __param_arr_txholdbufs = { (sizeof(txholdbufs) / sizeof((txholdbufs)[0])), ((void *)0), param_set_int, param_get_int, sizeof(txholdbufs[0]), txholdbufs }; static char __param_str_txholdbufs[] = "tty_io" "." "txholdbufs"; static struct kernel_param const __param_txholdbufs __attribute__((__unused__)) __attribute__ ((unused,__section__ ("__param"),aligned(sizeof(void *)))) = { __param_str_txholdbufs, 0, param_array_set, param_array_get, &__param_arr_txholdbufs }; ;

static char *driver_name = "SyncLink serial driver";
static char *driver_version = "$Revision: 4.37 $";

static int synclink_init_one (struct pci_dev *dev,
                                     const struct pci_device_id *ent);
static void synclink_remove_one (struct pci_dev *dev);

static struct pci_device_id synclink_pci_tbl[] = {
        { 0x13c0, 0x0010, (~0), (~0), },
        { 0x13c0, 0x0210, (~0), (~0), },
        { 0, },
};
;

;

static struct pci_driver synclink_pci_driver = {
        .name = "synclink",
        .id_table = synclink_pci_tbl,
        .probe = synclink_init_one,
        .remove = synclink_remove_one,
};

static struct tty_driver *serial_driver;





static void mgsl_change_params(struct mgsl_struct *info);
static void mgsl_wait_until_sent(struct tty_struct *tty, int timeout);







static void* mgsl_get_text_ptr(void)
{
        return mgsl_get_text_ptr;
}
static unsigned char *tmp_buf;
static struct semaphore tmp_buf_sem = { .count = { (1) }, .sleepers = 0, .wait = { .lock = (spinlock_t) { .raw_lock = { 1 } }, .task_list = { &((tmp_buf_sem).wait).task_list, &((tmp_buf_sem).wait).task_list } } };

static int mgsl_paranoia_check(struct mgsl_struct *info,
                                        char *name, const char *routine)
{
        if (!info)
                return 1;

        return 0;
}
static void ldisc_receive_buf(struct tty_struct *tty,
                              const __u8 *data, char *flags, int count)
{
        struct tty_ldisc *ld;
        if (!tty)
                return;
        ld = tty_ldisc_ref(tty);
        if (ld) {
                if (ld->receive_buf)
                        ld->receive_buf(tty, data, flags, count);
                tty_ldisc_deref(ld);
        }
}






static void mgsl_stop(struct tty_struct *tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (mgsl_paranoia_check(info, tty->name, "mgsl_stop"))
                return;

        if ( debug_level >= 3 )
                printk("mgsl_stop(%s)\n",info->device_name);

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if (info->tx_enabled)
                usc_stop_transmitter(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

}






static void mgsl_start(struct tty_struct *tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (mgsl_paranoia_check(info, tty->name, "mgsl_start"))
                return;

        if ( debug_level >= 3 )
                printk("mgsl_start(%s)\n",info->device_name);

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if (!info->tx_enabled)
                usc_start_transmitter(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

}
static int mgsl_bh_action(struct mgsl_struct *info)
{
        unsigned long flags;
        int rc = 0;

        flags = _spin_lock_irqsave(&info->irq_spinlock);

        if (info->pending_bh & 1) {
                info->pending_bh &= ~1;
                rc = 1;
        } else if (info->pending_bh & 2) {
                info->pending_bh &= ~2;
                rc = 2;
        } else if (info->pending_bh & 4) {
                info->pending_bh &= ~4;
                rc = 4;
        }

        if (!rc) {

                info->bh_running = 0;
                info->bh_requested = 0;
        }

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        return rc;
}




static void mgsl_bh_handler(void* Context)
{
        struct mgsl_struct *info = (struct mgsl_struct*)Context;
        int action;

        if (!info)
                return;

        if ( debug_level >= 4 )
                printk( "%s(%d):mgsl_bh_handler(%s) entry\n",
                        "synclink.c",1095,info->device_name);

        info->bh_running = 1;

        while((action = mgsl_bh_action(info)) != 0) {


                if ( debug_level >= 4 )
                        printk( "%s(%d):mgsl_bh_handler() work item action=%d\n",
                                "synclink.c",1104,action);

                switch (action) {

                case 1:
                        mgsl_bh_receive(info);
                        break;
                case 2:
                        mgsl_bh_transmit(info);
                        break;
                case 4:
                        mgsl_bh_status(info);
                        break;
                default:

                        printk("Unknown work item ID=%08X!\n", action);
                        break;
                }
        }

        if ( debug_level >= 4 )
                printk( "%s(%d):mgsl_bh_handler(%s) exit\n",
                        "synclink.c",1126,info->device_name);
}

static void mgsl_bh_receive(struct mgsl_struct *info)
{
        int (*get_rx_frame)(struct mgsl_struct *info) =
                (info->params.mode == 2 ? mgsl_get_rx_frame : mgsl_get_raw_rx_frame);

        if ( debug_level >= 4 )
                printk( "%s(%d):mgsl_bh_receive(%s)\n",
                        "synclink.c",1136,info->device_name);

        do
        {
                if (info->rx_rcc_underrun) {
                        unsigned long flags;
                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        usc_start_receiver(info);
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                        return;
                }
        } while(get_rx_frame(info));
}

static void mgsl_bh_transmit(struct mgsl_struct *info)
{
        struct tty_struct *tty = info->tty;
        unsigned long flags;

        if ( debug_level >= 4 )
                printk( "%s(%d):mgsl_bh_transmit() entry on %s\n",
                        "synclink.c",1157,info->device_name);

        if (tty) {
                tty_wakeup(tty);
                __wake_up(&tty->write_wait, 1, 1, ((void *)0));
        }




        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if ( !info->tx_active && info->loopmode_send_done_requested )
                usc_loopmode_send_done( info );
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
}

static void mgsl_bh_status(struct mgsl_struct *info)
{
        if ( debug_level >= 4 )
                printk( "%s(%d):mgsl_bh_status() entry on %s\n",
                        "synclink.c",1177,info->device_name);

        info->ri_chkcount = 0;
        info->dsr_chkcount = 0;
        info->dcd_chkcount = 0;
        info->cts_chkcount = 0;
}
static void mgsl_isr_receive_status( struct mgsl_struct *info )
{
        u16 status = usc_InReg( info, 0x24 );

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_receive_status status=%04X\n",
                        "synclink.c",1200,status);

        if ( (status & 0x0020) &&
                info->loopmode_insert_requested &&
                usc_loopmode_active(info) )
        {
                ++info->icount.rxabort;
                info->loopmode_insert_requested = 0;


                info->cmr_value &= ~0x2000;
                usc_OutReg(info, 0x02, info->cmr_value);


                usc_OutReg(info, 0x26,
                        (usc_InReg(info, 0x26) & ~0x0020));
        }

        if (status & (0x0080 + 0x0040)) {
                if (status & 0x0080)
                        info->icount.exithunt++;
                if (status & 0x0040)
                        info->icount.rxidle++;
                __wake_up(&info->event_wait_q, 1, 1, ((void *)0));
        }

        if (status & 0x0002){
                info->icount.rxover++;
                usc_process_rxoverrun_sync( info );
        }

        usc_OutReg( (info), 0x1a, 0x40 + (0x0020) );
        usc_OutReg( (info), 0x24, (u16)((status) & 0x01f6) );

}
static void mgsl_isr_transmit_status( struct mgsl_struct *info )
{
        u16 status = usc_InReg( info, 0x34 );

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_transmit_status status=%04X\n",
                        "synclink.c",1252,status);

        usc_OutReg( (info), 0x1a, 0x40 + (0x0008) );
        usc_OutReg( (info), 0x34, (u16)((info)->tcsr_value + ((status) & 0x00FF)) );

        if ( status & (0x0002 | 0x0020) )
        {





                usc_DmaCmd( info, 0x1000 );
                usc_RTCmd( info, 0x5000 );
        }

        if ( status & 0x0010 )
                info->icount.txok++;
        else if ( status & 0x0002 )
                info->icount.txunder++;
        else if ( status & 0x0020 )
                info->icount.txabort++;
        else
                info->icount.txunder++;

        info->tx_active = 0;
        info->xmit_cnt = info->xmit_head = info->xmit_tail = 0;
        del_timer(&info->tx_timer);

        if ( info->drop_rts_on_tx_done ) {
                usc_get_serial_signals( info );
                if ( info->serial_signals & 0x20 ) {
                        info->serial_signals &= ~0x20;
                        usc_set_serial_signals( info );
                }
                info->drop_rts_on_tx_done = 0;
        }


        if (info->netcount)
                hdlcdev_tx_done(info);
        else

        {
                if (info->tty->stopped || info->tty->hw_stopped) {
                        usc_stop_transmitter(info);
                        return;
                }
                info->pending_bh |= 2;
        }

}
static void mgsl_isr_io_pin( struct mgsl_struct *info )
{
        struct mgsl_icount *icount;
        u16 status = usc_InReg( info, 0x1c );

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_io_pin status=%04X\n",
                        "synclink.c",1320,status);

        usc_OutReg( (info), 0x1a, 0x40 + (0x0002) );
        usc_OutReg((info),0x1c,(u16)((status) & 0xaaa0));

        if (status & (0x0020 | 0x0080 |
                      0x0200 | 0x0800) ) {
                icount = &info->icount;

                if (status & 0x0800) {
                        if ((info->ri_chkcount)++ >= 100)
                                usc_OutReg( (info), 0x1e, (u16)(usc_InReg((info),0x1e) & ~((0x0800 +0x0400))) );
                        icount->rng++;
                        if ( status & 0x0400 )
                                info->input_signal_events.ri_up++;
                        else
                                info->input_signal_events.ri_down++;
                }
                if (status & 0x0200) {
                        if ((info->dsr_chkcount)++ >= 100)
                                usc_OutReg( (info), 0x1e, (u16)(usc_InReg((info),0x1e) & ~((0x0200 +0x0100))) );
                        icount->dsr++;
                        if ( status & 0x0100 )
                                info->input_signal_events.dsr_up++;
                        else
                                info->input_signal_events.dsr_down++;
                }
                if (status & 0x0080) {
                        if ((info->dcd_chkcount)++ >= 100)
                                usc_OutReg( (info), 0x1e, (u16)(usc_InReg((info),0x1e) & ~((0x0080 +0x0040))) );
                        icount->dcd++;
                        if (status & 0x0040) {
                                info->input_signal_events.dcd_up++;
                        } else
                                info->input_signal_events.dcd_down++;

                        if (info->netcount)
                                hdlc_set_carrier(status & 0x0040, info->netdev);

                }
                if (status & 0x0020)
                {
                        if ((info->cts_chkcount)++ >= 100)
                                usc_OutReg( (info), 0x1e, (u16)(usc_InReg((info),0x1e) & ~((0x0020 +0x0010))) );
                        icount->cts++;
                        if ( status & 0x0010 )
                                info->input_signal_events.cts_up++;
                        else
                                info->input_signal_events.cts_down++;
                }
                __wake_up(&info->status_event_wait_q, 1, 1, ((void *)0));
                __wake_up(&info->event_wait_q, 1, 1, ((void *)0));

                if ( (info->flags & 0x02000000) &&
                     (status & 0x0080) ) {
                        if ( debug_level >= 5 )
                                printk("%s CD now %s...", info->device_name,
                                       (status & 0x0040) ? "on" : "off");
                        if (status & 0x0040)
                                __wake_up(&info->open_wait, 1, 1, ((void *)0));
                        else {
                                if ( debug_level >= 5 )
                                        printk("doing serial hangup...");
                                if (info->tty)
                                        tty_hangup(info->tty);
                        }
                }

                if ( (info->flags & 0x04000000) &&
                     (status & 0x0020) ) {
                        if (info->tty->hw_stopped) {
                                if (status & 0x0010) {
                                        if ( debug_level >= 5 )
                                                printk("CTS tx start...");
                                        if (info->tty)
                                                info->tty->hw_stopped = 0;
                                        usc_start_transmitter(info);
                                        info->pending_bh |= 2;
                                        return;
                                }
                        } else {
                                if (!(status & 0x0010)) {
                                        if ( debug_level >= 5 )
                                                printk("CTS tx stop...");
                                        if (info->tty)
                                                info->tty->hw_stopped = 1;
                                        usc_stop_transmitter(info);
                                }
                        }
                }
        }

        info->pending_bh |= 4;


        if ( status & 0x2000 ){
                usc_OutReg( info, 0x1e,
                        (unsigned short)(usc_InReg(info,0x1e) & ~(0x2000 +0x1000)) );
                usc_OutReg((info),0x1c,(u16)((0x2000) & 0xaaa0));
                info->irq_occurred = 1;
        }

}
static void mgsl_isr_transmit_data( struct mgsl_struct *info )
{
        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_transmit_data xmit_cnt=%d\n",
                        "synclink.c",1435,info->xmit_cnt);

        usc_OutReg( (info), 0x1a, 0x40 + (0x0004) );

        if (info->tty->stopped || info->tty->hw_stopped) {
                usc_stop_transmitter(info);
                return;
        }

        if ( info->xmit_cnt )
                usc_load_txfifo( info );
        else
                info->tx_active = 0;

        if (info->xmit_cnt < 256)
                info->pending_bh |= 2;

}
static void mgsl_isr_receive_data( struct mgsl_struct *info )
{
        int Fifocount;
        u16 status;
        unsigned char DataByte;
        struct tty_struct *tty = info->tty;
        struct mgsl_icount *icount = &info->icount;

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_receive_data\n",
                        "synclink.c",1473);

        usc_OutReg( (info), 0x1a, 0x40 + (0x0010) );


        usc_OutReg((info), 0x24, (0x5000));



        usc_OutReg( info, 0x26 +0x40, (u16)(usc_InReg(info, 0x26 +0x40) & ~0x0008 ));



        while( (Fifocount = (usc_InReg(info,0x26) >> 8)) ) {

                outw( (inw(info->io_base + 4) & 0x0780) | (0x20 +0x40),
                      info->io_base + 4 );
                DataByte = inb( info->io_base + 4 );


                status = usc_InReg(info, 0x24);
                if ( status & (0x0008 + 0x0004 +
                                0x0002 + 0x0020) )
                        usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );

                if (tty->flip.count >= 512)
                        continue;

                *tty->flip.char_buf_ptr = DataByte;
                icount->rx++;

                *tty->flip.flag_buf_ptr = 0;
                if ( status & (0x0008 + 0x0004 +
                                0x0002 + 0x0020) ) {
                        printk("rxerr=%04X\n",status);

                        if ( status & 0x0020 ) {
                                status &= ~(0x0008 + 0x0004);
                                icount->brk++;
                        } else if (status & 0x0004)
                                icount->parity++;
                        else if (status & 0x0008)
                                icount->frame++;
                        else if (status & 0x0002) {


                                usc_RTCmd(info,0x4800);
                                icount->overrun++;
                        }


                        if (status & info->ignore_status_mask)
                                continue;

                        status &= info->read_status_mask;

                        if (status & 0x0020) {
                                *tty->flip.flag_buf_ptr = 1;
                                if (info->flags & 0x0004)
                                        do_SAK(tty);
                        } else if (status & 0x0004)
                                *tty->flip.flag_buf_ptr = 3;
                        else if (status & 0x0008)
                                *tty->flip.flag_buf_ptr = 2;
                        if (status & 0x0002) {




                                if (tty->flip.count < 512) {
                                        tty->flip.count++;
                                        tty->flip.flag_buf_ptr++;
                                        tty->flip.char_buf_ptr++;
                                        *tty->flip.flag_buf_ptr = 4;
                                }
                        }
                }

                tty->flip.flag_buf_ptr++;
                tty->flip.char_buf_ptr++;
                tty->flip.count++;
        }

        if ( debug_level >= 5 ) {
                printk("%s(%d):mgsl_isr_receive_data flip count=%d\n",
                        "synclink.c",1558,tty->flip.count);
                printk("%s(%d):rx=%d brk=%d parity=%d frame=%d overrun=%d\n",
                        "synclink.c",1560,icount->rx,icount->brk,
                        icount->parity,icount->frame,icount->overrun);
        }

        if ( tty->flip.count )
                tty_flip_buffer_push(tty);
}
static void mgsl_isr_misc( struct mgsl_struct *info )
{
        u16 status = usc_InReg( info, 0x1c );

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_misc status=%04X\n",
                        "synclink.c",1581,status);

        if ((status & 0x0008) &&
            (info->params.mode == 2)) {


                usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (0)) );
                usc_DmaCmd(info, 0x1200);
                usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
                usc_OutReg( (info), 0x1a, 0x40 + (0x0010 + 0x0020) );
                usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0x80 + (0x0010 + 0x0020)) );


                info->pending_bh |= 1;
                info->rx_rcc_underrun = 1;
        }

        usc_OutReg( (info), 0x1a, 0x40 + (0x0001) );
        usc_OutReg((info),0x1c,(u16)((status) & 0x000f));

}
static void mgsl_isr_null( struct mgsl_struct *info )
{

}
static void mgsl_isr_receive_dma( struct mgsl_struct *info )
{
        u16 status;


        usc_OutDmaReg( info, 0x1a, 0x0200 +0x0002 );



        status = usc_InDmaReg( info, 0x82 );

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_receive_dma(%s) status=%04X\n",
                        "synclink.c",1648,info->device_name,status);

        info->pending_bh |= 1;

        if ( status & 0x0008 ) {
                info->rx_overflow = 1;
                info->icount.buf_overrun++;
        }

}
static void mgsl_isr_transmit_dma( struct mgsl_struct *info )
{
        u16 status;


        usc_OutDmaReg(info, 0x1a, 0x0100 +0x0001 );




        status = usc_InDmaReg( info, 0x02 );

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_isr_transmit_dma(%s) status=%04X\n",
                        "synclink.c",1693,info->device_name,status);

        if ( status & 0x0004 ) {
                --info->tx_dma_buffers_used;




                if ( load_next_tx_holding_buffer(info) ) {



                        info->pending_bh |= 2;
                }
        }

}
static irqreturn_t mgsl_interrupt(int irq, void *dev_id, struct pt_regs * regs)
{
        struct mgsl_struct * info;
        u16 UscVector;
        u16 DmaVector;

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_interrupt(%d)entry.\n",
                        "synclink.c",1731,irq);

        info = (struct mgsl_struct *)dev_id;
        if (!info)
                return (0);

        _spin_lock(&info->irq_spinlock);

        for(;;) {

                UscVector = usc_InReg(info, 0x14) >> 9;
                DmaVector = usc_InDmaReg(info, 0x14);

                if ( debug_level >= 5 )
                        printk("%s(%d):%s UscVector=%08X DmaVector=%08X\n",
                                "synclink.c",1746,info->device_name,UscVector,DmaVector);

                if ( !UscVector && !DmaVector )
                        break;


                if ( UscVector )
                        (*UscIsrTable[UscVector])(info);
                else if ( (DmaVector&(0x0400|0x0200)) == 0x0400)
                        mgsl_isr_transmit_dma(info);
                else
                        mgsl_isr_receive_dma(info);

                if ( info->isr_overflow ) {
                        printk("<3>""%s(%d):%s isr overflow irq=%d\n",
                                "synclink.c",1761,info->device_name, irq);
                        usc_OutReg( (info), 0x18, (u16)(usc_InReg((info),0x18) & 0x7f00) );
                        usc_OutDmaReg( (info), 0x18, (u16)(usc_InDmaReg((info),0x18) & ~(0x8000)) );
                        break;
                }
        }





        if ( info->pending_bh && !info->bh_running && !info->bh_requested ) {
                if ( debug_level >= 5 )
                        printk("%s(%d):%s queueing bh task.\n",
                                "synclink.c",1775,info->device_name);
                schedule_work(&info->task);
                info->bh_requested = 1;
        }

        _spin_unlock(&info->irq_spinlock);

        if ( debug_level >= 5 )
                printk("%s(%d):mgsl_interrupt(%d)exit.\n",
                        "synclink.c",1784,irq);
        return (1);
}
static int startup(struct mgsl_struct * info)
{
        int retval = 0;

        if ( debug_level >= 3 )
                printk("%s(%d):mgsl_startup(%s)\n","synclink.c",1800,info->device_name);

        if (info->flags & 0x80000000)
                return 0;

        if (!info->xmit_buf) {

                info->xmit_buf = (unsigned char *)get_zeroed_page((0x10u | 0x40u | 0x80u));
                if (!info->xmit_buf) {
                        printk("<3>""%s(%d):%s can't allocate transmit buffer\n",
                                "synclink.c",1810,info->device_name);
                        return -12;
                }
        }

        info->pending_bh = 0;

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(info->icount))) ? __constant_c_and_count_memset(((&info->icount)),((0x01010101UL*(unsigned char)(0))),((sizeof(info->icount)))) : __constant_c_memset(((&info->icount)),((0x01010101UL*(unsigned char)(0))),((sizeof(info->icount))))) : (__builtin_constant_p((sizeof(info->icount))) ? __memset_generic((((&info->icount))),(((0))),(((sizeof(info->icount))))) : __memset_generic(((&info->icount)),((0)),((sizeof(info->icount))))));

        init_timer(&info->tx_timer);
        info->tx_timer.data = (unsigned long)info;
        info->tx_timer.function = mgsl_tx_timeout;


        retval = mgsl_claim_resources(info);


        if ( !retval )
                retval = mgsl_adapter_test(info);

        if ( retval ) {
                if (capable(21) && info->tty)
                        set_bit(1, &info->tty->flags);
                mgsl_release_resources(info);
                return retval;
        }


        mgsl_change_params(info);

        if (info->tty)
                clear_bit(1, &info->tty->flags);

        info->flags |= 0x80000000;

        return 0;

}
static void shutdown(struct mgsl_struct * info)
{
        unsigned long flags;

        if (!(info->flags & 0x80000000))
                return;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_shutdown(%s)\n",
                         "synclink.c",1865, info->device_name );



        __wake_up(&info->status_event_wait_q, 1, 1, ((void *)0));
        __wake_up(&info->event_wait_q, 1, 1, ((void *)0));

        del_timer(&info->tx_timer);

        if (info->xmit_buf) {
                free_pages(((unsigned long) info->xmit_buf),0);
                info->xmit_buf = ((void *)0);
        }

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_OutReg( (info), 0x18, (u16)(usc_InReg((info),0x18) & 0x7f00) );
        usc_stop_receiver(info);
        usc_stop_transmitter(info);
        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0x80 + (0x0010 + 0x0020 + 0x0004 + 0x0008 + 0x0002 + 0x0001)) );

        usc_OutDmaReg( (info), 0x18, (u16)(usc_InDmaReg((info),0x18) & ~(0x8000 + 0x0001 + 0x0002)) );




        usc_OutReg(info, 0x0a, (u16)((usc_InReg(info, 0x0a) | 0x8000) | 0x4000));




        usc_OutReg(info, 0x0a, (u16)((usc_InReg(info, 0x0a) | 0x2000) | 0x1000));

        if (!info->tty || info->tty->termios->c_cflag & 0002000) {
                info->serial_signals &= ~(0x80 + 0x20);
                usc_set_serial_signals(info);
        }

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        mgsl_release_resources(info);

        if (info->tty)
                set_bit(1, &info->tty->flags);

        info->flags &= ~0x80000000;

}

static void mgsl_program_hw(struct mgsl_struct *info)
{
        unsigned long flags;

        flags = _spin_lock_irqsave(&info->irq_spinlock);

        usc_stop_receiver(info);
        usc_stop_transmitter(info);
        info->xmit_cnt = info->xmit_head = info->xmit_tail = 0;

        if (info->params.mode == 2 ||
            info->params.mode == 6 ||
            info->netcount)
                usc_set_sync_mode(info);
        else
                usc_set_async_mode(info);

        usc_set_serial_signals(info);

        info->dcd_chkcount = 0;
        info->cts_chkcount = 0;
        info->ri_chkcount = 0;
        info->dsr_chkcount = 0;

        usc_OutReg( (info), 0x1e, (u16)(usc_InReg((info),0x1e) | ((0x0020 +0x0010)+(0x0200 +0x0100)+(0x0080 +0x0040)+(0x0800 +0x0400))) );
        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0002)) );
        usc_get_serial_signals(info);

        if (info->netcount || info->tty->termios->c_cflag & 0000200)
                usc_start_receiver(info);

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
}



static void mgsl_change_params(struct mgsl_struct *info)
{
        unsigned cflag;
        int bits_per_char;

        if (!info->tty || !info->tty->termios)
                return;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_change_params(%s)\n",
                         "synclink.c",1959, info->device_name );

        cflag = info->tty->termios->c_cflag;



        if (cflag & 0010017)
                info->serial_signals |= 0x20 + 0x80;
        else
                info->serial_signals &= ~(0x20 + 0x80);



        switch (cflag & 0000060) {
              case 0000000: info->params.data_bits = 5; break;
              case 0000020: info->params.data_bits = 6; break;
              case 0000040: info->params.data_bits = 7; break;
              case 0000060: info->params.data_bits = 8; break;

              default: info->params.data_bits = 7; break;
              }

        if (cflag & 0000100)
                info->params.stop_bits = 2;
        else
                info->params.stop_bits = 1;

        info->params.parity = 0;
        if (cflag & 0000400) {
                if (cflag & 0001000)
                        info->params.parity = 2;
                else
                        info->params.parity = 1;

                if (cflag & 010000000000)
                        info->params.parity = 3;

        }




        bits_per_char = info->params.data_bits +
                        info->params.stop_bits + 1;





        if (info->params.data_rate <= 460800)
                info->params.data_rate = tty_get_baud_rate(info->tty);

        if ( info->params.data_rate ) {
                info->timeout = (32*250*bits_per_char) /
                                info->params.data_rate;
        }
        info->timeout += 250/50;

        if (cflag & 020000000000)
                info->flags |= 0x04000000;
        else
                info->flags &= ~0x04000000;

        if (cflag & 0004000)
                info->flags &= ~0x02000000;
        else
                info->flags |= 0x02000000;



        info->read_status_mask = 0x0002;
        if ((((info->tty))->termios->c_iflag & (0000020)))
                info->read_status_mask |= 0x0004 | 0x0008;
        if ((((info->tty))->termios->c_iflag & (0000002)) || (((info->tty))->termios->c_iflag & (0000010)))
                info->read_status_mask |= 0x0020;

        if ((((info->tty))->termios->c_iflag & (0000004)))
                info->ignore_status_mask |= 0x0004 | 0x0008;
        if ((((info->tty))->termios->c_iflag & (0000001))) {
                info->ignore_status_mask |= 0x0020;



                if ((((info->tty))->termios->c_iflag & (0000004)))
                        info->ignore_status_mask |= 0x0002;
        }

        mgsl_program_hw(info);

}
static void mgsl_put_char(struct tty_struct *tty, unsigned char ch)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if ( debug_level >= 3 ) {
                printk( "%s(%d):mgsl_put_char(%d) on %s\n",
                        "synclink.c",2066,ch,info->device_name);
        }

        if (mgsl_paranoia_check(info, tty->name, "mgsl_put_char"))
                return;

        if (!tty || !info->xmit_buf)
                return;

        flags = _spin_lock_irqsave(&info->irq_spinlock);

        if ( (info->params.mode == 1 ) || !info->tx_active ) {

                if (info->xmit_cnt < (1UL << 12) - 1) {
                        info->xmit_buf[info->xmit_head++] = ch;
                        info->xmit_head &= (1UL << 12)-1;
                        info->xmit_cnt++;
                }
        }

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

}
static void mgsl_flush_chars(struct tty_struct *tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if ( debug_level >= 3 )
                printk( "%s(%d):mgsl_flush_chars() entry on %s xmit_cnt=%d\n",
                        "synclink.c",2105,info->device_name,info->xmit_cnt);

        if (mgsl_paranoia_check(info, tty->name, "mgsl_flush_chars"))
                return;

        if (info->xmit_cnt <= 0 || tty->stopped || tty->hw_stopped ||
            !info->xmit_buf)
                return;

        if ( debug_level >= 3 )
                printk( "%s(%d):mgsl_flush_chars() entry on %s starting transmitter\n",
                        "synclink.c",2116,info->device_name );

        flags = _spin_lock_irqsave(&info->irq_spinlock);

        if (!info->tx_active) {
                if ( (info->params.mode == 2 ||
                        info->params.mode == 6) && info->xmit_cnt ) {



                        mgsl_load_tx_dma_buffer(info,
                                 info->xmit_buf,info->xmit_cnt);
                }
                usc_start_transmitter(info);
        }

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

}
static int mgsl_write(struct tty_struct * tty,
                    const unsigned char *buf, int count)
{
        int c, ret = 0;
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if ( debug_level >= 3 )
                printk( "%s(%d):mgsl_write(%s) count=%d\n",
                        "synclink.c",2157,info->device_name,count);

        if (mgsl_paranoia_check(info, tty->name, "mgsl_write"))
                goto cleanup;

        if (!tty || !info->xmit_buf || !tmp_buf)
                goto cleanup;

        if ( info->params.mode == 2 ||
                        info->params.mode == 6 ) {


                if (info->tx_active) {

                        if ( info->params.mode == 2 ) {
                                ret = 0;
                                goto cleanup;
                        }





                        if (info->tx_holding_count >= info->num_tx_holding_buffers ) {

                                ret = 0;
                                goto cleanup;
                        }


                        ret = count;
                        save_tx_buffer_request(info,buf,count);




                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        load_next_tx_holding_buffer(info);
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                        goto cleanup;
                }





                if ( (info->params.flags & 0x4000) &&
                        !usc_loopmode_active(info) )
                {
                        ret = 0;
                        goto cleanup;
                }

                if ( info->xmit_cnt ) {


                        ret = 0;



                        mgsl_load_tx_dma_buffer(info,
                                info->xmit_buf,info->xmit_cnt);
                        if ( debug_level >= 3 )
                                printk( "%s(%d):mgsl_write(%s) sync xmit_cnt flushing\n",
                                        "synclink.c",2221,info->device_name);
                } else {
                        if ( debug_level >= 3 )
                                printk( "%s(%d):mgsl_write(%s) sync transmit accepted\n",
                                        "synclink.c",2225,info->device_name);
                        ret = count;
                        info->xmit_cnt = count;
                        mgsl_load_tx_dma_buffer(info,buf,count);
                }
        } else {
                while (1) {
                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        c = ({ int __x = (count); int __y = (({ typeof((1UL << 12) - info->xmit_cnt - 1) _x = ((1UL << 12) - info->xmit_cnt - 1); typeof((1UL << 12) - info->xmit_head) _y = ((1UL << 12) - info->xmit_head); (void) (&_x == &_y); _x < _y ? _x : _y; })); __x < __y ? __x: __y; });


                        if (c <= 0) {
                                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                                break;
                        }
                        (__builtin_constant_p(c) ? __constant_memcpy((info->xmit_buf + info->xmit_head),(buf),(c)) : __memcpy((info->xmit_buf + info->xmit_head),(buf),(c)));
                        info->xmit_head = ((info->xmit_head + c) &
                                           ((1UL << 12)-1));
                        info->xmit_cnt += c;
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                        buf += c;
                        count -= c;
                        ret += c;
                }
        }

        if (info->xmit_cnt && !tty->stopped && !tty->hw_stopped) {
                flags = _spin_lock_irqsave(&info->irq_spinlock);
                if (!info->tx_active)
                        usc_start_transmitter(info);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        }
cleanup:
        if ( debug_level >= 3 )
                printk( "%s(%d):mgsl_write(%s) returning=%d\n",
                        "synclink.c",2260,info->device_name,ret);

        return ret;

}
static int mgsl_write_room(struct tty_struct *tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        int ret;

        if (mgsl_paranoia_check(info, tty->name, "mgsl_write_room"))
                return 0;
        ret = (1UL << 12) - info->xmit_cnt - 1;
        if (ret < 0)
                ret = 0;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_write_room(%s)=%d\n",
                         "synclink.c",2286, info->device_name,ret );

        if ( info->params.mode == 2 ||
                info->params.mode == 6 ) {

                if ( info->tx_active )
                        return 0;
                else
                        return 65535;
        }

        return ret;

}
static int mgsl_chars_in_buffer(struct tty_struct *tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_chars_in_buffer(%s)\n",
                         "synclink.c",2314, info->device_name );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_chars_in_buffer"))
                return 0;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_chars_in_buffer(%s)=%d\n",
                         "synclink.c",2321, info->device_name,info->xmit_cnt );

        if ( info->params.mode == 2 ||
                info->params.mode == 6 ) {

                if ( info->tx_active )
                        return info->max_frame_size;
                else
                        return 0;
        }

        return info->xmit_cnt;
}
static void mgsl_flush_buffer(struct tty_struct *tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_flush_buffer(%s) entry\n",
                         "synclink.c",2349, info->device_name );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_flush_buffer"))
                return;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        info->xmit_cnt = info->xmit_head = info->xmit_tail = 0;
        del_timer(&info->tx_timer);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        __wake_up(&tty->write_wait, 1, 1, ((void *)0));
        tty_wakeup(tty);
}
static void mgsl_send_xchar(struct tty_struct *tty, char ch)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_send_xchar(%s,%d)\n",
                         "synclink.c",2378, info->device_name, ch );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_send_xchar"))
                return;

        info->x_char = ch;
        if (ch) {

                flags = _spin_lock_irqsave(&info->irq_spinlock);
                if (!info->tx_enabled)
                        usc_start_transmitter(info);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        }
}
static void mgsl_throttle(struct tty_struct * tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_throttle(%s) entry\n",
                         "synclink.c",2407, info->device_name );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_throttle"))
                return;

        if ((((tty))->termios->c_iflag & (0010000)))
                mgsl_send_xchar(tty, ((tty)->termios->c_cc[9]));

        if (tty->termios->c_cflag & 020000000000) {
                flags = _spin_lock_irqsave(&info->irq_spinlock);
                info->serial_signals &= ~0x20;
                usc_set_serial_signals(info);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        }
}
static void mgsl_unthrottle(struct tty_struct * tty)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_unthrottle(%s) entry\n",
                         "synclink.c",2437, info->device_name );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_unthrottle"))
                return;

        if ((((tty))->termios->c_iflag & (0010000))) {
                if (info->x_char)
                        info->x_char = 0;
                else
                        mgsl_send_xchar(tty, ((tty)->termios->c_cc[8]));
        }

        if (tty->termios->c_cflag & 020000000000) {
                flags = _spin_lock_irqsave(&info->irq_spinlock);
                info->serial_signals |= 0x20;
                usc_set_serial_signals(info);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        }

}
static int mgsl_get_stats(struct mgsl_struct * info, struct mgsl_icount *user_icount)
{
        int err;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_get_params(%s)\n",
                         "synclink.c",2473, info->device_name);

        if (!user_icount) {
                (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(info->icount))) ? __constant_c_and_count_memset(((&info->icount)),((0x01010101UL*(unsigned char)(0))),((sizeof(info->icount)))) : __constant_c_memset(((&info->icount)),((0x01010101UL*(unsigned char)(0))),((sizeof(info->icount))))) : (__builtin_constant_p((sizeof(info->icount))) ? __memset_generic((((&info->icount))),(((0))),(((sizeof(info->icount))))) : __memset_generic(((&info->icount)),((0)),((sizeof(info->icount))))));
        } else {
                err = copy_to_user(user_icount,&info->icount,sizeof(struct mgsl_icount)) ? -14 : 0;
                if (err)
                        return -14;
        }

        return 0;

}
static int mgsl_get_params(struct mgsl_struct * info, MGSL_PARAMS *user_params)
{
        int err;
        if (debug_level >= 3)
                printk("%s(%d):mgsl_get_params(%s)\n",
                         "synclink.c",2501, info->device_name);

        err = copy_to_user(user_params,&info->params,sizeof(MGSL_PARAMS)) ? -14 : 0;
        if (err) {
                if ( debug_level >= 3 )
                        printk( "%s(%d):mgsl_get_params(%s) user buffer copy failed\n",
                                "synclink.c",2507,info->device_name);
                return -14;
        }

        return 0;

}
static int mgsl_set_params(struct mgsl_struct * info, MGSL_PARAMS *new_params)
{
        unsigned long flags;
        MGSL_PARAMS tmp_params;
        int err;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_set_params %s\n", "synclink.c",2533,
                        info->device_name );
        err = copy_from_user(&tmp_params,new_params,sizeof(MGSL_PARAMS)) ? -14 : 0;
        if (err) {
                if ( debug_level >= 3 )
                        printk( "%s(%d):mgsl_set_params(%s) user buffer copy failed\n",
                                "synclink.c",2539,info->device_name);
                return -14;
        }

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        (__builtin_constant_p(sizeof(MGSL_PARAMS)) ? __constant_memcpy((&info->params),(&tmp_params),(sizeof(MGSL_PARAMS))) : __memcpy((&info->params),(&tmp_params),(sizeof(MGSL_PARAMS))));
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        mgsl_change_params(info);

        return 0;

}
static int mgsl_get_txidle(struct mgsl_struct * info, int *idle_mode)
{
        int err;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_get_txidle(%s)=%d\n",
                         "synclink.c",2568, info->device_name, info->idle_mode);

        err = copy_to_user(idle_mode,&info->idle_mode,sizeof(int)) ? -14 : 0;
        if (err) {
                if ( debug_level >= 3 )
                        printk( "%s(%d):mgsl_get_txidle(%s) user buffer copy failed\n",
                                "synclink.c",2574,info->device_name);
                return -14;
        }

        return 0;

}
static int mgsl_set_txidle(struct mgsl_struct * info, int idle_mode)
{
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_set_txidle(%s,%d)\n", "synclink.c",2594,
                        info->device_name, idle_mode );

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        info->idle_mode = idle_mode;
        usc_set_txidle( info );
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        return 0;

}
static int mgsl_txenable(struct mgsl_struct * info, int enable)
{
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_txenable(%s,%d)\n", "synclink.c",2621,
                        info->device_name, enable);

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if ( enable ) {
                if ( !info->tx_enabled ) {

                        usc_start_transmitter(info);







                        if ( info->params.flags & 0x4000 )
                                usc_loopmode_insert_request( info );
                }
        } else {
                if ( info->tx_enabled )
                        usc_stop_transmitter(info);
        }
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        return 0;

}






static int mgsl_txabort(struct mgsl_struct * info)
{
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_txabort(%s)\n", "synclink.c",2658,
                        info->device_name);

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if ( info->tx_active && info->params.mode == 2 )
        {
                if ( info->params.flags & 0x4000 )
                        usc_loopmode_cancel_transmit( info );
                else
                        usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x9000)));
        }
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        return 0;

}







static int mgsl_rxenable(struct mgsl_struct * info, int enable)
{
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_rxenable(%s,%d)\n", "synclink.c",2685,
                        info->device_name, enable);

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if ( enable ) {
                if ( !info->rx_enabled )
                        usc_start_receiver(info);
        } else {
                if ( info->rx_enabled )
                        usc_stop_receiver(info);
        }
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        return 0;

}
static int mgsl_wait_event(struct mgsl_struct * info, int * mask_ptr)
{
        unsigned long flags;
        int s;
        int rc=0;
        struct mgsl_icount cprev, cnow;
        int events;
        int mask;
        struct _input_signal_events oldsigs, newsigs;
        wait_queue_t wait = { .private = get_current(), .func = default_wake_function, .task_list = { ((void *)0), ((void *)0) } };

        rc = copy_from_user(&mask,mask_ptr,sizeof(int)) ? -14 : 0;
        if (rc) {
                return -14;
        }

        if (debug_level >= 3)
                printk("%s(%d):mgsl_wait_event(%s,%d)\n", "synclink.c",2726,
                        info->device_name, mask);

        flags = _spin_lock_irqsave(&info->irq_spinlock);


        usc_get_serial_signals(info);
        s = info->serial_signals;
        events = mask &
                ( ((s & 0x40) ? 0x0001:0x0002) +
                  ((s & 0x01) ? 0x0010:0x0020) +
                  ((s & 0x10) ? 0x0004:0x0008) +
                  ((s & 0x04) ? 0x0040 :0x0080) );
        if (events) {
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                goto exit;
        }


        cprev = info->icount;
        oldsigs = info->input_signal_events;


        if (mask & (0x0100 + 0x0200)) {
                u16 oldreg = usc_InReg(info,0x26);
                u16 newreg = oldreg +
                         (mask & 0x0100 ? 0x0080:0) +
                         (mask & 0x0200 ? 0x0040:0);
                if (oldreg != newreg)
                        usc_OutReg(info, 0x26, newreg);
        }

        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
        add_wait_queue(&info->event_wait_q, &wait);

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);


        for(;;) {
                schedule();
                if (signal_pending(get_current())) {
                        rc = -512;
                        break;
                }


                flags = _spin_lock_irqsave(&info->irq_spinlock);
                cnow = info->icount;
                newsigs = info->input_signal_events;
                do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);


                if (newsigs.dsr_up == oldsigs.dsr_up &&
                    newsigs.dsr_down == oldsigs.dsr_down &&
                    newsigs.dcd_up == oldsigs.dcd_up &&
                    newsigs.dcd_down == oldsigs.dcd_down &&
                    newsigs.cts_up == oldsigs.cts_up &&
                    newsigs.cts_down == oldsigs.cts_down &&
                    newsigs.ri_up == oldsigs.ri_up &&
                    newsigs.ri_down == oldsigs.ri_down &&
                    cnow.exithunt == cprev.exithunt &&
                    cnow.rxidle == cprev.rxidle) {
                        rc = -5;
                        break;
                }

                events = mask &
                        ( (newsigs.dsr_up != oldsigs.dsr_up ? 0x0001:0) +
                        (newsigs.dsr_down != oldsigs.dsr_down ? 0x0002:0) +
                        (newsigs.dcd_up != oldsigs.dcd_up ? 0x0010:0) +
                        (newsigs.dcd_down != oldsigs.dcd_down ? 0x0020:0) +
                        (newsigs.cts_up != oldsigs.cts_up ? 0x0004:0) +
                        (newsigs.cts_down != oldsigs.cts_down ? 0x0008:0) +
                        (newsigs.ri_up != oldsigs.ri_up ? 0x0040:0) +
                        (newsigs.ri_down != oldsigs.ri_down ? 0x0080:0) +
                        (cnow.exithunt != cprev.exithunt ? 0x0100:0) +
                          (cnow.rxidle != cprev.rxidle ? 0x0200:0) );
                if (events)
                        break;

                cprev = cnow;
                oldsigs = newsigs;
        }

        remove_wait_queue(&info->event_wait_q, &wait);
        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((0)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);

        if (mask & (0x0100 + 0x0200)) {
                flags = _spin_lock_irqsave(&info->irq_spinlock);
                if (!waitqueue_active(&info->event_wait_q)) {

                        usc_OutReg(info, 0x26, usc_InReg(info,0x26) &
                                ~(0x0080 + 0x0040));
                }
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        }
exit:
        if ( rc == 0 )
                rc = ({ int __ret_pu; __typeof__(*(mask_ptr)) __pus_tmp = events; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(mask_ptr, &__pus_tmp, sizeof(*(mask_ptr))) != 0), 0)) __ret_pu=-14; __ret_pu; });

        return rc;

}

static int modem_input_wait(struct mgsl_struct *info,int arg)
{
        unsigned long flags;
        int rc;
        struct mgsl_icount cprev, cnow;
        wait_queue_t wait = { .private = get_current(), .func = default_wake_function, .task_list = { ((void *)0), ((void *)0) } };


        flags = _spin_lock_irqsave(&info->irq_spinlock);
        cprev = info->icount;
        add_wait_queue(&info->status_event_wait_q, &wait);
        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        for(;;) {
                schedule();
                if (signal_pending(get_current())) {
                        rc = -512;
                        break;
                }


                flags = _spin_lock_irqsave(&info->irq_spinlock);
                cnow = info->icount;
                do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);


                if (cnow.rng == cprev.rng && cnow.dsr == cprev.dsr &&
                    cnow.dcd == cprev.dcd && cnow.cts == cprev.cts) {
                        rc = -5;
                        break;
                }


                if ((arg & 0x080 && cnow.rng != cprev.rng) ||
                    (arg & 0x100 && cnow.dsr != cprev.dsr) ||
                    (arg & 0x040 && cnow.dcd != cprev.dcd) ||
                    (arg & 0x020 && cnow.cts != cprev.cts)) {
                        rc = 0;
                        break;
                }

                cprev = cnow;
        }
        remove_wait_queue(&info->status_event_wait_q, &wait);
        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((0)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
        return rc;
}



static int tiocmget(struct tty_struct *tty, struct file *file)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned int result;
        unsigned long flags;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_get_serial_signals(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        result = ((info->serial_signals & 0x20) ? 0x004:0) +
                ((info->serial_signals & 0x80) ? 0x002:0) +
                ((info->serial_signals & 0x01) ? 0x040:0) +
                ((info->serial_signals & 0x04) ? 0x080:0) +
                ((info->serial_signals & 0x40) ? 0x100:0) +
                ((info->serial_signals & 0x10) ? 0x020:0);

        if (debug_level >= 3)
                printk("%s(%d):%s tiocmget() value=%08X\n",
                         "synclink.c",2902, info->device_name, result );
        return result;
}



static int tiocmset(struct tty_struct *tty, struct file *file,
                    unsigned int set, unsigned int clear)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):%s tiocmset(%x,%x)\n",
                        "synclink.c",2916,info->device_name, set, clear);

        if (set & 0x004)
                info->serial_signals |= 0x20;
        if (set & 0x002)
                info->serial_signals |= 0x80;
        if (clear & 0x004)
                info->serial_signals &= ~0x20;
        if (clear & 0x002)
                info->serial_signals &= ~0x80;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_set_serial_signals(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        return 0;
}







static void mgsl_break(struct tty_struct *tty, int break_state)
{
        struct mgsl_struct * info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_break(%s,%d)\n",
                         "synclink.c",2947, info->device_name, break_state);

        if (mgsl_paranoia_check(info, tty->name, "mgsl_break"))
                return;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if (break_state == -1)
                usc_OutReg(info,0x16,(u16)(usc_InReg(info,0x16) | 0x0080));
        else
                usc_OutReg(info,0x16,(u16)(usc_InReg(info,0x16) & ~0x0080));
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

}
static int mgsl_ioctl(struct tty_struct *tty, struct file * file,
                    unsigned int cmd, unsigned long arg)
{
        struct mgsl_struct * info = (struct mgsl_struct *)tty->driver_data;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_ioctl %s cmd=%08X\n", "synclink.c",2978,
                        info->device_name, cmd );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_ioctl"))
                return -19;

        if ((cmd != 0x541E) && (cmd != 0x541F) &&
            (cmd != 0x545C) && (cmd != 0x545D)) {
                if (tty->flags & (1 << 1))
                    return -5;
        }

        return mgsl_ioctl_common(info, cmd, arg);
}

static int mgsl_ioctl_common(struct mgsl_struct *info, unsigned int cmd, unsigned long arg)
{
        int error;
        struct mgsl_icount cnow;
        void *argp = (void *)arg;
        struct serial_icounter_struct *p_cuser;
        unsigned long flags;

        switch (cmd) {
                case 8:
                        return mgsl_get_params(info, argp);
                case (sizeof(struct _MGSL_PARAMS)):
                        return mgsl_set_params(info, argp);
                case (((0U) << (((0 +8)+8)+14)) | ((('m')) << (0 +8)) | (((3)) << 0) | ((0) << ((0 +8)+8))):
                        return mgsl_get_txidle(info, argp);
                case (((0U) << (((0 +8)+8)+14)) | ((('m')) << (0 +8)) | (((2)) << 0) | ((0) << ((0 +8)+8))):
                        return mgsl_set_txidle(info,(int)arg);
                case (((0U) << (((0 +8)+8)+14)) | ((('m')) << (0 +8)) | (((4)) << 0) | ((0) << ((0 +8)+8))):
                        return mgsl_txenable(info,(int)arg);
                case (((0U) << (((0 +8)+8)+14)) | ((('m')) << (0 +8)) | (((5)) << 0) | ((0) << ((0 +8)+8))):
                        return mgsl_rxenable(info,(int)arg);
                case (((0U) << (((0 +8)+8)+14)) | ((('m')) << (0 +8)) | (((6)) << 0) | ((0) << ((0 +8)+8))):
                        return mgsl_txabort(info);
                case (((0U) << (((0 +8)+8)+14)) | ((('m')) << (0 +8)) | (((7)) << 0) | ((0) << ((0 +8)+8))):
                        return mgsl_get_stats(info, argp);
                case 5:
                        return mgsl_wait_event(info, argp);
                case (((0U) << (((0 +8)+8)+14)) | ((('m')) << (0 +8)) | (((9)) << 0) | ((0) << ((0 +8)+8))):
                        return mgsl_loopmode_send_done(info);



                case 0x545C:
                        return modem_input_wait(info,(int)arg);







                case 0x545D:
                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        cnow = info->icount;
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                        p_cuser = argp;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->cts)) __pus_tmp = cnow.cts; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->cts, &__pus_tmp, sizeof(*(&p_cuser->cts))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->dsr)) __pus_tmp = cnow.dsr; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->dsr, &__pus_tmp, sizeof(*(&p_cuser->dsr))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->rng)) __pus_tmp = cnow.rng; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->rng, &__pus_tmp, sizeof(*(&p_cuser->rng))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->dcd)) __pus_tmp = cnow.dcd; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->dcd, &__pus_tmp, sizeof(*(&p_cuser->dcd))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->rx)) __pus_tmp = cnow.rx; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->rx, &__pus_tmp, sizeof(*(&p_cuser->rx))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->tx)) __pus_tmp = cnow.tx; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->tx, &__pus_tmp, sizeof(*(&p_cuser->tx))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->frame)) __pus_tmp = cnow.frame; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->frame, &__pus_tmp, sizeof(*(&p_cuser->frame))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->overrun)) __pus_tmp = cnow.overrun; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->overrun, &__pus_tmp, sizeof(*(&p_cuser->overrun))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->parity)) __pus_tmp = cnow.parity; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->parity, &__pus_tmp, sizeof(*(&p_cuser->parity))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->brk)) __pus_tmp = cnow.brk; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->brk, &__pus_tmp, sizeof(*(&p_cuser->brk))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        error = ({ int __ret_pu; __typeof__(*(&p_cuser->buf_overrun)) __pus_tmp = cnow.buf_overrun; __ret_pu=0; if(__builtin_expect(!!(__copy_to_user_ll(&p_cuser->buf_overrun, &__pus_tmp, sizeof(*(&p_cuser->buf_overrun))) != 0), 0)) __ret_pu=-14; __ret_pu; });
                        if (error) return error;
                        return 0;
                default:
                        return -515;
        }
        return 0;
}
static void mgsl_set_termios(struct tty_struct *tty, struct termios *old_termios)
{
        struct mgsl_struct *info = (struct mgsl_struct *)tty->driver_data;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_set_termios %s\n", "synclink.c",3085,
                        tty->driver->name );


        if ((tty->termios->c_cflag == old_termios->c_cflag)
            && ((tty->termios->c_iflag & (0000001|0000002|0000004|0000010|0000020))
                == (old_termios->c_iflag & (0000001|0000002|0000004|0000010|0000020))))
          return;

        mgsl_change_params(info);


        if (old_termios->c_cflag & 0010017 &&
            !(tty->termios->c_cflag & 0010017)) {
                info->serial_signals &= ~(0x20 + 0x80);
                flags = _spin_lock_irqsave(&info->irq_spinlock);
                usc_set_serial_signals(info);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        }


        if (!(old_termios->c_cflag & 0010017) &&
            tty->termios->c_cflag & 0010017) {
                info->serial_signals |= 0x80;
                if (!(tty->termios->c_cflag & 020000000000) ||
                    !(__builtin_constant_p(0) ? constant_test_bit((0),(&tty->flags)) : variable_test_bit((0),(&tty->flags)))) {
                        info->serial_signals |= 0x20;
                }
                flags = _spin_lock_irqsave(&info->irq_spinlock);
                usc_set_serial_signals(info);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        }


        if (old_termios->c_cflag & 020000000000 &&
            !(tty->termios->c_cflag & 020000000000)) {
                tty->hw_stopped = 0;
                mgsl_start(tty);
        }

}
static void mgsl_close(struct tty_struct *tty, struct file * filp)
{
        struct mgsl_struct * info = (struct mgsl_struct *)tty->driver_data;

        if (mgsl_paranoia_check(info, tty->name, "mgsl_close"))
                return;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_close(%s) entry, count=%d\n",
                         "synclink.c",3148, info->device_name, info->count);

        if (!info->count)
                return;

        if (tty_hung_up_p(filp))
                goto cleanup;

        if ((tty->count == 1) && (info->count != 1)) {





                printk("mgsl_close: bad refcount; tty->count is 1, "
                       "info->count is %d\n", info->count);
                info->count = 1;
        }

        info->count--;


        if (info->count)
                goto cleanup;

        info->flags |= 0x08000000;





        tty->closing = 1;



        if (info->closing_wait != 65535) {
                if (debug_level >= 3)
                        printk("%s(%d):mgsl_close(%s) calling tty_wait_until_sent\n",
                                 "synclink.c",3186, info->device_name );
                tty_wait_until_sent(tty, info->closing_wait);
        }

        if (info->flags & 0x80000000)
                mgsl_wait_until_sent(tty, info->timeout);

        if (tty->driver->flush_buffer)
                tty->driver->flush_buffer(tty);

        tty_ldisc_flush(tty);

        shutdown(info);

        tty->closing = 0;
        info->tty = ((void *)0);

        if (info->blocked_open) {
                if (info->close_delay) {
                        msleep_interruptible(jiffies_to_msecs(info->close_delay));
                }
                __wake_up(&info->open_wait, 1, 1, ((void *)0));
        }

        info->flags &= ~(0x20000000|0x08000000);

        __wake_up(&info->close_wait, 1, 1, ((void *)0));

cleanup:
        if (debug_level >= 3)
                printk("%s(%d):mgsl_close(%s) exit, count=%d\n", "synclink.c",3216,
                        tty->driver->name, info->count);

}
static void mgsl_wait_until_sent(struct tty_struct *tty, int timeout)
{
        struct mgsl_struct * info = (struct mgsl_struct *)tty->driver_data;
        unsigned long orig_jiffies, char_time;

        if (!info )
                return;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_wait_until_sent(%s) entry\n",
                         "synclink.c",3242, info->device_name );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_wait_until_sent"))
                return;

        if (!(info->flags & 0x80000000))
                goto exit;

        orig_jiffies = jiffies;







        if ( info->params.data_rate ) {
                char_time = info->timeout/(32 * 5);
                if (!char_time)
                        char_time++;
        } else
                char_time = 1;

        if (timeout)
                char_time = ({ unsigned long __x = (char_time); unsigned long __y = (timeout); __x < __y ? __x: __y; });

        if ( info->params.mode == 2 ||
                info->params.mode == 6 ) {
                while (info->tx_active) {
                        msleep_interruptible(jiffies_to_msecs(char_time));
                        if (signal_pending(get_current()))
                                break;
                        if (timeout && (({ unsigned long __dummy; typeof(jiffies) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ({ unsigned long __dummy; typeof(orig_jiffies + timeout) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ((long)(orig_jiffies + timeout) - (long)(jiffies) < 0)))
                                break;
                }
        } else {
                while (!(usc_InReg(info,0x34) & 0x0004) &&
                        info->tx_enabled) {
                        msleep_interruptible(jiffies_to_msecs(char_time));
                        if (signal_pending(get_current()))
                                break;
                        if (timeout && (({ unsigned long __dummy; typeof(jiffies) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ({ unsigned long __dummy; typeof(orig_jiffies + timeout) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ((long)(orig_jiffies + timeout) - (long)(jiffies) < 0)))
                                break;
                }
        }

exit:
        if (debug_level >= 3)
                printk("%s(%d):mgsl_wait_until_sent(%s) exit\n",
                         "synclink.c",3291, info->device_name );

}
static void mgsl_hangup(struct tty_struct *tty)
{
        struct mgsl_struct * info = (struct mgsl_struct *)tty->driver_data;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_hangup(%s)\n",
                         "synclink.c",3309, info->device_name );

        if (mgsl_paranoia_check(info, tty->name, "mgsl_hangup"))
                return;

        mgsl_flush_buffer(tty);
        shutdown(info);

        info->count = 0;
        info->flags &= ~0x20000000;
        info->tty = ((void *)0);

        __wake_up(&info->open_wait, 1, 1, ((void *)0));

}
static int block_til_ready(struct tty_struct *tty, struct file * filp,
                           struct mgsl_struct *info)
{
        wait_queue_t wait = { .private = get_current(), .func = default_wake_function, .task_list = { ((void *)0), ((void *)0) } };
        int retval;
        int do_clocal = 0, extra_count = 0;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s(%d):block_til_ready on %s\n",
                         "synclink.c",3348, tty->driver->name );

        if (filp->f_flags & 00004000 || tty->flags & (1 << 1)){

                info->flags |= 0x20000000;
                return 0;
        }

        if (tty->termios->c_cflag & 0004000)
                do_clocal = 1;
        retval = 0;
        add_wait_queue(&info->open_wait, &wait);

        if (debug_level >= 3)
                printk("%s(%d):block_til_ready before block on %s count=%d\n",
                         "synclink.c",3371, tty->driver->name, info->count );

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if (!tty_hung_up_p(filp)) {
                extra_count = 1;
                info->count--;
        }
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        info->blocked_open++;

        while (1) {
                if (tty->termios->c_cflag & 0010017) {
                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        info->serial_signals |= 0x20 + 0x80;
                        usc_set_serial_signals(info);
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                }

                do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);

                if (tty_hung_up_p(filp) || !(info->flags & 0x80000000)){
                        retval = (info->flags & 0x0001) ?
                                        -11 : -512;
                        break;
                }

                flags = _spin_lock_irqsave(&info->irq_spinlock);
                usc_get_serial_signals(info);
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);

                if (!(info->flags & 0x08000000) &&
                    (do_clocal || (info->serial_signals & 0x01)) ) {
                        break;
                }

                if (signal_pending(get_current())) {
                        retval = -512;
                        break;
                }

                if (debug_level >= 3)
                        printk("%s(%d):block_til_ready blocking on %s count=%d\n",
                                 "synclink.c",3413, tty->driver->name, info->count );

                schedule();
        }

        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((0)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
        remove_wait_queue(&info->open_wait, &wait);

        if (extra_count)
                info->count++;
        info->blocked_open--;

        if (debug_level >= 3)
                printk("%s(%d):block_til_ready after blocking on %s count=%d\n",
                         "synclink.c",3427, tty->driver->name, info->count );

        if (!retval)
                info->flags |= 0x20000000;

        return retval;

}
static int mgsl_open(struct tty_struct *tty, struct file * filp)
{
        struct mgsl_struct *info;
        int retval, line;
        unsigned long page;
        unsigned long flags;


        line = tty->index;
        if ((line < 0) || (line >= mgsl_device_count)) {
                printk("%s(%d):mgsl_open with invalid line #%d.\n",
                        "synclink.c",3457,line);
                return -19;
        }


        info = mgsl_device_list;
        while(info && info->line != line)
                info = info->next_device;
        if (mgsl_paranoia_check(info, tty->name, "mgsl_open"))
                return -19;

        tty->driver_data = info;
        info->tty = tty;

        if (debug_level >= 3)
                printk("%s(%d):mgsl_open(%s), old ref count = %d\n",
                         "synclink.c",3473,tty->driver->name, info->count);


        if (tty_hung_up_p(filp) || info->flags & 0x08000000){
                if (info->flags & 0x08000000)
                        interruptible_sleep_on(&info->close_wait);
                retval = ((info->flags & 0x0001) ?
                        -11 : -512);
                goto cleanup;
        }

        if (!tmp_buf) {
                page = get_zeroed_page((0x10u | 0x40u | 0x80u));
                if (!page) {
                        retval = -12;
                        goto cleanup;
                }
                if (tmp_buf)
                        free_pages((page),0);
                else
                        tmp_buf = (unsigned char *) page;
        }

        info->tty->low_latency = (info->flags & 0x2000) ? 1 : 0;

        flags = _spin_lock_irqsave(&info->netlock);
        if (info->netcount) {
                retval = -16;
                _spin_unlock_irqrestore(&info->netlock, flags);
                goto cleanup;
        }
        info->count++;
        _spin_unlock_irqrestore(&info->netlock, flags);

        if (info->count == 1) {

                retval = startup(info);
                if (retval < 0)
                        goto cleanup;
        }

        retval = block_til_ready(tty, filp, info);
        if (retval) {
                if (debug_level >= 3)
                        printk("%s(%d):block_til_ready(%s) returned %d\n",
                                 "synclink.c",3518, info->device_name, retval);
                goto cleanup;
        }

        if (debug_level >= 3)
                printk("%s(%d):mgsl_open(%s) success\n",
                         "synclink.c",3524, info->device_name);
        retval = 0;

cleanup:
        if (retval) {
                if (tty->count == 1)
                        info->tty = ((void *)0);
                if(info->count)
                        info->count--;
        }

        return retval;

}





static int line_info(char *buf, struct mgsl_struct *info)
{
        char stat_buf[30];
        int ret;
        unsigned long flags;

        if (info->bus_type == 5) {
                ret = sprintf(buf, "%s:PCI io:%04X irq:%d mem:%08X lcr:%08X",
                        info->device_name, info->io_base, info->irq_level,
                        info->phys_memory_base, info->phys_lcr_base);
        } else {
                ret = sprintf(buf, "%s:(E)ISA io:%04X irq:%d dma:%d",
                        info->device_name, info->io_base,
                        info->irq_level, info->dma_level);
        }


        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_get_serial_signals(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        stat_buf[0] = 0;
        stat_buf[1] = 0;
        if (info->serial_signals & 0x20)
                strcat(stat_buf, "|RTS");
        if (info->serial_signals & 0x10)
                strcat(stat_buf, "|CTS");
        if (info->serial_signals & 0x80)
                strcat(stat_buf, "|DTR");
        if (info->serial_signals & 0x40)
                strcat(stat_buf, "|DSR");
        if (info->serial_signals & 0x01)
                strcat(stat_buf, "|CD");
        if (info->serial_signals & 0x04)
                strcat(stat_buf, "|RI");

        if (info->params.mode == 2 ||
            info->params.mode == 6 ) {
                ret += sprintf(buf+ret, " HDLC txok:%d rxok:%d",
                              info->icount.txok, info->icount.rxok);
                if (info->icount.txunder)
                        ret += sprintf(buf+ret, " txunder:%d", info->icount.txunder);
                if (info->icount.txabort)
                        ret += sprintf(buf+ret, " txabort:%d", info->icount.txabort);
                if (info->icount.rxshort)
                        ret += sprintf(buf+ret, " rxshort:%d", info->icount.rxshort);
                if (info->icount.rxlong)
                        ret += sprintf(buf+ret, " rxlong:%d", info->icount.rxlong);
                if (info->icount.rxover)
                        ret += sprintf(buf+ret, " rxover:%d", info->icount.rxover);
                if (info->icount.rxcrc)
                        ret += sprintf(buf+ret, " rxcrc:%d", info->icount.rxcrc);
        } else {
                ret += sprintf(buf+ret, " ASYNC tx:%d rx:%d",
                              info->icount.tx, info->icount.rx);
                if (info->icount.frame)
                        ret += sprintf(buf+ret, " fe:%d", info->icount.frame);
                if (info->icount.parity)
                        ret += sprintf(buf+ret, " pe:%d", info->icount.parity);
                if (info->icount.brk)
                        ret += sprintf(buf+ret, " brk:%d", info->icount.brk);
                if (info->icount.overrun)
                        ret += sprintf(buf+ret, " oe:%d", info->icount.overrun);
        }


        ret += sprintf(buf+ret, " %s\n", stat_buf+1);

        ret += sprintf(buf+ret, "txactive=%d bh_req=%d bh_run=%d pending_bh=%x\n",
         info->tx_active,info->bh_requested,info->bh_running,
         info->pending_bh);

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        {
        u16 Tcsr = usc_InReg( info, 0x34 );
        u16 Tdmr = usc_InDmaReg( info, 0x02 );
        u16 Ticr = usc_InReg( info, 0x36 );
        u16 Rscr = usc_InReg( info, 0x24 );
        u16 Rdmr = usc_InDmaReg( info, 0x82 );
        u16 Ricr = usc_InReg( info, 0x26 );
        u16 Icr = usc_InReg( info, 0x18 );
        u16 Dccr = usc_InReg( info, 0x1a );
        u16 Tmr = usc_InReg( info, 0x32 );
        u16 Tccr = usc_InReg( info, 0x3c );
        u16 Ccar = inw( info->io_base + 4 );
        ret += sprintf(buf+ret, "tcsr=%04X tdmr=%04X ticr=%04X rcsr=%04X rdmr=%04X\n"
                        "ricr=%04X icr =%04X dccr=%04X tmr=%04X tccr=%04X ccar=%04X\n",
                        Tcsr,Tdmr,Ticr,Rscr,Rdmr,Ricr,Icr,Dccr,Tmr,Tccr,Ccar );
        }
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        return ret;

}
static int mgsl_read_proc(char *page, char **start, off_t off, int count,
                 int *eof, void *data)
{
        int len = 0, l;
        off_t begin = 0;
        struct mgsl_struct *info;

        len += sprintf(page, "synclink driver:%s\n", driver_version);

        info = mgsl_device_list;
        while( info ) {
                l = line_info(page + len, info);
                len += l;
                if (len+begin > off+count)
                        goto done;
                if (len+begin < off) {
                        begin += len;
                        len = 0;
                }
                info = info->next_device;
        }

        *eof = 1;
done:
        if (off >= len+begin)
                return 0;
        *start = page + (off-begin);
        return ((count < begin+len-off) ? count : begin+len-off);

}
static int mgsl_allocate_dma_buffers(struct mgsl_struct *info)
{
        unsigned short BuffersPerFrame;

        info->last_mem_alloc = 0;






        BuffersPerFrame = (unsigned short)(info->max_frame_size/((1UL << 12)));
        if ( info->max_frame_size % ((1UL << 12)) )
                BuffersPerFrame++;

        if ( info->bus_type == 5 ) {
                info->tx_buffer_count = info->num_tx_dma_buffers * BuffersPerFrame;
                info->rx_buffer_count = 62 - info->tx_buffer_count;
        } else {
                info->tx_buffer_count = info->num_tx_dma_buffers * BuffersPerFrame;
                info->rx_buffer_count = (BuffersPerFrame * 7) + 6;






                if ( (info->tx_buffer_count + info->rx_buffer_count) > 62 )
                        info->rx_buffer_count = 62 - info->tx_buffer_count;

        }

        if ( debug_level >= 3 )
                printk("%s(%d):Allocating %d TX and %d RX DMA buffers.\n",
                        "synclink.c",3757, info->tx_buffer_count,info->rx_buffer_count);

        if ( mgsl_alloc_buffer_list_memory( info ) < 0 ||
                  mgsl_alloc_frame_memory(info, info->rx_buffer_list, info->rx_buffer_count) < 0 ||
                  mgsl_alloc_frame_memory(info, info->tx_buffer_list, info->tx_buffer_count) < 0 ||
                  mgsl_alloc_intermediate_rxbuffer_memory(info) < 0 ||
                  mgsl_alloc_intermediate_txbuffer_memory(info) < 0 ) {
                printk("%s(%d):Can't allocate DMA buffer memory\n","synclink.c",3764);
                return -12;
        }

        mgsl_reset_rx_dma_buffers( info );
        mgsl_reset_tx_dma_buffers( info );

        return 0;

}
static int mgsl_alloc_buffer_list_memory( struct mgsl_struct *info )
{
        unsigned int i;

        if ( info->bus_type == 5 ) {

                info->buffer_list = info->memory_base + info->last_mem_alloc;
                info->buffer_list_phys = info->last_mem_alloc;
                info->last_mem_alloc += ((1UL << 12));
        } else {






                info->buffer_list = kmalloc(((1UL << 12)), (0x10u | 0x40u | 0x80u) | 0x01u);
                if ( info->buffer_list == ((void *)0) )
                        return -12;

                info->buffer_list_phys = virt_to_phys(info->buffer_list);
        }



        (__builtin_constant_p(0) ? (__builtin_constant_p((((1UL << 12)))) ? __constant_c_and_count_memset(((info->buffer_list)),((0x01010101UL*(unsigned char)(0))),((((1UL << 12))))) : __constant_c_memset(((info->buffer_list)),((0x01010101UL*(unsigned char)(0))),((((1UL << 12)))))) : (__builtin_constant_p((((1UL << 12)))) ? __memset_generic((((info->buffer_list))),(((0))),(((((1UL << 12)))))) : __memset_generic(((info->buffer_list)),((0)),((((1UL << 12)))))));




        info->rx_buffer_list = (DMABUFFERENTRY *)info->buffer_list;
        info->tx_buffer_list = (DMABUFFERENTRY *)info->buffer_list;
        info->tx_buffer_list += info->rx_buffer_count;
        for ( i = 0; i < info->rx_buffer_count; i++ ) {

                info->rx_buffer_list[i].phys_entry =
                        info->buffer_list_phys + (i * sizeof(DMABUFFERENTRY));




                info->rx_buffer_list[i].link = info->buffer_list_phys;

                if ( i < info->rx_buffer_count - 1 )
                        info->rx_buffer_list[i].link += (i + 1) * sizeof(DMABUFFERENTRY);
        }

        for ( i = 0; i < info->tx_buffer_count; i++ ) {

                info->tx_buffer_list[i].phys_entry = info->buffer_list_phys +
                        ((info->rx_buffer_count + i) * sizeof(DMABUFFERENTRY));




                info->tx_buffer_list[i].link = info->buffer_list_phys +
                        info->rx_buffer_count * sizeof(DMABUFFERENTRY);

                if ( i < info->tx_buffer_count - 1 )
                        info->tx_buffer_list[i].link += (i + 1) * sizeof(DMABUFFERENTRY);
        }

        return 0;

}
static void mgsl_free_buffer_list_memory( struct mgsl_struct *info )
{
        if ( info->buffer_list && info->bus_type != 5 )
                kfree(info->buffer_list);

        info->buffer_list = ((void *)0);
        info->rx_buffer_list = ((void *)0);
        info->tx_buffer_list = ((void *)0);

}
static int mgsl_alloc_frame_memory(struct mgsl_struct *info,DMABUFFERENTRY *BufferList,int Buffercount)
{
        int i;
        unsigned long phys_addr;



        for ( i = 0; i < Buffercount; i++ ) {
                if ( info->bus_type == 5 ) {

                        BufferList[i].virt_addr = info->memory_base + info->last_mem_alloc;
                        phys_addr = info->last_mem_alloc;
                        info->last_mem_alloc += ((1UL << 12));
                } else {

                        BufferList[i].virt_addr =
                                kmalloc(((1UL << 12)), (0x10u | 0x40u | 0x80u) | 0x01u);
                        if ( BufferList[i].virt_addr == ((void *)0) )
                                return -12;
                        phys_addr = virt_to_phys(BufferList[i].virt_addr);
                }
                BufferList[i].phys_addr = phys_addr;
        }

        return 0;

}
static void mgsl_free_frame_memory(struct mgsl_struct *info, DMABUFFERENTRY *BufferList, int Buffercount)
{
        int i;

        if ( BufferList ) {
                for ( i = 0 ; i < Buffercount ; i++ ) {
                        if ( BufferList[i].virt_addr ) {
                                if ( info->bus_type != 5 )
                                        kfree(BufferList[i].virt_addr);
                                BufferList[i].virt_addr = ((void *)0);
                        }
                }
        }

}
static void mgsl_free_dma_buffers( struct mgsl_struct *info )
{
        mgsl_free_frame_memory( info, info->rx_buffer_list, info->rx_buffer_count );
        mgsl_free_frame_memory( info, info->tx_buffer_list, info->tx_buffer_count );
        mgsl_free_buffer_list_memory( info );

}
static int mgsl_alloc_intermediate_rxbuffer_memory(struct mgsl_struct *info)
{
        info->intermediate_rxbuffer = kmalloc(info->max_frame_size, (0x10u | 0x40u | 0x80u) | 0x01u);
        if ( info->intermediate_rxbuffer == ((void *)0) )
                return -12;

        return 0;

}
static void mgsl_free_intermediate_rxbuffer_memory(struct mgsl_struct *info)
{
        if ( info->intermediate_rxbuffer )
                kfree(info->intermediate_rxbuffer);

        info->intermediate_rxbuffer = ((void *)0);

}
static int mgsl_alloc_intermediate_txbuffer_memory(struct mgsl_struct *info)
{
        int i;

        if ( debug_level >= 3 )
                printk("%s %s(%d)  allocating %d tx holding buffers\n",
                                info->device_name, "synclink.c",4044,info->num_tx_holding_buffers);

        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(info->tx_holding_buffers))) ? __constant_c_and_count_memset(((info->tx_holding_buffers)),((0x01010101UL*(unsigned char)(0))),((sizeof(info->tx_holding_buffers)))) : __constant_c_memset(((info->tx_holding_buffers)),((0x01010101UL*(unsigned char)(0))),((sizeof(info->tx_holding_buffers))))) : (__builtin_constant_p((sizeof(info->tx_holding_buffers))) ? __memset_generic((((info->tx_holding_buffers))),(((0))),(((sizeof(info->tx_holding_buffers))))) : __memset_generic(((info->tx_holding_buffers)),((0)),((sizeof(info->tx_holding_buffers))))));

        for ( i=0; i<info->num_tx_holding_buffers; ++i) {
                info->tx_holding_buffers[i].buffer =
                        kmalloc(info->max_frame_size, (0x10u | 0x40u | 0x80u));
                if ( info->tx_holding_buffers[i].buffer == ((void *)0) )
                        return -12;
        }

        return 0;

}
static void mgsl_free_intermediate_txbuffer_memory(struct mgsl_struct *info)
{
        int i;

        for ( i=0; i<info->num_tx_holding_buffers; ++i ) {
                if ( info->tx_holding_buffers[i].buffer ) {
                                kfree(info->tx_holding_buffers[i].buffer);
                                info->tx_holding_buffers[i].buffer=((void *)0);
                }
        }

        info->get_tx_holding_index = 0;
        info->put_tx_holding_index = 0;
        info->tx_holding_count = 0;

}
static int load_next_tx_holding_buffer(struct mgsl_struct *info)
{
        int ret = 0;

        if ( info->tx_holding_count ) {



                struct tx_holding_buffer *ptx =
                        &info->tx_holding_buffers[info->get_tx_holding_index];
                int num_free = num_free_tx_dma_buffers(info);
                int num_needed = ptx->buffer_size / ((1UL << 12));
                if ( ptx->buffer_size % ((1UL << 12)) )
                        ++num_needed;

                if (num_needed <= num_free) {
                        info->xmit_cnt = ptx->buffer_size;
                        mgsl_load_tx_dma_buffer(info,ptx->buffer,ptx->buffer_size);

                        --info->tx_holding_count;
                        if ( ++info->get_tx_holding_index >= info->num_tx_holding_buffers)
                                info->get_tx_holding_index=0;


                        mod_timer(&info->tx_timer, jiffies + msecs_to_jiffies(5000));

                        ret = 1;
                }
        }

        return ret;
}
static int save_tx_buffer_request(struct mgsl_struct *info,const char *Buffer, unsigned int BufferSize)
{
        struct tx_holding_buffer *ptx;

        if ( info->tx_holding_count >= info->num_tx_holding_buffers ) {
                return 0;
        }

        ptx = &info->tx_holding_buffers[info->put_tx_holding_index];
        ptx->buffer_size = BufferSize;
        (__builtin_constant_p(BufferSize) ? __constant_memcpy((ptx->buffer),(Buffer),(BufferSize)) : __memcpy((ptx->buffer),(Buffer),(BufferSize)));

        ++info->tx_holding_count;
        if ( ++info->put_tx_holding_index >= info->num_tx_holding_buffers)
                info->put_tx_holding_index=0;

        return 1;
}

static int mgsl_claim_resources(struct mgsl_struct *info)
{
        if (__request_region(&ioport_resource, (info->io_base), (info->io_addr_size), ("synclink")) == ((void *)0)) {
                printk( "%s(%d):I/O address conflict on device %s Addr=%08X\n",
                        "synclink.c",4170,info->device_name, info->io_base);
                return -19;
        }
        info->io_addr_requested = 1;

        if ( request_irq(info->irq_level,mgsl_interrupt,info->irq_flags,
                info->device_name, info ) < 0 ) {
                printk( "%s(%d):Cant request interrupt on device %s IRQ=%d\n",
                        "synclink.c",4178,info->device_name, info->irq_level );
                goto errout;
        }
        info->irq_requested = 1;

        if ( info->bus_type == 5 ) {
                if (__request_region(&iomem_resource, (info->phys_memory_base), (0x40000), ("synclink")) == ((void *)0)) {
                        printk( "%s(%d):mem addr conflict device %s Addr=%08X\n",
                                "synclink.c",4186,info->device_name, info->phys_memory_base);
                        goto errout;
                }
                info->shared_mem_requested = 1;
                if (__request_region(&iomem_resource, (info->phys_lcr_base + info->lcr_offset), (128), ("synclink")) == ((void *)0)) {
                        printk( "%s(%d):lcr mem addr conflict device %s Addr=%08X\n",
                                "synclink.c",4192,info->device_name, info->phys_lcr_base + info->lcr_offset);
                        goto errout;
                }
                info->lcr_mem_requested = 1;

                info->memory_base = ioremap(info->phys_memory_base,0x40000);
                if (!info->memory_base) {
                        printk( "%s(%d):Cant map shared memory on device %s MemAddr=%08X\n",
                                "synclink.c",4200,info->device_name, info->phys_memory_base );
                        goto errout;
                }

                if ( !mgsl_memory_test(info) ) {
                        printk( "%s(%d):Failed shared memory test %s MemAddr=%08X\n",
                                "synclink.c",4206,info->device_name, info->phys_memory_base );
                        goto errout;
                }

                info->lcr_base = ioremap(info->phys_lcr_base,(1UL << 12)) + info->lcr_offset;
                if (!info->lcr_base) {
                        printk( "%s(%d):Cant map LCR memory on device %s MemAddr=%08X\n",
                                "synclink.c",4213,info->device_name, info->phys_lcr_base );
                        goto errout;
                }

        } else {


                if (request_dma(info->dma_level,info->device_name) < 0){
                        printk( "%s(%d):Cant request DMA channel on device %s DMA=%d\n",
                                "synclink.c",4222,info->device_name, info->dma_level );
                        mgsl_release_resources( info );
                        return -19;
                }
                info->dma_requested = 1;


                set_dma_mode(info->dma_level,0xC0);
                enable_dma(info->dma_level);
        }

        if ( mgsl_allocate_dma_buffers(info) < 0 ) {
                printk( "%s(%d):Cant allocate DMA buffers on device %s DMA=%d\n",
                        "synclink.c",4235,info->device_name, info->dma_level );
                goto errout;
        }

        return 0;
errout:
        mgsl_release_resources(info);
        return -19;

}

static void mgsl_release_resources(struct mgsl_struct *info)
{
        if ( debug_level >= 3 )
                printk( "%s(%d):mgsl_release_resources(%s) entry\n",
                        "synclink.c",4250,info->device_name );

        if ( info->irq_requested ) {
                free_irq(info->irq_level, info);
                info->irq_requested = 0;
        }
        if ( info->dma_requested ) {
                disable_dma(info->dma_level);
                free_dma(info->dma_level);
                info->dma_requested = 0;
        }
        mgsl_free_dma_buffers(info);
        mgsl_free_intermediate_rxbuffer_memory(info);
        mgsl_free_intermediate_txbuffer_memory(info);

        if ( info->io_addr_requested ) {
                __release_region(&ioport_resource, (info->io_base), (info->io_addr_size));
                info->io_addr_requested = 0;
        }
        if ( info->shared_mem_requested ) {
                __release_region(&iomem_resource, (info->phys_memory_base), (0x40000));
                info->shared_mem_requested = 0;
        }
        if ( info->lcr_mem_requested ) {
                __release_region(&iomem_resource, (info->phys_lcr_base + info->lcr_offset), (128));
                info->lcr_mem_requested = 0;
        }
        if (info->memory_base){
                iounmap(info->memory_base);
                info->memory_base = ((void *)0);
        }
        if (info->lcr_base){
                iounmap(info->lcr_base - info->lcr_offset);
                info->lcr_base = ((void *)0);
        }

        if ( debug_level >= 3 )
                printk( "%s(%d):mgsl_release_resources(%s) exit\n",
                        "synclink.c",4288,info->device_name );

}
static void mgsl_add_device( struct mgsl_struct *info )
{
        info->next_device = ((void *)0);
        info->line = mgsl_device_count;
        sprintf(info->device_name,"ttySL%d",info->line);

        if (info->line < 20) {
                if (maxframe[info->line])
                        info->max_frame_size = maxframe[info->line];
                info->dosyncppp = dosyncppp[info->line];

                if (txdmabufs[info->line]) {
                        info->num_tx_dma_buffers = txdmabufs[info->line];
                        if (info->num_tx_dma_buffers < 1)
                                info->num_tx_dma_buffers = 1;
                }

                if (txholdbufs[info->line]) {
                        info->num_tx_holding_buffers = txholdbufs[info->line];
                        if (info->num_tx_holding_buffers < 1)
                                info->num_tx_holding_buffers = 1;
                        else if (info->num_tx_holding_buffers > 5)
                                info->num_tx_holding_buffers = 5;
                }
        }

        mgsl_device_count++;

        if ( !mgsl_device_list )
                mgsl_device_list = info;
        else {
                struct mgsl_struct *current_dev = mgsl_device_list;
                while( current_dev->next_device )
                        current_dev = current_dev->next_device;
                current_dev->next_device = info;
        }

        if ( info->max_frame_size < 4096 )
                info->max_frame_size = 4096;
        else if ( info->max_frame_size > 65535 )
                info->max_frame_size = 65535;

        if ( info->bus_type == 5 ) {
                printk( "SyncLink PCI v%d %s: IO=%04X IRQ=%d Mem=%08X,%08X MaxFrameSize=%u\n",
                        info->hw_version + 1, info->device_name, info->io_base, info->irq_level,
                        info->phys_memory_base, info->phys_lcr_base,
                        info->max_frame_size );
        } else {
                printk( "SyncLink ISA %s: IO=%04X IRQ=%d DMA=%d MaxFrameSize=%u\n",
                        info->device_name, info->io_base, info->irq_level, info->dma_level,
                        info->max_frame_size );
        }


        hdlcdev_init(info);


}
static struct mgsl_struct* mgsl_allocate_device(void)
{
        struct mgsl_struct *info;

        info = (struct mgsl_struct *)kmalloc(sizeof(struct mgsl_struct),
                 (0x10u | 0x40u | 0x80u));

        if (!info) {
                printk("Error can't allocate device instance data\n");
        } else {
                (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(struct mgsl_struct))) ? __constant_c_and_count_memset(((info)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct mgsl_struct)))) : __constant_c_memset(((info)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct mgsl_struct))))) : (__builtin_constant_p((sizeof(struct mgsl_struct))) ? __memset_generic((((info))),(((0))),(((sizeof(struct mgsl_struct))))) : __memset_generic(((info)),((0)),((sizeof(struct mgsl_struct))))));
                info->magic = 0x5401;
                do { do { (&(&info->task)->entry)->next = (&(&info->task)->entry); (&(&info->task)->entry)->prev = (&(&info->task)->entry); } while (0); (&info->task)->pending = 0; do { ((&info->task))->func = (mgsl_bh_handler); ((&info->task))->data = (info); } while (0); init_timer(&(&info->task)->timer); } while (0);
                info->max_frame_size = 4096;
                info->close_delay = 5*250/10;
                info->closing_wait = 30*250;
                init_waitqueue_head(&info->open_wait);
                init_waitqueue_head(&info->close_wait);
                init_waitqueue_head(&info->status_event_wait_q);
                init_waitqueue_head(&info->event_wait_q);
                spin_lock_init(&info->irq_spinlock);
                spin_lock_init(&info->netlock);
                (__builtin_constant_p(sizeof(MGSL_PARAMS)) ? __constant_memcpy((&info->params),(&default_params),(sizeof(MGSL_PARAMS))) : __memcpy((&info->params),(&default_params),(sizeof(MGSL_PARAMS))));
                info->idle_mode = 0;
                info->num_tx_dma_buffers = 1;
                info->num_tx_holding_buffers = 0;
        }

        return info;

}

static struct tty_operations mgsl_ops = {
        .open = mgsl_open,
        .close = mgsl_close,
        .write = mgsl_write,
        .put_char = mgsl_put_char,
        .flush_chars = mgsl_flush_chars,
        .write_room = mgsl_write_room,
        .chars_in_buffer = mgsl_chars_in_buffer,
        .flush_buffer = mgsl_flush_buffer,
        .ioctl = mgsl_ioctl,
        .throttle = mgsl_throttle,
        .unthrottle = mgsl_unthrottle,
        .send_xchar = mgsl_send_xchar,
        .break_ctl = mgsl_break,
        .wait_until_sent = mgsl_wait_until_sent,
        .read_proc = mgsl_read_proc,
        .set_termios = mgsl_set_termios,
        .stop = mgsl_stop,
        .start = mgsl_start,
        .hangup = mgsl_hangup,
        .tiocmget = tiocmget,
        .tiocmset = tiocmset,
};




static int mgsl_init_tty(void)
{
        int rc;

        serial_driver = alloc_tty_driver(128);
        if (!serial_driver)
                return -12;

        serial_driver->owner = ((struct module *)0);
        serial_driver->driver_name = "synclink";
        serial_driver->name = "ttySL";
        serial_driver->major = ttymajor;
        serial_driver->minor_start = 64;
        serial_driver->type = 0x0003;
        serial_driver->subtype = 1;
        serial_driver->init_termios = tty_std_termios;
        serial_driver->init_termios.c_cflag =
                0000015 | 0000060 | 0000200 | 0002000 | 0004000;
        serial_driver->flags = 0x0004;
        tty_set_operations(serial_driver, &mgsl_ops);
        if ((rc = tty_register_driver(serial_driver)) < 0) {
                printk("%s(%d):Couldn't register serial driver\n",
                        "synclink.c",4447);
                put_tty_driver(serial_driver);
                serial_driver = ((void *)0);
                return rc;
        }

        printk("%s %s, tty major#%d\n",
                driver_name, driver_version,
                serial_driver->major);
        return 0;
}



static void mgsl_enum_isa_devices(void)
{
        struct mgsl_struct *info;
        int i;



        for (i=0 ;(i < 10) && io[i] && irq[i]; i++){
                if ( debug_level >= 3 )
                        printk("ISA device specified io=%04X,irq=%d,dma=%d\n",
                                io[i], irq[i], dma[i] );

                info = mgsl_allocate_device();
                if ( !info ) {

                        if ( debug_level >= 2 )
                                printk( "can't allocate device instance data.\n");
                        continue;
                }


                info->io_base = (unsigned int)io[i];
                info->irq_level = (unsigned int)irq[i];
                info->irq_level = irq_canonicalize(info->irq_level);
                info->dma_level = (unsigned int)dma[i];
                info->bus_type = 1;
                info->io_addr_size = 16;
                info->irq_flags = 0;

                mgsl_add_device( info );
        }
}

static void synclink_cleanup(void)
{
        int rc;
        struct mgsl_struct *info;
        struct mgsl_struct *tmp;

        printk("Unloading %s: %s\n", driver_name, driver_version);

        if (serial_driver) {
                if ((rc = tty_unregister_driver(serial_driver)))
                        printk("%s(%d) failed to unregister tty driver err=%d\n",
                               "synclink.c",4505,rc);
                put_tty_driver(serial_driver);
        }

        info = mgsl_device_list;
        while(info) {

                hdlcdev_exit(info);

                mgsl_release_resources(info);
                tmp = info;
                info = info->next_device;
                kfree(tmp);
        }

        if (tmp_buf) {
                free_pages(((unsigned long) tmp_buf),0);
                tmp_buf = ((void *)0);
        }

        if (pci_registered)
                pci_unregister_driver(&synclink_pci_driver);
}

static int __attribute__ ((__section__ (".init.text"))) synclink_init(void)
{
        int rc;

        if (break_on_load) {
                mgsl_get_text_ptr();
                asm("   int $3");;
        }

        printk("%s %s\n", driver_name, driver_version);

        mgsl_enum_isa_devices();
        if ((rc = pci_register_driver(&synclink_pci_driver)) < 0)
                printk("%s:failed to register PCI driver, error=%d\n","synclink.c",rc);
        else
                pci_registered = 1;

        if ((rc = mgsl_init_tty()) < 0)
                goto error;

        return 0;

error:
        synclink_cleanup();
        return rc;
}

static void __attribute__((__unused__)) __attribute__ ((__section__(".exit.text"))) synclink_exit(void)
{
        synclink_cleanup();
}

static initcall_t __initcall_synclink_init __attribute__((__unused__)) __attribute__((__section__(".initcall" "6" ".init"))) = synclink_init;;
static exitcall_t __exitcall_synclink_exit __attribute__((__unused__)) __attribute__ ((__section__ (".exitcall.exit"))) = synclink_exit;;
static void usc_RTCmd( struct mgsl_struct *info, u16 Cmd )
{



        outw( Cmd + info->loopback_bits, info->io_base + 4 );


        if ( info->bus_type == 5 )
                inw( info->io_base + 4 );

}
static void usc_DmaCmd( struct mgsl_struct *info, u16 Cmd )
{

        outw( Cmd + info->mbre_bit, info->io_base );


        if ( info->bus_type == 5 )
                inw( info->io_base );

}
static void usc_OutDmaReg( struct mgsl_struct *info, u16 RegAddr, u16 RegValue )
{



        outw( RegAddr + info->mbre_bit, info->io_base );
        outw( RegValue, info->io_base );


        if ( info->bus_type == 5 )
                inw( info->io_base );

}
static u16 usc_InDmaReg( struct mgsl_struct *info, u16 RegAddr )
{



        outw( RegAddr + info->mbre_bit, info->io_base );
        return inw( info->io_base );

}
static void usc_OutReg( struct mgsl_struct *info, u16 RegAddr, u16 RegValue )
{
        outw( RegAddr + info->loopback_bits, info->io_base + 4 );
        outw( RegValue, info->io_base + 4 );


        if ( info->bus_type == 5 )
                inw( info->io_base + 4 );

}
static u16 usc_InReg( struct mgsl_struct *info, u16 RegAddr )
{
        outw( RegAddr + info->loopback_bits, info->io_base + 4 );
        return inw( info->io_base + 4 );

}
static void usc_set_sdlc_mode( struct mgsl_struct *info )
{
        u16 RegValue;
        int PreSL1660;
        usc_OutReg(info,0x0e,0x1f);
        RegValue=usc_InReg(info,0x0c);
        if ( RegValue == 0x4553 )
                PreSL1660 = 1;
        else
                PreSL1660 = 0;


        if ( info->params.flags & 0x4000 )
        {
           RegValue = 0x8e06;





        }
        else
        {
                if (info->params.mode == 6) {
                        RegValue = 0x0001;

                        usc_OutReg( info, 0x16,
                                (unsigned short)((usc_InReg(info, 0x16) & ~(0x2000|0x1000)) | 0x1000));
                        RegValue |= 0x0400;
                }
                else {

                RegValue = 0x0606;

                if ( info->params.flags & 0x0001 )
                        RegValue |= 0x4000;
                else if ( info->params.flags & 0x0002 )
                        RegValue |= 0x8000;
                else if ( info->params.flags & 0x0004 )
                        RegValue |= 0x8000 + 0x4000;
                }

                if ( info->params.preamble != 0 )
                        RegValue |= 0x2000;
        }

        if ( info->params.mode == 2 &&
                (info->params.flags & 0x0010) )
                RegValue |= 0x1000;

        if ( info->params.addr_filter != 0xff )
        {

                usc_OutReg( info, 0x28, info->params.addr_filter );
                RegValue |= 0x0010;
        }

        usc_OutReg( info, 0x02, RegValue );
        info->cmr_value = RegValue;
        RegValue = 0x0500;

        switch ( info->params.encoding ) {
        case 1: RegValue |= 0x2000; break;
        case 2: RegValue |= 0x4000; break;
        case 3: RegValue |= 0x4000 + 0x2000; break;
        case 4: RegValue |= 0x8000; break;
        case 5: RegValue |= 0x8000 + 0x2000; break;
        case 6: RegValue |= 0x8000 + 0x4000; break;
        case 7: RegValue |= 0x8000 + 0x4000 + 0x2000; break;
        }

        if ( (info->params.crc_type & 0x00ff) == 1 )
                RegValue |= 0x0200;
        else if ( (info->params.crc_type & 0x00ff) == 2 )
                RegValue |= ( 0x1000 | 0x0400 | 0x0200 );

        usc_OutReg( info, 0x22, RegValue );
        usc_OutReg( info, 0x2a, 0xffff );

        usc_OutReg((info), 0x24, (0x7000));
        RegValue = usc_InReg( info, 0x26 ) & 0xc0;

        if ( info->bus_type == 5 )
                usc_OutReg( info, 0x26, (u16)(0x030a | RegValue) );
        else
                usc_OutReg( info, 0x26, (u16)(0x140a | RegValue) );



        usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
        usc_OutReg( (info), 0x1a, 0x40 + (0x0020) );
        RegValue = 0x0400;

        switch ( info->params.encoding ) {
        case 1: RegValue |= 0x2000; break;
        case 2: RegValue |= 0x4000; break;
        case 3: RegValue |= 0x4000 + 0x2000; break;
        case 4: RegValue |= 0x8000; break;
        case 5: RegValue |= 0x8000 + 0x2000; break;
        case 6: RegValue |= 0x8000 + 0x4000; break;
        case 7: RegValue |= 0x8000 + 0x4000 + 0x2000; break;
        }

        if ( (info->params.crc_type & 0x00ff) == 1 )
                RegValue |= 0x0200 + 0x0100;
        else if ( (info->params.crc_type & 0x00ff) == 2 )
                RegValue |= ( 0x1000 | 0x0400 | 0x0200 | 0x0100);

        usc_OutReg( info, 0x32, RegValue );

        usc_set_txidle( info );


        usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x7000)));
        if ( info->bus_type == 5 )
                usc_OutReg( info, 0x36, 0x0736 );
        else
                usc_OutReg( info, 0x36, 0x1436 );

        usc_OutReg( (info), 0x34, (u16)((info)->tcsr_value + ((0x00fa) & 0x00FF)) );
        usc_OutReg( (info), 0x1a, 0x40 + (0x0008) );
        info->tcsr_value = 0;

        if ( !PreSL1660 )
                info->tcsr_value |= 0x0800;

        usc_OutReg( info, 0x34, info->tcsr_value );
        RegValue = 0x0f40;

        if ( info->params.flags & 0x0100 )
                RegValue |= 0x0003;
        else if ( info->params.flags & 0x0200 )
                RegValue |= 0x0004;
        else if ( info->params.flags & 0x8000)
                RegValue |= 0x0006;
        else
                RegValue |= 0x0007;

        if ( info->params.flags & 0x0400 )
                RegValue |= 0x0018;
        else if ( info->params.flags & 0x0800 )
                RegValue |= 0x0020;
        else if ( info->params.flags & 0x0008)
                RegValue |= 0x0038;
        else
                RegValue |= 0x0030;

        usc_OutReg( info, 0x10, RegValue );
        RegValue = 0x0000;

        if ( info->params.flags & (0x0100 + 0x0400) ) {
                u32 XtalSpeed;
                u32 DpllDivisor;
                u16 Tc;




                if ( info->bus_type == 5 )
                        XtalSpeed = 11059200;
                else
                        XtalSpeed = 14745600;

                if ( info->params.flags & 0x2000 ) {
                        DpllDivisor = 16;
                        RegValue |= 0x0400;
                }
                else if ( info->params.flags & 0x1000 ) {
                        DpllDivisor = 8;
                        RegValue |= 0x0800;
                }
                else
                        DpllDivisor = 32;
                if ( info->params.clock_speed )
                {
                        Tc = (u16)((XtalSpeed/DpllDivisor)/info->params.clock_speed);
                        if ( !((((XtalSpeed/DpllDivisor) % info->params.clock_speed) * 2)
                               / info->params.clock_speed) )
                                Tc--;
                }
                else
                        Tc = -1;



                usc_OutReg( info, 0x3e, Tc );

                RegValue |= 0x0010;

                switch ( info->params.encoding ) {
                case 0:
                case 1:
                case 2:
                case 3: RegValue |= 0x0100; break;
                case 4:
                case 5: RegValue |= 0x0200; break;
                case 6:
                case 7: RegValue |= 0x0200 + 0x0100; break;
                }
        }

        usc_OutReg( info, 0x12, RegValue );
        usc_OutReg( info, 0x04, 0x1020 );


        if ( info->params.flags & 0x0020 ) {
                usc_OutReg( info, 0x1e,
                            (u16)(usc_InReg(info,0x1e) | 0x0010) );
        }



        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0x0f00) + 0xb000) );

        usc_OutReg( (info), 0x1a, 0x40 + (0x0020 + 0x0010 + 0x0008 + 0x0004 + 0x0001) );



        usc_OutReg(info, 0x1e, (u16)(usc_InReg(info,0x1e) | 0x0008));
        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0001)) );

        info->mbre_bit = 0;
        outw( 0, info->io_base );
        usc_DmaCmd( info, 0x9000 );
        info->mbre_bit = 0x0100;
        outw( 0x0100, info->io_base );

        if (info->bus_type == 1) {


                usc_OutReg(info, 0x0a, (u16)((usc_InReg(info, 0x0a) | 0x8000) & ~0x4000));
        }
        if ( info->bus_type == 5 ) {

                usc_OutDmaReg( info, 0x06, 0xa00b );
        }
        else
                usc_OutDmaReg( info, 0x06, 0x800b );
        usc_OutDmaReg( info, 0x82, 0xf200 );
        usc_OutDmaReg( info, 0x02, 0xf200 );
        usc_OutDmaReg( info, 0x18, 0x9000 );

        usc_InDmaReg( info, 0x82 );
        usc_InDmaReg( info, 0x02 );
        usc_OutDmaReg( info, 0x1a, 0x0303 );
        RegValue = 0x8080;

        switch ( info->params.preamble_length ) {
        case 1: RegValue |= 0x0400; break;
        case 2: RegValue |= 0x0800; break;
        case 3: RegValue |= 0x0800 + 0x0400; break;
        }

        switch ( info->params.preamble ) {
        case 2: RegValue |= 0x0100 + 0x1000; break;
        case 5: RegValue |= 0x0100; break;
        case 3: RegValue |= 0x0200; break;
        case 4: RegValue |= 0x0200 + 0x0100; break;
        }

        usc_OutReg( info, 0x06, RegValue );
        if ( info->bus_type == 5 ) {

                usc_OutDmaReg( info, 0x12, 0x0000 );
        }
        else
                usc_OutDmaReg( info, 0x12, 0x2000 );

        usc_stop_transmitter(info);
        usc_stop_receiver(info);

}
static void usc_enable_loopback(struct mgsl_struct *info, int enable)
{
        if (enable) {

                usc_OutReg(info,0x16,usc_InReg(info,0x16) | (0x0080 +0x0040));
                usc_OutReg( info, 0x10, 0x0f64 );



                if (info->params.clock_speed) {
                        if (info->bus_type == 5)
                                usc_OutReg(info, 0x2e, (u16)((11059200/info->params.clock_speed)-1));
                        else
                                usc_OutReg(info, 0x2e, (u16)((14745600/info->params.clock_speed)-1));
                } else
                        usc_OutReg(info, 0x2e, (u16)8);



                usc_OutReg( info, 0x12, (u16)((usc_InReg( info, 0x12 ) & ~0x0002) | 0x0001) );


                usc_OutReg(info, 0x16, (u16)((usc_InReg(info, 0x16) & 0xfff8) | 0x0004));


                info->loopback_bits = 0x300;
                outw( 0x0300, info->io_base + 4 );
        } else {

                usc_OutReg(info,0x16,usc_InReg(info,0x16) & ~(0x0080 +0x0040));


                info->loopback_bits = 0;
                outw( 0,info->io_base + 4 );
        }

}
static void usc_enable_aux_clock( struct mgsl_struct *info, u32 data_rate )
{
        u32 XtalSpeed;
        u16 Tc;

        if ( data_rate ) {
                if ( info->bus_type == 5 )
                        XtalSpeed = 11059200;
                else
                        XtalSpeed = 14745600;
                Tc = (u16)(XtalSpeed/data_rate);
                if ( !(((XtalSpeed % data_rate) * 2) / data_rate) )
                        Tc--;


                usc_OutReg( info, 0x2e, Tc );







                usc_OutReg( info, 0x12, (u16)((usc_InReg( info, 0x12 ) & ~0x0002) | 0x0001) );


                usc_OutReg( info, 0x16, (u16)((usc_InReg(info, 0x16) & 0xfff8) | 0x0004) );
        } else {

                usc_OutReg( info, 0x12, (u16)(usc_InReg( info, 0x12 ) & ~0x0001) );
        }

}
static void usc_process_rxoverrun_sync( struct mgsl_struct *info )
{
        int start_index;
        int end_index;
        int frame_start_index;
        int start_of_frame_found = 0;
        int end_of_frame_found = 0;
        int reprogram_dma = 0;

        DMABUFFERENTRY *buffer_list = info->rx_buffer_list;
        u32 phys_addr;

        usc_DmaCmd( info, 0x4200 );
        usc_OutReg((info), 0x24, (0x3000));
        usc_RTCmd( info, 0x4800 );




        frame_start_index = start_index = end_index = info->current_rx_buffer;






        while( !buffer_list[end_index].count )
        {



                if ( !start_of_frame_found )
                {
                        start_of_frame_found = 1;
                        frame_start_index = end_index;
                        end_of_frame_found = 0;
                }

                if ( buffer_list[end_index].status )
                {






                        start_of_frame_found = 0;
                        end_of_frame_found = 1;
                }


                end_index++;
                if ( end_index == info->rx_buffer_count )
                        end_index = 0;

                if ( start_index == end_index )
                {



                        mgsl_reset_rx_dma_buffers( info );
                        frame_start_index = 0;
                        start_of_frame_found = 0;
                        reprogram_dma = 1;
                        break;
                }
        }

        if ( start_of_frame_found && !end_of_frame_found )
        {







                start_index = frame_start_index;

                do
                {
                        *((unsigned long *)&(info->rx_buffer_list[start_index++].count)) = ((1UL << 12));


                        if ( start_index == info->rx_buffer_count )
                                start_index = 0;

                } while( start_index != end_index );

                reprogram_dma = 1;
        }

        if ( reprogram_dma )
        {
                usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
                usc_OutReg( (info), 0x1a, 0x40 + (0x0010|0x0020) );
                usc_OutReg( (info), 0x24, (u16)((0x0010|0x0020) & 0x01f6) );

                usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (0)) );


                usc_OutReg( info, 0x04, (u16)(usc_InReg(info,0x04) | 0x2000) );


                phys_addr = info->rx_buffer_list[frame_start_index].phys_entry;
                usc_OutDmaReg( info, 0xbc, (u16)phys_addr );
                usc_OutDmaReg( info, 0xbe, (u16)(phys_addr >> 16) );

                usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
                usc_OutReg( (info), 0x1a, 0x40 + (0x0010 + 0x0020) );
                usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0020)) );




                usc_OutDmaReg( info, 0x9e, 0x0008 + 0x0004 );
                usc_OutDmaReg( info, 0x18, (u16)(usc_InDmaReg(info,0x18) | 0x0002) );
                usc_DmaCmd( info, 0x7200 );
                if ( info->params.flags & 0x0040 )
                        usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (3)) );
                else
                        usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (2)) );
        }
        else
        {

                usc_OutReg( info, 0x04, (u16)(usc_InReg(info,0x04) | 0x2000) );
                usc_RTCmd( info, 0x4800 );
        }

}
static void usc_stop_receiver( struct mgsl_struct *info )
{
        if (debug_level >= 5)
                printk("%s(%d):usc_stop_receiver(%s)\n",
                         "synclink.c",5578, info->device_name );



        usc_DmaCmd( info, 0x1200 );

        usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
        usc_OutReg( (info), 0x1a, 0x40 + (0x0010 + 0x0020) );
        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0x80 + (0x0010 + 0x0020)) );

        usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (0)) );


        usc_OutReg( info, 0x04, (u16)(usc_InReg(info,0x04) | 0x2000) );
        usc_RTCmd( info, 0x4800 );

        info->rx_enabled = 0;
        info->rx_overflow = 0;
        info->rx_rcc_underrun = 0;

}
static void usc_start_receiver( struct mgsl_struct *info )
{
        u32 phys_addr;

        if (debug_level >= 5)
                printk("%s(%d):usc_start_receiver(%s)\n",
                         "synclink.c",5613, info->device_name );

        mgsl_reset_rx_dma_buffers( info );
        usc_stop_receiver( info );

        usc_OutReg( info, 0x04, (u16)(usc_InReg(info,0x04) | 0x2000) );
        usc_RTCmd( info, 0x4800 );

        if ( info->params.mode == 2 ||
                info->params.mode == 6 ) {





                phys_addr = info->rx_buffer_list[0].phys_entry;
                usc_OutDmaReg( info, 0xbc, (u16)phys_addr );
                usc_OutDmaReg( info, 0xbe, (u16)(phys_addr >> 16) );

                usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
                usc_OutReg( (info), 0x1a, 0x40 + (0x0010 + 0x0020) );
                usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0020)) );




                usc_OutDmaReg( info, 0x9e, 0x0008 + 0x0004 );
                usc_OutDmaReg( info, 0x18, (u16)(usc_InDmaReg(info,0x18) | 0x0002) );
                usc_DmaCmd( info, 0x7200 );
                if ( info->params.flags & 0x0040 )
                        usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (3)) );
                else
                        usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (2)) );
        } else {
                usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
                usc_OutReg( (info), 0x1a, 0x40 + (0x0010 + 0x0020) );
                usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0010)) );

                usc_RTCmd( info, 0x4800 );
                usc_OutReg((info), 0x24, (0x3000));

                usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (2)) );
        }

        usc_OutReg( info, 0x04, 0x1020 );

        info->rx_enabled = 1;

}
static void usc_start_transmitter( struct mgsl_struct *info )
{
        u32 phys_addr;
        unsigned int FrameSize;

        if (debug_level >= 5)
                printk("%s(%d):usc_start_transmitter(%s)\n",
                         "synclink.c",5678, info->device_name );

        if ( info->xmit_cnt ) {





                info->drop_rts_on_tx_done = 0;

                if ( info->params.flags & 0x0080 ) {
                        usc_get_serial_signals( info );
                        if ( !(info->serial_signals & 0x20) ) {
                                info->serial_signals |= 0x20;
                                usc_set_serial_signals( info );
                                info->drop_rts_on_tx_done = 1;
                        }
                }


                if ( info->params.mode == 1 ) {
                        if ( !info->tx_active ) {
                                usc_OutReg( (info), 0x34, (u16)((info)->tcsr_value + ((0x00fa) & 0x00FF)) );
                                usc_OutReg( (info), 0x1a, 0x40 + (0x0008 + 0x0004) );
                                usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0004)) );
                                usc_load_txfifo(info);
                        }
                } else {

                        usc_DmaCmd( info, 0x1000 );




                        FrameSize = info->tx_buffer_list[info->start_tx_dma_buffer].rcc;





                        if ( info->params.mode == 6 )
                                info->tx_buffer_list[info->start_tx_dma_buffer].rcc = 0;



                        usc_OutReg( info, 0x3a, (u16)FrameSize );

                        usc_RTCmd( info, 0x5000 );


                        phys_addr = info->tx_buffer_list[info->start_tx_dma_buffer].phys_entry;
                        usc_OutDmaReg( info, 0x3c, (u16)phys_addr );
                        usc_OutDmaReg( info, 0x3e, (u16)(phys_addr >> 16) );

                        usc_OutReg( (info), 0x34, (u16)((info)->tcsr_value + ((0x00fa) & 0x00FF)) );
                        usc_OutReg( (info), 0x1a, 0x40 + (0x0008) );
                        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0008)) );

                        if ( info->params.mode == 6 &&
                                        info->num_tx_dma_buffers > 1 ) {







                           usc_OutDmaReg( info, 0x1e, 0x0004|0x0008 );
                           usc_OutDmaReg( info, 0x18, (u16)(usc_InDmaReg(info,0x18) | 0x0001) );
                        }


                        usc_DmaCmd( info, 0x7000 );

                        usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x8000)));

                        info->tx_timer.expires = jiffies + msecs_to_jiffies(5000);
                        add_timer(&info->tx_timer);
                }
                info->tx_active = 1;
        }

        if ( !info->tx_enabled ) {
                info->tx_enabled = 1;
                if ( info->params.flags & 0x0020 )
                        usc_OutReg( (info), 0x32, (u16)((usc_InReg((info),0x32) & 0xfffc) | (3)) );
                else
                        usc_OutReg( (info), 0x32, (u16)((usc_InReg((info),0x32) & 0xfffc) | (2)) );
        }

}
static void usc_stop_transmitter( struct mgsl_struct *info )
{
        if (debug_level >= 5)
                printk("%s(%d):usc_stop_transmitter(%s)\n",
                         "synclink.c",5781, info->device_name );

        del_timer(&info->tx_timer);

        usc_OutReg( (info), 0x34, (u16)((info)->tcsr_value + ((0x00fa) & 0x00FF)) );
        usc_OutReg( (info), 0x1a, 0x40 + (0x0008 + 0x0004) );
        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0x80 + (0x0008 + 0x0004)) );

        usc_OutReg( (info), 0x32, (u16)((usc_InReg((info),0x32) & 0xfffc) | (0)) );
        usc_DmaCmd( info, 0x1000 );
        usc_RTCmd( info, 0x5000 );

        info->tx_enabled = 0;
        info->tx_active = 0;

}
static void usc_load_txfifo( struct mgsl_struct *info )
{
        int Fifocount;
        u8 TwoBytes[2];

        if ( !info->xmit_cnt && !info->x_char )
                return;


        usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x5000)));



        while( (Fifocount = usc_InReg(info, 0x36) >> 8) && info->xmit_cnt ) {



                if ( (info->xmit_cnt > 1) && (Fifocount > 1) && !info->x_char ) {


                        TwoBytes[0] = info->xmit_buf[info->xmit_tail++];
                        info->xmit_tail = info->xmit_tail & ((1UL << 12)-1);
                        TwoBytes[1] = info->xmit_buf[info->xmit_tail++];
                        info->xmit_tail = info->xmit_tail & ((1UL << 12)-1);

                        outw( *((u16 *)TwoBytes), info->io_base + 2 + 4);

                        info->xmit_cnt -= 2;
                        info->icount.tx += 2;
                } else {


                        outw( (inw( info->io_base + 4) & 0x0780) | (0x30 +0x40),
                                info->io_base + 4 );

                        if (info->x_char) {

                                outw( info->x_char,info->io_base + 4 );
                                info->x_char = 0;
                        } else {
                                outw( info->xmit_buf[info->xmit_tail++],info->io_base + 4 );
                                info->xmit_tail = info->xmit_tail & ((1UL << 12)-1);
                                info->xmit_cnt--;
                        }
                        info->icount.tx++;
                }
        }

}
static void usc_reset( struct mgsl_struct *info )
{
        if ( info->bus_type == 5 ) {
                int i;
                u32 readval;




                volatile u32 *MiscCtrl = (u32 *)(info->lcr_base + 0x50);
                u32 *LCR0BRDR = (u32 *)(info->lcr_base + 0x28);

                info->misc_ctrl_value |= 0x40000000;
                *MiscCtrl = info->misc_ctrl_value;






                for(i=0;i<10;i++)
                        readval = *MiscCtrl;

                info->misc_ctrl_value &= ~0x40000000;
                *MiscCtrl = info->misc_ctrl_value;

                *LCR0BRDR = (0x00400020 + ((1) << 30) + ((2) << 28) + ((2) << 26) + ((0) << 20) + ((4) << 15) + ((0) << 13) + ((0) << 11) + ((5) << 6) );
        } else {

                outb( 0,info->io_base + 8 );
        }

        info->mbre_bit = 0;
        info->loopback_bits = 0;
        info->usc_idle_mode = 0;
        outw( 0x000c,info->io_base + 4 );


        outw( 0,info->io_base );
        outw( 0,info->io_base + 4 );


        usc_RTCmd( info, 0xb800 );
        usc_OutReg( info, 0x0a, 0xf0f5 );
        usc_OutReg( info, 0x16, 0x0004 );

}
static void usc_set_async_mode( struct mgsl_struct *info )
{
        u16 RegValue;


        usc_OutReg( (info), 0x18, (u16)(usc_InReg((info),0x18) & 0x7f00) );

        outw( 0, info->io_base );
        usc_DmaCmd( info, 0x9000 );

        usc_loopback_frame( info );
        RegValue = 0;
        if ( info->params.stop_bits != 1 )
                RegValue |= 0x4000;
        usc_OutReg( info, 0x02, RegValue );
        RegValue = 0;

        if ( info->params.data_bits != 8 )
                RegValue |= 0x0010 +0x0008 +0x0004;

        if ( info->params.parity != 0 ) {
                RegValue |= 0x0020;
                if ( info->params.parity != 2 )
                        RegValue |= 0x0040;
        }

        usc_OutReg( info, 0x22, RegValue );




        usc_OutReg((info), 0x24, (0x6000));
        usc_OutReg( info, 0x26, 0x0000 );

        usc_OutReg( (info), 0x24, (u16)((0x01f6) & 0x01f6) );
        usc_OutReg( (info), 0x1a, 0x40 + (0x0020) );
        RegValue = 0;

        if ( info->params.data_bits != 8 )
                RegValue |= 0x0010 +0x0008 +0x0004;

        if ( info->params.parity != 0 ) {
                RegValue |= 0x0020;
                if ( info->params.parity != 2 )
                        RegValue |= 0x0040;
        }

        usc_OutReg( info, 0x32, RegValue );

        usc_set_txidle( info );




        usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x6000)));
        usc_OutReg( info, 0x36, 0x1f40 );

        usc_OutReg( (info), 0x34, (u16)((info)->tcsr_value + ((0x00fa) & 0x00FF)) );
        usc_OutReg( (info), 0x1a, 0x40 + (0x0008) );

        usc_enable_async_clock( info, info->params.data_rate );
        usc_OutReg( info, 0x04, 0x0020 );

        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0x80 + (0x0008 + 0x0004 + 0x0010 + 0x0020)) );


        usc_OutReg( (info), 0x1a, 0x40 + (0x0008 + 0x0004 + 0x0010 + 0x0020) );


        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0x0f00) + 0xb000) );

        if (info->bus_type == 1) {


                usc_OutReg(info, 0x0a, (u16)((usc_InReg(info, 0x0a) | 0x2000) & ~0x1000));
        }

        if (info->params.loopback) {
                info->loopback_bits = 0x300;
                outw(0x0300, info->io_base + 4);
        }

}
static void usc_loopback_frame( struct mgsl_struct *info )
{
        int i;
        unsigned long oldmode = info->params.mode;

        info->params.mode = 2;

        usc_OutReg( (info), 0x18, (u16)(usc_InReg((info),0x18) & 0x7f00) );

        usc_set_sdlc_mode( info );
        usc_enable_loopback( info, 1 );


        usc_OutReg( info, 0x2e, 0 );
        usc_OutReg( info, 0x06, 0x0100 );


        usc_RTCmd( info, 0x4800 );
        usc_OutReg( (info), 0x22, (u16)((usc_InReg((info),0x22) & 0xfffc) | (2)) );




        usc_OutReg( info, 0x3a, 2 );
        usc_RTCmd( info, 0x5000 );


        usc_OutReg( (info), 0x34, (u16)((info)->tcsr_value + ((0x00fa) & 0x00FF)) );
        outw(0,info->io_base + 2 + 4);


        usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x8000)));
        usc_OutReg( (info), 0x32, (u16)((usc_InReg((info),0x32) & 0xfffc) | (2)) );


        for (i=0 ; i<1000 ; i++)
                if (usc_InReg( info, 0x24 ) & (0x0100 + 0x0010 + 0x0008 + 0x0002))
                        break;


        usc_enable_loopback(info, 0);

        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0x0f00) + 0xb000) );

        info->params.mode = oldmode;

}






static void usc_set_sync_mode( struct mgsl_struct *info )
{
        usc_loopback_frame( info );
        usc_set_sdlc_mode( info );

        if (info->bus_type == 1) {


                usc_OutReg(info, 0x0a, (u16)((usc_InReg(info, 0x0a) | 0x2000) & ~0x1000));
        }

        usc_enable_aux_clock(info, info->params.clock_speed);

        if (info->params.loopback)
                usc_enable_loopback(info,1);

}






static void usc_set_txidle( struct mgsl_struct *info )
{
        u16 usc_idle_mode = 0x0000;



        switch( info->idle_mode ){
        case 0: usc_idle_mode = 0x0000; break;
        case 1: usc_idle_mode = 0x0100; break;
        case 2: usc_idle_mode = 0x0200; break;
        case 3: usc_idle_mode = 0x0300; break;
        case 4: usc_idle_mode = 0x0500; break;
        case 5: usc_idle_mode = 0x0600; break;
        case 6: usc_idle_mode = 0x0700; break;
        }

        info->usc_idle_mode = usc_idle_mode;

        info->tcsr_value &= ~0x0700;
        info->tcsr_value += usc_idle_mode;
        usc_OutReg(info, 0x34, info->tcsr_value);
        if ( info->params.mode == 6 ) {
                unsigned char syncpat = 0;
                switch( info->idle_mode ) {
                case 0:
                        syncpat = 0x7e;
                        break;
                case 1:
                        syncpat = 0x55;
                        break;
                case 2:
                case 5:
                        syncpat = 0x00;
                        break;
                case 3:
                case 6:
                        syncpat = 0xff;
                        break;
                case 4:
                        syncpat = 0xaa;
                        break;
                }

                usc_OutReg((info), 0x38, (u16)(((u16)syncpat<<8)|(u16)syncpat));
        }

}
static void usc_get_serial_signals( struct mgsl_struct *info )
{
        u16 status;


        info->serial_signals &= 0x80 + 0x20;




        status = usc_InReg( info, 0x1c );



        if ( status & 0x0010 )
                info->serial_signals |= 0x10;

        if ( status & 0x0040 )
                info->serial_signals |= 0x01;

        if ( status & 0x0400 )
                info->serial_signals |= 0x04;

        if ( status & 0x0100 )
                info->serial_signals |= 0x40;

}
static void usc_set_serial_signals( struct mgsl_struct *info )
{
        u16 Control;
        unsigned char V24Out = info->serial_signals;



        Control = usc_InReg( info, 0x0a );

        if ( V24Out & 0x20 )
                Control &= ~(0x0040);
        else
                Control |= 0x0040;

        if ( V24Out & 0x80 )
                Control &= ~(0x0010);
        else
                Control |= 0x0010;

        usc_OutReg( info, 0x0a, Control );

}
static void usc_enable_async_clock( struct mgsl_struct *info, u32 data_rate )
{
        if ( data_rate ) {
                usc_OutReg( info, 0x10, 0x0f64 );
                if ( info->bus_type == 5 )
                        usc_OutReg( info, 0x2e, (u16)((691200/data_rate) - 1) );
                else
                        usc_OutReg( info, 0x2e, (u16)((921600/data_rate) - 1) );
                usc_OutReg( info, 0x12,
                            (u16)((usc_InReg( info, 0x12 ) & ~0x0002) | 0x0001) );




                usc_OutReg( info, 0x16,
                            (u16)((usc_InReg(info, 0x16) & 0xfff8) | 0x0004) );
        } else {

                usc_OutReg( info, 0x12, (u16)(usc_InReg( info, 0x12 ) & ~0x0001) );
        }

}
static void mgsl_reset_tx_dma_buffers( struct mgsl_struct *info )
{
        unsigned int i;

        for ( i = 0; i < info->tx_buffer_count; i++ ) {
                *((unsigned long *)&(info->tx_buffer_list[i].count)) = 0;
        }

        info->current_tx_buffer = 0;
        info->start_tx_dma_buffer = 0;
        info->tx_dma_buffers_used = 0;

        info->get_tx_holding_index = 0;
        info->put_tx_holding_index = 0;
        info->tx_holding_count = 0;

}
static int num_free_tx_dma_buffers(struct mgsl_struct *info)
{
        return info->tx_buffer_count - info->tx_dma_buffers_used;
}
static void mgsl_reset_rx_dma_buffers( struct mgsl_struct *info )
{
        unsigned int i;

        for ( i = 0; i < info->rx_buffer_count; i++ ) {
                *((unsigned long *)&(info->rx_buffer_list[i].count)) = ((1UL << 12));


        }

        info->current_rx_buffer = 0;

}
static void mgsl_free_rx_frame_buffers( struct mgsl_struct *info, unsigned int StartIndex, unsigned int EndIndex )
{
        int Done = 0;
        DMABUFFERENTRY *pBufEntry;
        unsigned int Index;




        Index = StartIndex;

        while( !Done ) {
                pBufEntry = &(info->rx_buffer_list[Index]);

                if ( Index == EndIndex ) {

                        Done = 1;
                }




                *((unsigned long *)&(pBufEntry->count)) = ((1UL << 12));


                Index++;
                if ( Index == info->rx_buffer_count )
                        Index = 0;
        }


        info->current_rx_buffer = Index;

}
static int mgsl_get_rx_frame(struct mgsl_struct *info)
{
        unsigned int StartIndex, EndIndex;
        unsigned short status;
        DMABUFFERENTRY *pBufEntry;
        unsigned int framesize = 0;
        int ReturnCode = 0;
        unsigned long flags;
        struct tty_struct *tty = info->tty;
        int return_frame = 0;
        StartIndex = EndIndex = info->current_rx_buffer;

        while( !info->rx_buffer_list[EndIndex].status ) {







                if ( info->rx_buffer_list[EndIndex].count )
                        goto Cleanup;


                EndIndex++;
                if ( EndIndex == info->rx_buffer_count )
                        EndIndex = 0;


                if ( EndIndex == StartIndex ) {





                        if ( info->rx_enabled ){
                                flags = _spin_lock_irqsave(&info->irq_spinlock);
                                usc_start_receiver(info);
                                _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                        }
                        goto Cleanup;
                }
        }




        status = info->rx_buffer_list[EndIndex].status;

        if ( status & (0x0100 + 0x0002 +
                        0x0008 + 0x0004) ) {
                if ( status & 0x0100 )
                        info->icount.rxshort++;
                else if ( status & 0x0004 )
                        info->icount.rxabort++;
                else if ( status & 0x0002 )
                        info->icount.rxover++;
                else {
                        info->icount.rxcrc++;
                        if ( info->params.crc_type & 0x8000 )
                                return_frame = 1;
                }
                framesize = 0;

                {
                        struct net_device_stats *stats = hdlc_stats(info->netdev);
                        stats->rx_errors++;
                        stats->rx_frame_errors++;
                }

        } else
                return_frame = 1;

        if ( return_frame ) {






                framesize = 0xffff - info->rx_buffer_list[EndIndex].rcc;


                if ( info->params.crc_type == 1 )
                        framesize -= 2;
                else if ( info->params.crc_type == 2 )
                        framesize -= 4;
        }

        if ( debug_level >= 4 )
                printk("%s(%d):mgsl_get_rx_frame(%s) status=%04X size=%d\n",
                        "synclink.c",6718,info->device_name,status,framesize);

        if ( debug_level >= 1 )
                mgsl_trace_block(info,info->rx_buffer_list[StartIndex].virt_addr,
                        ({ int __x = (framesize); int __y = (((1UL << 12))); __x < __y ? __x: __y; }),0);

        if (framesize) {
                if ( ( (info->params.crc_type & 0x8000) &&
                                ((framesize+1) > info->max_frame_size) ) ||
                        (framesize > info->max_frame_size) )
                        info->icount.rxlong++;
                else {

                        int copy_count = framesize;
                        int index = StartIndex;
                        unsigned char *ptmp = info->intermediate_rxbuffer;

                        if ( !(status & 0x0008))
                        info->icount.rxok++;

                        while(copy_count) {
                                int partial_count;
                                if ( copy_count > ((1UL << 12)) )
                                        partial_count = ((1UL << 12));
                                else
                                        partial_count = copy_count;

                                pBufEntry = &(info->rx_buffer_list[index]);
                                (__builtin_constant_p(partial_count) ? __constant_memcpy((ptmp),(pBufEntry->virt_addr),(partial_count)) : __memcpy((ptmp),(pBufEntry->virt_addr),(partial_count)));
                                ptmp += partial_count;
                                copy_count -= partial_count;

                                if ( ++index == info->rx_buffer_count )
                                        index = 0;
                        }

                        if ( info->params.crc_type & 0x8000 ) {
                                ++framesize;
                                *ptmp = (status & 0x0008 ?
                                                1 :
                                                0);

                                if ( debug_level >= 1 )
                                        printk("%s(%d):mgsl_get_rx_frame(%s) rx frame status=%d\n",
                                                "synclink.c",6762,info->device_name,
                                                *ptmp);
                        }


                        if (info->netcount)
                                hdlcdev_rx(info,info->intermediate_rxbuffer,framesize);
                        else

                                ldisc_receive_buf(tty, info->intermediate_rxbuffer, info->flag_buf, framesize);
                }
        }

        mgsl_free_rx_frame_buffers( info, StartIndex, EndIndex );

        ReturnCode = 1;

Cleanup:

        if ( info->rx_enabled && info->rx_overflow ) {





                if ( !info->rx_buffer_list[EndIndex].status &&
                        info->rx_buffer_list[EndIndex].count ) {
                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        usc_start_receiver(info);
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                }
        }

        return ReturnCode;

}
static int mgsl_get_raw_rx_frame(struct mgsl_struct *info)
{
        unsigned int CurrentIndex, NextIndex;
        unsigned short status;
        DMABUFFERENTRY *pBufEntry;
        unsigned int framesize = 0;
        int ReturnCode = 0;
        unsigned long flags;
        struct tty_struct *tty = info->tty;
        CurrentIndex = NextIndex = info->current_rx_buffer;
        ++NextIndex;
        if ( NextIndex == info->rx_buffer_count )
                NextIndex = 0;

        if ( info->rx_buffer_list[CurrentIndex].status != 0 ||
                (info->rx_buffer_list[CurrentIndex].count == 0 &&
                        info->rx_buffer_list[NextIndex].count == 0)) {







                status = info->rx_buffer_list[CurrentIndex].status;

                if ( status & (0x0100 + 0x0002 +
                                0x0008 + 0x0004) ) {
                        if ( status & 0x0100 )
                                info->icount.rxshort++;
                        else if ( status & 0x0004 )
                                info->icount.rxabort++;
                        else if ( status & 0x0002 )
                                info->icount.rxover++;
                        else
                                info->icount.rxcrc++;
                        framesize = 0;
                } else {
                        if ( status ) {
                                if ( info->rx_buffer_list[CurrentIndex].rcc )
                                        framesize = 0xffff - info->rx_buffer_list[CurrentIndex].rcc;
                                else
                                        framesize = ((1UL << 12));
                        }
                        else
                                framesize = ((1UL << 12));
                }

                if ( framesize > ((1UL << 12)) ) {






                        framesize = framesize % ((1UL << 12));
                }


                if ( debug_level >= 4 )
                        printk("%s(%d):mgsl_get_raw_rx_frame(%s) status=%04X size=%d\n",
                                "synclink.c",6929,info->device_name,status,framesize);

                if ( debug_level >= 1 )
                        mgsl_trace_block(info,info->rx_buffer_list[CurrentIndex].virt_addr,
                                ({ int __x = (framesize); int __y = (((1UL << 12))); __x < __y ? __x: __y; }),0);

                if (framesize) {



                        pBufEntry = &(info->rx_buffer_list[CurrentIndex]);
                        (__builtin_constant_p(framesize) ? __constant_memcpy((info->intermediate_rxbuffer),(pBufEntry->virt_addr),(framesize)) : __memcpy((info->intermediate_rxbuffer),(pBufEntry->virt_addr),(framesize)));
                        info->icount.rxok++;

                        ldisc_receive_buf(tty, info->intermediate_rxbuffer, info->flag_buf, framesize);
                }


                mgsl_free_rx_frame_buffers( info, CurrentIndex, CurrentIndex );

                ReturnCode = 1;
        }


        if ( info->rx_enabled && info->rx_overflow ) {





                if ( !info->rx_buffer_list[CurrentIndex].status &&
                        info->rx_buffer_list[CurrentIndex].count ) {
                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        usc_start_receiver(info);
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                }
        }

        return ReturnCode;

}
static void mgsl_load_tx_dma_buffer(struct mgsl_struct *info,
                const char *Buffer, unsigned int BufferSize)
{
        unsigned short Copycount;
        unsigned int i = 0;
        DMABUFFERENTRY *pBufEntry;

        if ( debug_level >= 1 )
                mgsl_trace_block(info,Buffer, ({ int __x = (BufferSize); int __y = (((1UL << 12))); __x < __y ? __x: __y; }), 1);

        if (info->params.flags & 0x4000) {



                info->cmr_value |= 0x2000;
        }





        i = info->current_tx_buffer;
        info->start_tx_dma_buffer = i;




        info->tx_buffer_list[i].status = info->cmr_value & 0xf000;
        info->tx_buffer_list[i].rcc = BufferSize;
        info->tx_buffer_list[i].count = BufferSize;




        while( BufferSize ){

                pBufEntry = &info->tx_buffer_list[i++];

                if ( i == info->tx_buffer_count )
                        i=0;



                if ( BufferSize > ((1UL << 12)) )
                        Copycount = ((1UL << 12));
                else
                        Copycount = BufferSize;



                if ( info->bus_type == 5 )
                        mgsl_load_pci_memory(pBufEntry->virt_addr, Buffer,Copycount);
                else
                        (__builtin_constant_p(Copycount) ? __constant_memcpy((pBufEntry->virt_addr),(Buffer),(Copycount)) : __memcpy((pBufEntry->virt_addr),(Buffer),(Copycount)));

                pBufEntry->count = Copycount;


                Buffer += Copycount;
                BufferSize -= Copycount;

                ++info->tx_dma_buffers_used;
        }


        info->current_tx_buffer = i;

}
static int mgsl_register_test( struct mgsl_struct *info )
{
        static unsigned short BitPatterns[] =
                { 0x0000, 0xffff, 0xaaaa, 0x5555, 0x1234, 0x6969, 0x9696, 0x0f0f };
        static unsigned int Patterncount = sizeof(BitPatterns)/sizeof(unsigned short);
        unsigned int i;
        int rc = 1;
        unsigned long flags;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_reset(info);



        if ( (usc_InReg( info, 0x1e ) != 0) ||
                  (usc_InReg( info, 0x14 ) != 0) ||
                  (usc_InDmaReg( info, 0x14 ) != 0) ){
                rc = 0;
        }

        if ( rc == 1 ){



                for ( i = 0 ; i < Patterncount ; i++ ) {
                        usc_OutReg( info, 0x2e, BitPatterns[i] );
                        usc_OutReg( info, 0x3e, BitPatterns[(i+1)%Patterncount] );
                        usc_OutReg( info, 0x3a, BitPatterns[(i+2)%Patterncount] );
                        usc_OutReg( info, 0x2a, BitPatterns[(i+3)%Patterncount] );
                        usc_OutReg( info, 0x28, BitPatterns[(i+4)%Patterncount] );
                        usc_OutDmaReg( info, 0x2a, BitPatterns[(i+5)%Patterncount] );

                        if ( (usc_InReg( info, 0x2e ) != BitPatterns[i]) ||
                                  (usc_InReg( info, 0x3e ) != BitPatterns[(i+1)%Patterncount]) ||
                                  (usc_InReg( info, 0x3a ) != BitPatterns[(i+2)%Patterncount]) ||
                                  (usc_InReg( info, 0x2a ) != BitPatterns[(i+3)%Patterncount]) ||
                                  (usc_InReg( info, 0x28 ) != BitPatterns[(i+4)%Patterncount]) ||
                                  (usc_InDmaReg( info, 0x2a ) != BitPatterns[(i+5)%Patterncount]) ){
                                rc = 0;
                                break;
                        }
                }
        }

        usc_reset(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        return rc;

}






static int mgsl_irq_test( struct mgsl_struct *info )
{
        unsigned long EndTime;
        unsigned long flags;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_reset(info);






        info->irq_occurred = 0;





        usc_OutReg( info, 0x0a, (unsigned short)((usc_InReg(info, 0x0a) | 0x2000) & ~0x1000) );

        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0x0f00) + 0xb000) );
        usc_OutReg( (info), 0x18, (u16)((usc_InReg((info),0x18) & 0xff00) + 0xc0 + (0x0002)) );
        usc_OutReg( (info), 0x1a, 0x40 + (0x0002) );

        usc_OutReg((info),0x1c,(u16)((0x2000) & 0xaaa0));
        usc_OutReg( (info), 0x1e, (u16)(usc_InReg((info),0x1e) | (0x2000 + 0x1000)) );

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        EndTime=100;
        while( EndTime-- && !info->irq_occurred ) {
                msleep_interruptible(10);
        }

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_reset(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        if ( !info->irq_occurred )
                return 0;
        else
                return 1;

}
static int mgsl_dma_test( struct mgsl_struct *info )
{
        unsigned short FifoLevel;
        unsigned long phys_addr;
        unsigned int FrameSize;
        unsigned int i;
        char *TmpPtr;
        int rc = 1;
        unsigned short status=0;
        unsigned long EndTime;
        unsigned long flags;
        MGSL_PARAMS tmp_params;


        (__builtin_constant_p(sizeof(MGSL_PARAMS)) ? __constant_memcpy((&tmp_params),(&info->params),(sizeof(MGSL_PARAMS))) : __memcpy((&tmp_params),(&info->params),(sizeof(MGSL_PARAMS))));

        (__builtin_constant_p(sizeof(MGSL_PARAMS)) ? __constant_memcpy((&info->params),(&default_params),(sizeof(MGSL_PARAMS))) : __memcpy((&info->params),(&default_params),(sizeof(MGSL_PARAMS))));



        flags = _spin_lock_irqsave(&info->irq_spinlock);



        usc_reset(info);
        usc_set_sdlc_mode(info);
        usc_enable_loopback(info,1);
        usc_OutDmaReg( info, 0x82, 0xe200 );

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);




        FrameSize = 40;




        info->tx_buffer_list[0].count = FrameSize;
        info->tx_buffer_list[0].rcc = FrameSize;
        info->tx_buffer_list[0].status = 0x4000;



        TmpPtr = info->tx_buffer_list[0].virt_addr;
        for (i = 0; i < FrameSize; i++ )
                *TmpPtr++ = i;




        info->rx_buffer_list[0].status = 0;
        info->rx_buffer_list[0].count = FrameSize + 4;



        (__builtin_constant_p(0) ? (__builtin_constant_p((FrameSize + 4)) ? __constant_c_and_count_memset(((info->rx_buffer_list[0].virt_addr)),((0x01010101UL*(unsigned char)(0))),((FrameSize + 4))) : __constant_c_memset(((info->rx_buffer_list[0].virt_addr)),((0x01010101UL*(unsigned char)(0))),((FrameSize + 4)))) : (__builtin_constant_p((FrameSize + 4)) ? __memset_generic((((info->rx_buffer_list[0].virt_addr))),(((0))),(((FrameSize + 4)))) : __memset_generic(((info->rx_buffer_list[0].virt_addr)),((0)),((FrameSize + 4)))));




        info->tx_buffer_list[1].count = 0;
        info->rx_buffer_list[1].count = 0;






        flags = _spin_lock_irqsave(&info->irq_spinlock);


        usc_RTCmd( info, 0x4800 );


        phys_addr = info->rx_buffer_list[0].phys_entry;
        usc_OutDmaReg( info, 0xbc, (unsigned short)phys_addr );
        usc_OutDmaReg( info, 0xbe, (unsigned short)(phys_addr >> 16) );


        usc_InDmaReg( info, 0x82 );
        usc_DmaCmd( info, 0x7200 );


        usc_OutReg( info, 0x22, (unsigned short)((usc_InReg(info, 0x22) & 0xfffc) | 0x0002) );

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);







        EndTime = jiffies + msecs_to_jiffies(100);

        for(;;) {
                if ((({ unsigned long __dummy; typeof(jiffies) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ({ unsigned long __dummy; typeof(EndTime) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ((long)(EndTime) - (long)(jiffies) < 0))) {
                        rc = 0;
                        break;
                }

                flags = _spin_lock_irqsave(&info->irq_spinlock);
                status = usc_InDmaReg( info, 0x82 );
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);

                if ( !(status & 0x0010) && (status & 0x0020) ) {



                        break;
                }
        }






        flags = _spin_lock_irqsave(&info->irq_spinlock);




        usc_OutReg( info, 0x3a, (unsigned short)info->tx_buffer_list[0].count );
        usc_RTCmd( info, 0x5000 );



        phys_addr = info->tx_buffer_list[0].phys_entry;
        usc_OutDmaReg( info, 0x3c, (unsigned short)phys_addr );
        usc_OutDmaReg( info, 0x3e, (unsigned short)(phys_addr >> 16) );



        usc_OutReg( info, 0x34, (unsigned short)(( usc_InReg(info, 0x34) & 0x0f00) | 0xfa) );
        usc_DmaCmd( info, 0x7000 );



        usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x5000)));

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);







        EndTime = jiffies + msecs_to_jiffies(100);

        for(;;) {
                if ((({ unsigned long __dummy; typeof(jiffies) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ({ unsigned long __dummy; typeof(EndTime) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ((long)(EndTime) - (long)(jiffies) < 0))) {
                        rc = 0;
                        break;
                }

                flags = _spin_lock_irqsave(&info->irq_spinlock);
                FifoLevel = usc_InReg(info, 0x36) >> 8;
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);

                if ( FifoLevel < 16 )
                        break;
                else
                        if ( FrameSize < 32 ) {


                                if ( FifoLevel <= (32 - FrameSize) )
                                        break;
                        }
        }


        if ( rc == 1 )
        {


                flags = _spin_lock_irqsave(&info->irq_spinlock);


                usc_OutReg((info), 0x34, (u16)((info)->tcsr_value + (0x8000)));
                usc_OutReg( info, 0x32, (unsigned short)((usc_InReg(info, 0x32) & 0xfffc) | 0x0002) );

                _spin_unlock_irqrestore(&info->irq_spinlock, flags);







                EndTime = jiffies + msecs_to_jiffies(100);



                flags = _spin_lock_irqsave(&info->irq_spinlock);
                status = usc_InReg( info, 0x34 );
                _spin_unlock_irqrestore(&info->irq_spinlock, flags);

                while ( !(status & (0x0040 +0x0020 +0x0010 +0x0004 +0x0002)) ) {
                        if ((({ unsigned long __dummy; typeof(jiffies) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ({ unsigned long __dummy; typeof(EndTime) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ((long)(EndTime) - (long)(jiffies) < 0))) {
                                rc = 0;
                                break;
                        }

                        flags = _spin_lock_irqsave(&info->irq_spinlock);
                        status = usc_InReg( info, 0x34 );
                        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
                }
        }


        if ( rc == 1 ){

                if ( status & (0x0020 + 0x0002) )
                        rc = 0;
        }

        if ( rc == 1 ) {



                EndTime = jiffies + msecs_to_jiffies(100);


                status=info->rx_buffer_list[0].status;
                while ( status == 0 ) {
                        if ((({ unsigned long __dummy; typeof(jiffies) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ({ unsigned long __dummy; typeof(EndTime) __dummy2; (void)(&__dummy == &__dummy2); 1; }) && ((long)(EndTime) - (long)(jiffies) < 0))) {
                                rc = 0;
                                break;
                        }
                        status=info->rx_buffer_list[0].status;
                }
        }


        if ( rc == 1 ) {

                status = info->rx_buffer_list[0].status;

                if ( status & (0x0100 + 0x0008 + 0x0002) ) {

                        rc = 0;
                } else {
                        if ( __builtin_memcmp( info->tx_buffer_list[0].virt_addr ,
                                info->rx_buffer_list[0].virt_addr, FrameSize ) ){
                                rc = 0;
                        }
                }
        }

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_reset( info );
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);


        (__builtin_constant_p(sizeof(MGSL_PARAMS)) ? __constant_memcpy((&info->params),(&tmp_params),(sizeof(MGSL_PARAMS))) : __memcpy((&info->params),(&tmp_params),(sizeof(MGSL_PARAMS))));

        return rc;

}
static int mgsl_adapter_test( struct mgsl_struct *info )
{
        if ( debug_level >= 3 )
                printk( "%s(%d):Testing device %s\n",
                        "synclink.c",7467,info->device_name );

        if ( !mgsl_register_test( info ) ) {
                info->init_error = 1;
                printk( "%s(%d):Register test failure for device %s Addr=%04X\n",
                        "synclink.c",7472,info->device_name, (unsigned short)(info->io_base) );
                return -19;
        }

        if ( !mgsl_irq_test( info ) ) {
                info->init_error = 3;
                printk( "%s(%d):Interrupt test failure for device %s IRQ=%d\n",
                        "synclink.c",7479,info->device_name, (unsigned short)(info->irq_level) );
                return -19;
        }

        if ( !mgsl_dma_test( info ) ) {
                info->init_error = 5;
                printk( "%s(%d):DMA test failure for device %s DMA=%d\n",
                        "synclink.c",7486,info->device_name, (unsigned short)(info->dma_level) );
                return -19;
        }

        if ( debug_level >= 3 )
                printk( "%s(%d):device %s passed diagnostics\n",
                        "synclink.c",7492,info->device_name );

        return 0;

}
static int mgsl_memory_test( struct mgsl_struct *info )
{
        static unsigned long BitPatterns[] = { 0x0, 0x55555555, 0xaaaaaaaa,
                                                                                        0x66666666, 0x99999999, 0xffffffff, 0x12345678 };
        unsigned long Patterncount = sizeof(BitPatterns)/sizeof(unsigned long);
        unsigned long i;
        unsigned long TestLimit = 0x40000/sizeof(unsigned long);
        unsigned long * TestAddr;

        if ( info->bus_type != 5 )
                return 1;

        TestAddr = (unsigned long *)info->memory_base;



        for ( i = 0 ; i < Patterncount ; i++ ) {
                *TestAddr = BitPatterns[i];
                if ( *TestAddr != BitPatterns[i] )
                        return 0;
        }




        for ( i = 0 ; i < TestLimit ; i++ ) {
                *TestAddr = i * 4;
                TestAddr++;
        }

        TestAddr = (unsigned long *)info->memory_base;

        for ( i = 0 ; i < TestLimit ; i++ ) {
                if ( *TestAddr != i * 4 )
                        return 0;
                TestAddr++;
        }

        (__builtin_constant_p(0) ? (__builtin_constant_p((0x40000)) ? __constant_c_and_count_memset(((info->memory_base)),((0x01010101UL*(unsigned char)(0))),((0x40000))) : __constant_c_memset(((info->memory_base)),((0x01010101UL*(unsigned char)(0))),((0x40000)))) : (__builtin_constant_p((0x40000)) ? __memset_generic((((info->memory_base))),(((0))),(((0x40000)))) : __memset_generic(((info->memory_base)),((0)),((0x40000)))));

        return 1;

}
static void mgsl_load_pci_memory( char* TargetPtr, const char* SourcePtr,
        unsigned short count )
{



        unsigned short Intervalcount = count / 64;
        unsigned short Index;
        unsigned long Dummy;

        for ( Index = 0 ; Index < Intervalcount ; Index++ )
        {
                (__builtin_constant_p(64) ? __constant_memcpy((TargetPtr),(SourcePtr),(64)) : __memcpy((TargetPtr),(SourcePtr),(64)));
                Dummy = *((volatile unsigned long *)TargetPtr);
                TargetPtr += 64;
                SourcePtr += 64;
        }

        (__builtin_constant_p(count % 64) ? __constant_memcpy((TargetPtr),(SourcePtr),(count % 64)) : __memcpy((TargetPtr),(SourcePtr),(count % 64)));

}

static void mgsl_trace_block(struct mgsl_struct *info,const char* data, int count, int xmit)
{
        int i;
        int linecount;
        if (xmit)
                printk("%s tx data:\n",info->device_name);
        else
                printk("%s rx data:\n",info->device_name);

        while(count) {
                if (count > 16)
                        linecount = 16;
                else
                        linecount = count;

                for(i=0;i<linecount;i++)
                        printk("%02X ",(unsigned char)data[i]);
                for(;i<17;i++)
                        printk("   ");
                for(i=0;i<linecount;i++) {
                        if (data[i]>=040 && data[i]<=0176)
                                printk("%c",data[i]);
                        else
                                printk(".");
                }
                printk("\n");

                data += linecount;
                count -= linecount;
        }
}
static void mgsl_tx_timeout(unsigned long context)
{
        struct mgsl_struct *info = (struct mgsl_struct*)context;
        unsigned long flags;

        if ( debug_level >= 3 )
                printk( "%s(%d):mgsl_tx_timeout(%s)\n",
                        "synclink.c",7655,info->device_name);
        if(info->tx_active &&
           (info->params.mode == 2 ||
            info->params.mode == 6) ) {
                info->icount.txtimeout++;
        }
        flags = _spin_lock_irqsave(&info->irq_spinlock);
        info->tx_active = 0;
        info->xmit_cnt = info->xmit_head = info->xmit_tail = 0;

        if ( info->params.flags & 0x4000 )
                usc_loopmode_cancel_transmit( info );

        _spin_unlock_irqrestore(&info->irq_spinlock, flags);


        if (info->netcount)
                hdlcdev_tx_done(info);
        else

                mgsl_bh_transmit(info);

}





static int mgsl_loopmode_send_done( struct mgsl_struct * info )
{
        unsigned long flags;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if (info->params.flags & 0x4000) {
                if (info->tx_active)
                        info->loopmode_send_done_requested = 1;
                else
                        usc_loopmode_send_done(info);
        }
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        return 0;
}




static void usc_loopmode_send_done( struct mgsl_struct * info )
{
        info->loopmode_send_done_requested = 0;

        info->cmr_value &= ~0x2000;
        usc_OutReg(info, 0x02, info->cmr_value);
}



static void usc_loopmode_cancel_transmit( struct mgsl_struct * info )
{

        usc_RTCmd( info, 0x5000 );
        usc_DmaCmd( info, 0x1000 );
        usc_loopmode_send_done( info );
}





static void usc_loopmode_insert_request( struct mgsl_struct * info )
{
        info->loopmode_insert_requested = 1;




        usc_OutReg( info, 0x26,
                (usc_InReg( info, 0x26 ) | 0x0020 ) );


        info->cmr_value |= 0x2000;
        usc_OutReg(info, 0x02, info->cmr_value);
}



static int usc_loopmode_active( struct mgsl_struct * info)
{
        return usc_InReg( info, 0x04 ) & 0x0080 ? 1 : 0 ;
}
static int hdlcdev_attach(struct net_device *dev, unsigned short encoding,
                          unsigned short parity)
{
        struct mgsl_struct *info = (dev_to_hdlc(dev)->priv);
        unsigned char new_encoding;
        unsigned short new_crctype;


        if (info->count)
                return -16;

        switch (encoding)
        {
        case 1: new_encoding = 0; break;
        case 2: new_encoding = 3; break;
        case 3: new_encoding = 4; break;
        case 4: new_encoding = 5; break;
        case 5: new_encoding = 6; break;
        default: return -22;
        }

        switch (parity)
        {
        case 1: new_crctype = 0; break;
        case 5: new_crctype = 1; break;
        case 7: new_crctype = 2; break;
        default: return -22;
        }

        info->params.encoding = new_encoding;
        info->params.crc_type = new_crctype;;


        if (info->netcount)
                mgsl_program_hw(info);

        return 0;
}
static int hdlcdev_xmit(struct sk_buff *skb, struct net_device *dev)
{
        struct mgsl_struct *info = (dev_to_hdlc(dev)->priv);
        struct net_device_stats *stats = hdlc_stats(dev);
        unsigned long flags;

        if (debug_level >= 3)
                printk("<6>" "%s:hdlc_xmit(%s)\n","synclink.c",dev->name);


        netif_stop_queue(dev);


        info->xmit_cnt = skb->len;
        mgsl_load_tx_dma_buffer(info, skb->data, skb->len);


        stats->tx_packets++;
        stats->tx_bytes += skb->len;


        kfree_skb(skb);


        dev->trans_start = jiffies;


        flags = _spin_lock_irqsave(&info->irq_spinlock);
        if (!info->tx_active)
                usc_start_transmitter(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        return 0;
}
static int hdlcdev_open(struct net_device *dev)
{
        struct mgsl_struct *info = (dev_to_hdlc(dev)->priv);
        int rc;
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s:hdlcdev_open(%s)\n","synclink.c",dev->name);


        if ((rc = hdlc_open(dev)))
                return rc;


        flags = _spin_lock_irqsave(&info->netlock);
        if (info->count != 0 || info->netcount != 0) {
                printk("<4>" "%s: hdlc_open returning busy\n", dev->name);
                _spin_unlock_irqrestore(&info->netlock, flags);
                return -16;
        }
        info->netcount=1;
        _spin_unlock_irqrestore(&info->netlock, flags);


        if ((rc = startup(info)) != 0) {
                flags = _spin_lock_irqsave(&info->netlock);
                info->netcount=0;
                _spin_unlock_irqrestore(&info->netlock, flags);
                return rc;
        }


        info->serial_signals |= 0x20 + 0x80;
        mgsl_program_hw(info);


        dev->trans_start = jiffies;
        netif_start_queue(dev);


        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_get_serial_signals(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);
        hdlc_set_carrier(info->serial_signals & 0x01, dev);

        return 0;
}
static int hdlcdev_close(struct net_device *dev)
{
        struct mgsl_struct *info = (dev_to_hdlc(dev)->priv);
        unsigned long flags;

        if (debug_level >= 3)
                printk("%s:hdlcdev_close(%s)\n","synclink.c",dev->name);

        netif_stop_queue(dev);


        shutdown(info);

        hdlc_close(dev);

        flags = _spin_lock_irqsave(&info->netlock);
        info->netcount=0;
        _spin_unlock_irqrestore(&info->netlock, flags);

        return 0;
}
static int hdlcdev_ioctl(struct net_device *dev, struct ifreq *ifr, int cmd)
{
        const size_t size = sizeof(sync_serial_settings);
        sync_serial_settings new_line;
        sync_serial_settings *line = ifr->ifr_ifru.ifru_settings.ifs_ifsu.sync;
        struct mgsl_struct *info = (dev_to_hdlc(dev)->priv);
        unsigned int flags;

        if (debug_level >= 3)
                printk("%s:hdlcdev_ioctl(%s)\n","synclink.c",dev->name);


        if (info->count)
                return -16;

        if (cmd != 0x894A)
                return hdlc_ioctl(dev, ifr, cmd);

        switch(ifr->ifr_ifru.ifru_settings.type) {
        case 0x0001:

                ifr->ifr_ifru.ifru_settings.type = 0x1005;
                if (ifr->ifr_ifru.ifru_settings.size < size) {
                        ifr->ifr_ifru.ifru_settings.size = size;
                        return -105;
                }

                flags = info->params.flags & (0x0000 | 0x0100 |
                                              0x0200 | 0x8000 |
                                              0x0000 | 0x0400 |
                                              0x0800 | 0x0008);

                switch (flags){
                case (0x0000 | 0x0000): new_line.clock_type = 1; break;
                case (0x0200 | 0x0800): new_line.clock_type = 2; break;
                case (0x0000 | 0x0800): new_line.clock_type = 3; break;
                case (0x0000 | 0x0008): new_line.clock_type = 4; break;
                default: new_line.clock_type = 0;
                }

                new_line.clock_rate = info->params.clock_speed;
                new_line.loopback = info->params.loopback ? 1:0;

                if (copy_to_user(line, &new_line, size))
                        return -14;
                return 0;

        case 0x1005:

                if(!capable(12))
                        return -1;
                if (copy_from_user(&new_line, line, size))
                        return -14;

                switch (new_line.clock_type)
                {
                case 1: flags = 0x0000 | 0x0000; break;
                case 4: flags = 0x0000 | 0x0008; break;
                case 2: flags = 0x0200 | 0x0800; break;
                case 3: flags = 0x0000 | 0x0800; break;
                case 0: flags = info->params.flags &
                                             (0x0000 | 0x0100 |
                                              0x0200 | 0x8000 |
                                              0x0000 | 0x0400 |
                                              0x0800 | 0x0008); break;
                default: return -22;
                }

                if (new_line.loopback != 0 && new_line.loopback != 1)
                        return -22;

                info->params.flags &= ~(0x0000 | 0x0100 |
                                        0x0200 | 0x8000 |
                                        0x0000 | 0x0400 |
                                        0x0800 | 0x0008);
                info->params.flags |= flags;

                info->params.loopback = new_line.loopback;

                if (flags & (0x0200 | 0x0800))
                        info->params.clock_speed = new_line.clock_rate;
                else
                        info->params.clock_speed = 0;


                if (info->netcount)
                        mgsl_program_hw(info);
                return 0;

        default:
                return hdlc_ioctl(dev, ifr, cmd);
        }
}






static void hdlcdev_tx_timeout(struct net_device *dev)
{
        struct mgsl_struct *info = (dev_to_hdlc(dev)->priv);
        struct net_device_stats *stats = hdlc_stats(dev);
        unsigned long flags;

        if (debug_level >= 3)
                printk("hdlcdev_tx_timeout(%s)\n",dev->name);

        stats->tx_errors++;
        stats->tx_aborted_errors++;

        flags = _spin_lock_irqsave(&info->irq_spinlock);
        usc_stop_transmitter(info);
        _spin_unlock_irqrestore(&info->irq_spinlock, flags);

        netif_wake_queue(dev);
}







static void hdlcdev_tx_done(struct mgsl_struct *info)
{
        if (netif_queue_stopped(info->netdev))
                netif_wake_queue(info->netdev);
}
static void hdlcdev_rx(struct mgsl_struct *info, char *buf, int size)
{
        struct sk_buff *skb = dev_alloc_skb(size);
        struct net_device *dev = info->netdev;
        struct net_device_stats *stats = hdlc_stats(dev);

        if (debug_level >= 3)
                printk("hdlcdev_rx(%s)\n",dev->name);

        if (skb == ((void *)0)) {
                printk("<5>" "%s: can't alloc skb, dropping packet\n", dev->name);
                stats->rx_dropped++;
                return;
        }

        (__builtin_constant_p(size) ? __constant_memcpy((skb_put(skb, size)),(buf),(size)) : __memcpy((skb_put(skb, size)),(buf),(size)));

        skb->protocol = hdlc_type_trans(skb, info->netdev);

        stats->rx_packets++;
        stats->rx_bytes += size;

        netif_rx(skb);

        info->netdev->last_rx = jiffies;
}
static int hdlcdev_init(struct mgsl_struct *info)
{
        int rc;
        struct net_device *dev;
        hdlc_device *hdlc;



        if (!(dev = alloc_hdlcdev(info))) {
                printk("<3>" "%s:hdlc device allocation failure\n","synclink.c");
                return -12;
        }


        dev->base_addr = info->io_base;
        dev->irq = info->irq_level;
        dev->dma = info->dma_level;


        dev->do_ioctl = hdlcdev_ioctl;
        dev->open = hdlcdev_open;
        dev->stop = hdlcdev_close;
        dev->tx_timeout = hdlcdev_tx_timeout;
        dev->watchdog_timeo = 10*250;
        dev->tx_queue_len = 50;


        hdlc = dev_to_hdlc(dev);
        hdlc->attach = hdlcdev_attach;
        hdlc->xmit = hdlcdev_xmit;


        if ((rc = register_hdlc_device(dev))) {
                printk("<4>" "%s:unable to register hdlc device\n","synclink.c");
                free_netdev(dev);
                return rc;
        }

        info->netdev = dev;
        return 0;
}







static void hdlcdev_exit(struct mgsl_struct *info)
{
        unregister_hdlc_device(info->netdev);
        free_netdev(info->netdev);
        info->netdev = ((void *)0);
}




static int synclink_init_one (struct pci_dev *dev,
                                        const struct pci_device_id *ent)
{
        struct mgsl_struct *info;

        if (pci_enable_device(dev)) {
                printk("error enabling pci device %p\n", dev);
                return -5;
        }

        if (!(info = mgsl_allocate_device())) {
                printk("can't allocate device instance data.\n");
                return -5;
        }



        info->io_base = ((dev)->resource[(2)].start);
        info->irq_level = dev->irq;
        info->phys_memory_base = ((dev)->resource[(3)].start);





        info->phys_lcr_base = ((dev)->resource[(0)].start);
        info->lcr_offset = info->phys_lcr_base & ((1UL << 12)-1);
        info->phys_lcr_base &= ~((1UL << 12)-1);

        info->bus_type = 5;
        info->io_addr_size = 8;
        info->irq_flags = 0x04000000;

        if (dev->device == 0x0210) {

                info->misc_ctrl_value = 0x007c4080;
                info->hw_version = 1;
        } else {





                info->misc_ctrl_value = 0x087e4546;
                info->hw_version = 0;
        }

        mgsl_add_device(info);

        return 0;
}

static void synclink_remove_one (struct pci_dev *dev)
{
}
struct tty_struct tty;
struct file filp;
void * f(void *arg) {
  //mgsl_enum_isa_devices();
  //mgsl_init_tty();
  //synclink_cleanup();
  synclink_init();
  mgsl_open(&tty, &filp);
  //synclink_exit();
  return 0;
}

typedef int pthread_t;
typedef int pthread_attr_t;

int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                   void * (*start_routine) (void*), void *arg);

int main() {
  pthread_t t;
  pthread_create(&t, 0, f, 0);
  pthread_create(&t, 0, f, 0);
}
