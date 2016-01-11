typedef struct {
        unsigned long fds_bits [(1024/(8 * sizeof(unsigned long)))];
} __kernel_fd_set;
typedef void (*__kernel_sighandler_t)(int);
typedef int __kernel_key_t;
typedef unsigned short __kernel_dev_t;
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
typedef int __kernel_daddr_t;
typedef char * __kernel_caddr_t;
typedef unsigned short __kernel_uid16_t;
typedef unsigned short __kernel_gid16_t;
typedef unsigned int __kernel_uid32_t;
typedef unsigned int __kernel_gid32_t;
typedef unsigned short __kernel_old_uid_t;
typedef unsigned short __kernel_old_gid_t;
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
        unsigned long arg0, arg1, arg2;
};
extern long do_no_restart_syscall(struct restart_block *parm);
static __inline__ void set_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btsl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static __inline__ void __set_bit(int nr, volatile unsigned long * addr)
{
        __asm__(
                "btsl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static __inline__ void clear_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btrl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static __inline__ void __clear_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__(
                "btrl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static __inline__ void __change_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__(
                "btcl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static __inline__ void change_bit(int nr, volatile unsigned long * addr)
{
        __asm__ __volatile__( "lock ; "
                "btcl %1,%0"
                :"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
}
static __inline__ int test_and_set_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;
        __asm__ __volatile__( "lock ; "
                "btsl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static __inline__ int __test_and_set_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;
        __asm__(
                "btsl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
        return oldbit;
}
static __inline__ int test_and_clear_bit(int nr, volatile unsigned long * addr)
{
        int oldbit;
        __asm__ __volatile__( "lock ; "
                "btrl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static __inline__ int __test_and_clear_bit(int nr, volatile unsigned long *addr)
{
        int oldbit;
        __asm__(
                "btrl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr));
        return oldbit;
}
static __inline__ int __test_and_change_bit(int nr, volatile unsigned long *addr)
{
        int oldbit;
        __asm__ __volatile__(
                "btcl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static __inline__ int test_and_change_bit(int nr, volatile unsigned long* addr)
{
        int oldbit;
        __asm__ __volatile__( "lock ; "
                "btcl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit),"=m" ((*(volatile long *) addr))
                :"Ir" (nr) : "memory");
        return oldbit;
}
static __inline__ int constant_test_bit(int nr, const volatile unsigned long * addr)
{
        return ((1UL << (nr & 31)) & (((const volatile unsigned int *) addr)[nr >> 5])) != 0;
}
static __inline__ int variable_test_bit(int nr, const volatile unsigned long * addr)
{
        int oldbit;
        __asm__ __volatile__(
                "btl %2,%1\n\tsbbl %0,%0"
                :"=r" (oldbit)
                :"m" ((*(volatile long *) addr)),"Ir" (nr));
        return oldbit;
}
static __inline__ int find_first_zero_bit(unsigned long * addr, unsigned size)
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
                :"1" ((size + 31) >> 5), "2" (addr), "b" (addr));
        return res;
}
static __inline__ int find_first_bit(unsigned long * addr, unsigned size)
{
        int d0, d1;
        int res;
        __asm__ __volatile__(
                "xorl %%eax,%%eax\n\t"
                "repe; scasl\n\t"
                "jz 1f\n\t"
                "leal -4(%%edi),%%edi\n\t"
                "bsfl (%%edi),%%eax\n"
                "1:\tsubl %%ebx,%%edi\n\t"
                "shll $3,%%edi\n\t"
                "addl %%edi,%%eax"
                :"=a" (res), "=&c" (d0), "=&D" (d1)
                :"1" ((size + 31) >> 5), "2" (addr), "b" (addr));
        return res;
}
static __inline__ int find_next_zero_bit(unsigned long * addr, int size, int offset)
{
        unsigned long * p = ((unsigned long *) addr) + (offset >> 5);
        int set = 0, bit = offset & 31, res;
        if (bit) {
                __asm__("bsfl %1,%0\n\t"
                        "jne 1f\n\t"
                        "movl $32, %0\n"
                        "1:"
                        : "=r" (set)
                        : "r" (~(*p >> bit)));
                if (set < (32 - bit))
                        return set + offset;
                set = 32 - bit;
                p++;
        }
        res = find_first_zero_bit (p, size - 32 * (p - (unsigned long *) addr));
        return (offset + set + res);
}
static __inline__ int find_next_bit(unsigned long *addr, int size, int offset)
{
        unsigned long * p = addr + (offset >> 5);
        int set = 0, bit = offset & 31, res;
        if (bit) {
                __asm__("bsfl %1,%0\n\t"
                        "jne 1f\n\t"
                        "movl $32, %0\n"
                        "1:"
                        : "=r" (set)
                        : "r" (*p >> bit));
                if (set < (32 - bit))
                        return set + offset;
                set = 32 - bit;
                p++;
        }
        res = find_first_bit (p, size - 32 * (p - addr));
        return (offset + set + res);
}
static __inline__ unsigned long ffz(unsigned long word)
{
        __asm__("bsfl %1,%0"
                :"=r" (word)
                :"r" (~word));
        return word;
}
static __inline__ unsigned long __ffs(unsigned long word)
{
        __asm__("bsfl %1,%0"
                :"=r" (word)
                :"rm" (word));
        return word;
}
static inline int sched_find_first_bit(unsigned long *b)
{
        if (__builtin_expect((b[0]),0))
                return __ffs(b[0]);
        if (__builtin_expect((b[1]),0))
                return __ffs(b[1]) + 32;
        if (__builtin_expect((b[2]),0))
                return __ffs(b[2]) + 64;
        if (b[3])
                return __ffs(b[3]) + 96;
        return __ffs(b[4]) + 128;
}
static __inline__ int ffs(int x)
{
        int r;
        __asm__("bsfl %1,%0\n\t"
                "jnz 1f\n\t"
                "movl $-1,%0\n"
                "1:" : "=r" (r) : "g" (x));
        return r+1;
}
static inline int generic_ffs(int x)
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
extern __inline__ int generic_fls(int x)
{
        int r = 32;
        if (!x)
                return 0;
        if (!(x & 0xffff0000)) {
                x <<= 16;
                r -= 16;
        }
        if (!(x & 0xff000000)) {
                x <<= 8;
                r -= 8;
        }
        if (!(x & 0xf0000000)) {
                x <<= 4;
                r -= 4;
        }
        if (!(x & 0xc0000000)) {
                x <<= 2;
                r -= 2;
        }
        if (!(x & 0x80000000)) {
                x <<= 1;
                r -= 1;
        }
        return r;
}
extern __inline__ int get_bitmask_order(unsigned int count)
{
        int order;
        order = generic_fls(count);
        return order;
}
static inline unsigned int generic_hweight32(unsigned int w)
{
        unsigned int res = (w & 0x55555555) + ((w >> 1) & 0x55555555);
        res = (res & 0x33333333) + ((res >> 2) & 0x33333333);
        res = (res & 0x0F0F0F0F) + ((res >> 4) & 0x0F0F0F0F);
        res = (res & 0x00FF00FF) + ((res >> 8) & 0x00FF00FF);
        return (res & 0x0000FFFF) + ((res >> 16) & 0x0000FFFF);
}
static inline unsigned int generic_hweight16(unsigned int w)
{
        unsigned int res = (w & 0x5555) + ((w >> 1) & 0x5555);
        res = (res & 0x3333) + ((res >> 2) & 0x3333);
        res = (res & 0x0F0F) + ((res >> 4) & 0x0F0F);
        return (res & 0x00FF) + ((res >> 8) & 0x00FF);
}
static inline unsigned int generic_hweight8(unsigned int w)
{
        unsigned int res = (w & 0x55) + ((w >> 1) & 0x55);
        res = (res & 0x33) + ((res >> 2) & 0x33);
        return (res & 0x0F) + ((res >> 4) & 0x0F);
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
int save_i387_soft(void *s387, struct _fpstate * buf);
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
typedef struct { unsigned long pte_low, pte_high; } pte_t;
typedef struct { unsigned long long pmd; } pmd_t;
typedef struct { unsigned long long pgd; } pgd_t;
typedef struct { unsigned long pgprot; } pgprot_t;
static __inline__ int get_order(unsigned long size)
{
        int order;
        size = (size-1) >> (12 -1);
        order = -1;
        do {
                size >>= 1;
                order++;
        } while (size);
        return order;
}
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
static __inline__ __const__ __u32 ___arch__swab32(__u32 x)
{
        __asm__("bswap %0" : "=r" (x) : "0" (x));
        return x;
}
static __inline__ __const__ __u16 ___arch__swab16(__u16 x)
{
        __asm__("xchgb %b0,%h0"
                : "=q" (x)
                : "0" (x));
                return x;
}
static inline __u64 ___arch__swab64(__u64 val)
{
        union {
                struct { __u32 a,b; } s;
                __u64 u;
        } v;
        v.u = val;
        asm("bswapl %0 ; bswapl %1 ; xchgl %0,%1"
            : "=r" (v.s.a), "=r" (v.s.b)
            : "0" (v.s.a), "1" (v.s.b));
        return v.u;
}
static __inline__ __const__ __u16 __fswab16(__u16 x)
{
        return ___arch__swab16(x);
}
static __inline__ __u16 __swab16p(__u16 *x)
{
        return ___arch__swab16(*(x));
}
static __inline__ void __swab16s(__u16 *addr)
{
        do { *(addr) = ___arch__swab16(*((addr))); } while (0);
}
static __inline__ __const__ __u32 __fswab32(__u32 x)
{
        return ___arch__swab32(x);
}
static __inline__ __u32 __swab32p(__u32 *x)
{
        return ___arch__swab32(*(x));
}
static __inline__ void __swab32s(__u32 *addr)
{
        do { *(addr) = ___arch__swab32(*((addr))); } while (0);
}
static __inline__ __const__ __u64 __fswab64(__u64 x)
{
        return ___arch__swab64(x);
}
static __inline__ __u64 __swab64p(__u64 *x)
{
        return ___arch__swab64(*(x));
}
static __inline__ void __swab64s(__u64 *addr)
{
        do { *(addr) = ___arch__swab64(*((addr))); } while (0);
}
extern __u32 ntohl(__u32);
extern __u32 htonl(__u32);
extern unsigned short int ntohs(unsigned short int);
extern unsigned short int htons(unsigned short int);
extern int console_printk[];
struct completion;
extern struct notifier_block *panic_notifier_list;
 void panic(const char * fmt, ...)
        __attribute__ ((noreturn, format (printf, 1, 2)));
 __attribute__((regparm(0))) void do_exit(long error_code)
        __attribute__((noreturn));
 void complete_and_exit(struct completion *, long)
        __attribute__((noreturn));
extern int abs(int);
extern unsigned long simple_strtoul(const char *,char **,unsigned int);
extern long simple_strtol(const char *,char **,unsigned int);
extern unsigned long long simple_strtoull(const char *,char **,unsigned int);
extern long long simple_strtoll(const char *,char **,unsigned int);
extern int sprintf(char * buf, const char * fmt, ...)
        __attribute__ ((format (printf, 2, 3)));
extern int vsprintf(char *buf, const char *, va_list);
extern int snprintf(char * buf, size_t size, const char * fmt, ...)
        __attribute__ ((format (printf, 3, 4)));
extern int vsnprintf(char *buf, size_t size, const char *fmt, va_list args);
extern int sscanf(const char *, const char *, ...)
        __attribute__ ((format (scanf,2,3)));
extern int vsscanf(const char *, const char *, va_list);
extern int get_option(char **str, int *pint);
extern char *get_options(const char *str, int nints, int *ints);
extern unsigned long long memparse(char *ptr, char **retptr);
extern void dev_probe_lock(void);
extern void dev_probe_unlock(void);
extern int kernel_text_address(unsigned long addr);
extern int session_of_pgrp(int pgrp);
 __attribute__((regparm(0))) int printk(const char * fmt, ...)
        __attribute__ ((format (printf, 1, 2)));
static inline void console_silent(void)
{
        (console_printk[0]) = 0;
}
static inline void console_verbose(void)
{
        if ((console_printk[0]))
                (console_printk[0]) = 15;
}
extern void bust_spinlocks(int yes);
extern int oops_in_progress;
extern int tainted;
extern const char *print_tainted(void);
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
extern void BUILD_BUG(void);
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
        unsigned long x86_capability[4];
        char x86_vendor_id[16];
        char x86_model_id[64];
        int x86_cache_size;
        int fdiv_bug;
        int f00f_bug;
        int coma_bug;
        unsigned long loops_per_jiffy;
} __attribute__((__aligned__((1 << (5)))));
extern struct cpuinfo_x86 boot_cpu_data;
extern struct cpuinfo_x86 new_cpu_data;
extern struct tss_struct init_tss[8];
extern struct cpuinfo_x86 cpu_data[];
extern char ignore_irq13;
extern void identify_cpu(struct cpuinfo_x86 *);
extern void print_cpu_info(struct cpuinfo_x86 *);
extern void dodgy_tsc(void);
static inline void cpuid(int op, int *eax, int *ebx, int *ecx, int *edx)
{
        __asm__("cpuid"
                : "=a" (*eax),
                  "=b" (*ebx),
                  "=c" (*ecx),
                  "=d" (*edx)
                : "0" (op));
}
static inline unsigned int cpuid_eax(unsigned int op)
{
        unsigned int eax;
        __asm__("cpuid"
                : "=a" (eax)
                : "0" (op)
                : "bx", "cx", "dx");
        return eax;
}
static inline unsigned int cpuid_ebx(unsigned int op)
{
        unsigned int eax, ebx;
        __asm__("cpuid"
                : "=a" (eax), "=b" (ebx)
                : "0" (op)
                : "cx", "dx" );
        return ebx;
}
static inline unsigned int cpuid_ecx(unsigned int op)
{
        unsigned int eax, ecx;
        __asm__("cpuid"
                : "=a" (eax), "=c" (ecx)
                : "0" (op)
                : "bx", "dx" );
        return ecx;
}
static inline unsigned int cpuid_edx(unsigned int op)
{
        unsigned int eax, edx;
        __asm__("cpuid"
                : "=a" (eax), "=d" (edx)
                : "0" (op)
                : "bx", "cx");
        return edx;
}
extern unsigned long mmu_cr4_features;
static inline void set_in_cr4 (unsigned long mask)
{
        mmu_cr4_features |= mask;
        __asm__("movl %%cr4,%%eax\n\t"
                "orl %0,%%eax\n\t"
                "movl %%eax,%%cr4\n"
                : : "irg" (mask)
                :"ax");
}
static inline void clear_in_cr4 (unsigned long mask)
{
        mmu_cr4_features &= ~mask;
        __asm__("movl %%cr4,%%eax\n\t"
                "andl %0,%%eax\n\t"
                "movl %%eax,%%cr4\n"
                : : "irg" (~mask)
                :"ax");
}
extern int EISA_bus;
extern int MCA_bus;
extern unsigned int machine_id;
extern unsigned int machine_submodel_id;
extern unsigned int BIOS_revision;
extern unsigned int mca_pentium_flag;
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
        long reserved;
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
        unsigned short trace, bitmap;
        unsigned long io_bitmap[32 +1];
        unsigned long __cacheline_filler[5];
};
struct thread_struct {
        struct desc_struct tls_array[3];
        unsigned long esp0;
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
        unsigned long *ts_io_bitmap;
};
static inline void load_esp0(struct tss_struct *tss, unsigned long esp0)
{
        tss->esp0 = esp0;
        if ((__builtin_constant_p((0*32+11)) ? constant_test_bit(((0*32+11)),(boot_cpu_data.x86_capability)) : variable_test_bit(((0*32+11)),(boot_cpu_data.x86_capability)))) {
                __asm__ __volatile__("wrmsr" : : "c" (0x174), "a" (((12 + 0) * 8)), "d" (0));
                __asm__ __volatile__("wrmsr" : : "c" (0x175), "a" (esp0), "d" (0));
        }
}
static inline void disable_sysenter(void)
{
        if ((__builtin_constant_p((0*32+11)) ? constant_test_bit(((0*32+11)),(boot_cpu_data.x86_capability)) : variable_test_bit(((0*32+11)),(boot_cpu_data.x86_capability))))
                __asm__ __volatile__("wrmsr" : : "c" (0x174), "a" (0), "d" (0));
}
struct task_struct;
struct mm_struct;
extern void release_thread(struct task_struct *);
extern int kernel_thread(int (*fn)(void *), void * arg, unsigned long flags);
extern unsigned long thread_saved_pc(struct task_struct *tsk);
unsigned long get_wchan(struct task_struct *p);
struct microcode {
        unsigned int hdrver;
        unsigned int rev;
        unsigned int date;
        unsigned int sig;
        unsigned int cksum;
        unsigned int ldrver;
        unsigned int pf;
        unsigned int reserved[5];
        unsigned int bits[500];
};
static inline void rep_nop(void)
{
        __asm__ __volatile__("rep;nop": : :"memory");
}
struct thread_info {
        struct task_struct *task;
        struct exec_domain *exec_domain;
        unsigned long flags;
        __u32 cpu;
        __s32 preempt_count;
        mm_segment_t addr_limit;
        struct restart_block restart_block;
        __u8 supervisor_stack[0];
};
static inline struct thread_info *current_thread_info(void)
{
        struct thread_info *ti;
        __asm__("andl %%esp,%0; ":"=r" (ti) : "0" (~8191UL));
        return ti;
}
static inline void set_thread_flag(int flag)
{
        set_bit(flag,&current_thread_info()->flags);
}
static inline void clear_thread_flag(int flag)
{
        clear_bit(flag,&current_thread_info()->flags);
}
static inline int test_and_set_thread_flag(int flag)
{
        return test_and_set_bit(flag,&current_thread_info()->flags);
}
static inline int test_and_clear_thread_flag(int flag)
{
        return test_and_clear_bit(flag,&current_thread_info()->flags);
}
static inline int test_thread_flag(int flag)
{
        return (__builtin_constant_p(flag) ? constant_test_bit((flag),(&current_thread_info()->flags)) : variable_test_bit((flag),(&current_thread_info()->flags)));
}
static inline void set_ti_thread_flag(struct thread_info *ti, int flag)
{
        set_bit(flag,&ti->flags);
}
static inline void clear_ti_thread_flag(struct thread_info *ti, int flag)
{
        clear_bit(flag,&ti->flags);
}
static inline int test_and_set_ti_thread_flag(struct thread_info *ti, int flag)
{
        return test_and_set_bit(flag,&ti->flags);
}
static inline int test_and_clear_ti_thread_flag(struct thread_info *ti, int flag)
{
        return test_and_clear_bit(flag,&ti->flags);
}
static inline int test_ti_thread_flag(struct thread_info *ti, int flag)
{
        return (__builtin_constant_p(flag) ? constant_test_bit((flag),(&ti->flags)) : variable_test_bit((flag),(&ti->flags)));
}
static inline void set_need_resched(void)
{
        set_thread_flag(3);
}
static inline void clear_need_resched(void)
{
        clear_thread_flag(3);
}
struct task_struct;
extern void __switch_to(struct task_struct *prev, struct task_struct *next) __attribute__((regparm(3)));
static inline unsigned long _get_base(char * addr)
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
static inline unsigned long get_limit(unsigned long segment)
{
        unsigned long __limit;
        __asm__("lsll %1,%0"
                :"=r" (__limit):"r" (segment));
        return __limit+1;
}
struct __xchg_dummy { unsigned long a[100]; };
static inline void __set_64bit (unsigned long long * ptr,
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
static inline void __set_64bit_constant (unsigned long long *ptr,
                                                 unsigned long long value)
{
        __set_64bit(ptr,(unsigned int)(value), (unsigned int)((value)>>32ULL));
}
static inline void __set_64bit_var (unsigned long long *ptr,
                         unsigned long long value)
{
        __set_64bit(ptr,*(((unsigned int*)&(value))+0), *(((unsigned int*)&(value))+1));
}
static inline unsigned long __xchg(unsigned long x, volatile void * ptr, int size)
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
static inline unsigned long __cmpxchg(volatile void *ptr, unsigned long old,
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
void disable_hlt(void);
void enable_hlt(void);
extern unsigned long dmi_broken;
extern int is_sony_vaio_laptop;
typedef struct { volatile int counter; } atomic_t;
static __inline__ void atomic_add(int i, atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "addl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}
static __inline__ void atomic_sub(int i, atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "subl %1,%0"
                :"=m" (v->counter)
                :"ir" (i), "m" (v->counter));
}
static __inline__ int atomic_sub_and_test(int i, atomic_t *v)
{
        unsigned char c;
        __asm__ __volatile__(
                "lock ; " "subl %2,%0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"ir" (i), "m" (v->counter) : "memory");
        return c;
}
static __inline__ void atomic_inc(atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "incl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}
static __inline__ void atomic_dec(atomic_t *v)
{
        __asm__ __volatile__(
                "lock ; " "decl %0"
                :"=m" (v->counter)
                :"m" (v->counter));
}
static __inline__ int atomic_dec_and_test(atomic_t *v)
{
        unsigned char c;
        __asm__ __volatile__(
                "lock ; " "decl %0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"m" (v->counter) : "memory");
        return c != 0;
}
static __inline__ int atomic_inc_and_test(atomic_t *v)
{
        unsigned char c;
        __asm__ __volatile__(
                "lock ; " "incl %0; sete %1"
                :"=m" (v->counter), "=qm" (c)
                :"m" (v->counter) : "memory");
        return c != 0;
}
static __inline__ int atomic_add_negative(int i, atomic_t *v)
{
        unsigned char c;
        __asm__ __volatile__(
                "lock ; " "addl %2,%0; sets %1"
                :"=m" (v->counter), "=qm" (c)
                :"ir" (i), "m" (v->counter) : "memory");
        return c;
}
extern int printk(const char * fmt, ...)
        __attribute__ ((format (printf, 1, 2)));
typedef struct {
        volatile unsigned int lock;
} spinlock_t;
static inline void _raw_spin_unlock(spinlock_t *lock)
{
        char oldval = 1;
}
static inline int _raw_spin_trylock(spinlock_t *lock)
{
        char oldval;
        __asm__ __volatile__(
                "xchgb %b0,%1"
                :"=q" (oldval), "=m" (lock->lock)
                :"0" (0) : "memory");
        return oldval > 0;
}
static inline void _raw_spin_lock(spinlock_t *lock)
{
}
typedef struct {
        volatile unsigned int lock;
} rwlock_t;
static inline void _raw_read_lock(rwlock_t *rw)
{
        do { if (__builtin_constant_p(rw)) asm volatile("lock ; " "subl $1,%0\n\t" "js 2f\n" "1:\n" ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t" "2:\tpushl %%eax\n\t" "leal %0,%%eax\n\t" "call " "__read_lock_failed" "\n\t" "popl %%eax\n\t" "jmp 1b\n" ".previous\n\t" :"=m" (*(volatile int *)rw) : : "memory"); else asm volatile("lock ; " "subl $1,(%0)\n\t" "js 2f\n" "1:\n" ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t" "2:\tcall " "__read_lock_failed" "\n\t" "jmp 1b\n" ".previous\n\t" ::"a" (rw) : "memory"); } while (0);
}
static inline void _raw_write_lock(rwlock_t *rw)
{
        do { if (__builtin_constant_p(rw)) asm volatile("lock ; " "subl $" "0x01000000" ",%0\n\t" "jnz 2f\n" "1:\n" ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t" "2:\tpushl %%eax\n\t" "leal %0,%%eax\n\t" "call " "__write_lock_failed" "\n\t" "popl %%eax\n\t" "jmp 1b\n" ".previous\n\t" :"=m" (*(volatile int *)rw) : : "memory"); else asm volatile("lock ; " "subl $" "0x01000000" ",(%0)\n\t" "jnz 2f\n" "1:\n" ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t" "2:\tcall " "__write_lock_failed" "\n\t" "jmp 1b\n" ".previous\n\t" ::"a" (rw) : "memory"); } while (0);
}
static inline int _raw_write_trylock(rwlock_t *lock)
{
        atomic_t *count = (atomic_t *)lock;
        if (atomic_sub_and_test(0x01000000, count))
                return 1;
        atomic_add(0x01000000, count);
        return 0;
}
extern int atomic_dec_and_lock(atomic_t *atomic, spinlock_t *lock);
extern spinlock_t task_capability_lock;
typedef __u32 kernel_cap_t;
extern kernel_cap_t cap_bset;
static inline kernel_cap_t cap_combine(kernel_cap_t a, kernel_cap_t b)
{
     kernel_cap_t dest;
     (dest) = (a) | (b);
     return dest;
}
static inline kernel_cap_t cap_intersect(kernel_cap_t a, kernel_cap_t b)
{
     kernel_cap_t dest;
     (dest) = (a) & (b);
     return dest;
}
static inline kernel_cap_t cap_drop(kernel_cap_t a, kernel_cap_t drop)
{
     kernel_cap_t dest;
     (dest) = (a) & ~(drop);
     return dest;
}
static inline kernel_cap_t cap_invert(kernel_cap_t c)
{
     kernel_cap_t dest;
     (dest) = ~(c);
     return dest;
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
typedef struct {
        unsigned sequence;
        spinlock_t lock;
} seqlock_t;
static inline void write_seqlock(seqlock_t *sl)
{
        do { do { } while (0); _raw_spin_lock(&sl->lock); } while(0);
        ++sl->sequence;
        __asm__ __volatile__ ("": : :"memory");
}
static inline void write_sequnlock(seqlock_t *sl)
{
        __asm__ __volatile__ ("": : :"memory");
        sl->sequence++;
        do { _raw_spin_unlock(&sl->lock); do { } while (0); } while (0);
}
static inline int write_tryseqlock(seqlock_t *sl)
{
        int ret = ({do { } while (0); _raw_spin_trylock(&sl->lock) ? 1 : ({do { } while (0); 0;});});
        if (ret) {
                ++sl->sequence;
                __asm__ __volatile__ ("": : :"memory");
        }
        return ret;
}
static inline unsigned read_seqbegin(const seqlock_t *sl)
{
        unsigned ret = sl->sequence;
        __asm__ __volatile__ ("lock; addl $0,0(%%esp)": : :"memory");
        return ret;
}
static inline int read_seqretry(const seqlock_t *sl, unsigned iv)
{
        __asm__ __volatile__ ("lock; addl $0,0(%%esp)": : :"memory");
        return (iv & 1) | (sl->sequence ^ iv);
}
static __inline__ unsigned long
timespec_to_jiffies(struct timespec *value)
{
        unsigned long sec = value->tv_sec;
        long nsec = value->tv_nsec;
        if (sec >= (((~0UL >> 1)-1) / 1000))
                return ((~0UL >> 1)-1);
        nsec += 1000000000L / 1000 - 1;
        nsec /= 1000000000L / 1000;
        return 1000 * sec + nsec;
}
static __inline__ void
jiffies_to_timespec(unsigned long jiffies, struct timespec *value)
{
        value->tv_nsec = (jiffies % 1000) * (1000000000L / 1000);
        value->tv_sec = jiffies / 1000;
}
static __inline__ unsigned long
timeval_to_jiffies(struct timeval *value)
{
        unsigned long sec = value->tv_sec;
        long usec = value->tv_usec;
        if (sec >= (((~0UL >> 1)-1) / 1000))
                return ((~0UL >> 1)-1);
        usec += 1000000L / 1000 - 1;
        usec /= 1000000L / 1000;
        return 1000 * sec + usec;
}
static __inline__ void
jiffies_to_timeval(unsigned long jiffies, struct timeval *value)
{
        value->tv_usec = (jiffies % 1000) * (1000000L / 1000);
        value->tv_sec = jiffies / 1000;
}
static __inline__ int timespec_equal(struct timespec *a, struct timespec *b)
{
        return (a->tv_sec == b->tv_sec) && (a->tv_nsec == b->tv_nsec);
}
static inline unsigned long
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
extern seqlock_t xtime_lock;
static inline unsigned long get_seconds(void)
{
        return xtime.tv_sec;
}
struct timespec current_kernel_time(void);
extern void do_gettimeofday(struct timeval *tv);
extern void do_settimeofday(struct timeval *tv);
extern long do_nanosleep(struct timespec *t);
extern long do_utimes(char * filename, struct timeval * times);
struct itimerspec {
        struct timespec it_interval;
        struct timespec it_value;
};
struct itimerval {
        struct timeval it_interval;
        struct timeval it_value;
};
typedef unsigned long long cycles_t;
extern cycles_t cacheflush_time;
static inline cycles_t get_cycles (void)
{
        unsigned long long ret;
        __asm__ __volatile__("rdtsc" : "=A" (ret));
        return ret;
}
extern unsigned long cpu_khz;
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
extern long time_phase;
extern long time_freq;
extern long time_adj;
extern long time_reftime;
extern long time_adjust;
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
extern u64 jiffies_64;
extern unsigned long volatile jiffies;
u64 get_jiffies_64(void);
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
extern void rb_replace_node(struct rb_node *victim, struct rb_node *new,
                            struct rb_root *root);
static inline void rb_link_node(struct rb_node * node, struct rb_node * parent,
                                struct rb_node ** rb_link)
{
        node->rb_parent = parent;
        node->rb_color = 0;
        node->rb_left = node->rb_right = ((void *)0);
        *rb_link = node;
}
static inline void prefetch(const void *x) {;}
static inline void prefetchw(const void *x) {;}
struct list_head {
        struct list_head *next, *prev;
};
static inline void __list_add(struct list_head *new,
                              struct list_head *prev,
                              struct list_head *next)
{
        next->prev = new;
        new->next = next;
        new->prev = prev;
        prev->next = new;
}
static inline void list_add(struct list_head *new, struct list_head *head)
{
        __list_add(new, head, head->next);
}
static inline void list_add_tail(struct list_head *new, struct list_head *head)
{
        __list_add(new, head->prev, head);
}
static __inline__ void __list_add_rcu(struct list_head * new,
        struct list_head * prev,
        struct list_head * next)
{
        new->next = next;
        new->prev = prev;
        __asm__ __volatile__ ("": : :"memory");
        next->prev = new;
        prev->next = new;
}
static __inline__ void list_add_rcu(struct list_head *new, struct list_head *head)
{
        __list_add_rcu(new, head, head->next);
}
static __inline__ void list_add_tail_rcu(struct list_head *new, struct list_head *head)
{
        __list_add_rcu(new, head->prev, head);
}
static inline void __list_del(struct list_head * prev, struct list_head * next)
{
        next->prev = prev;
        prev->next = next;
}
static inline void list_del(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
}
static inline void list_del_rcu(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
}
static inline void list_del_init(struct list_head *entry)
{
        __list_del(entry->prev, entry->next);
        do { (entry)->next = (entry); (entry)->prev = (entry); } while (0);
}
static inline void list_move(struct list_head *list, struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add(list, head);
}
static inline void list_move_tail(struct list_head *list,
                                  struct list_head *head)
{
        __list_del(list->prev, list->next);
        list_add_tail(list, head);
}
static inline int list_empty(struct list_head *head)
{
        return head->next == head;
}
static inline void __list_splice(struct list_head *list,
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
static inline void list_splice(struct list_head *list, struct list_head *head)
{
        if (!list_empty(list))
                __list_splice(list, head);
}
static inline void list_splice_init(struct list_head *list,
                                    struct list_head *head)
{
        if (!list_empty(list)) {
                __list_splice(list, head);
                do { (list)->next = (list); (list)->prev = (list); } while (0);
        }
}
typedef struct __wait_queue wait_queue_t;
typedef int (*wait_queue_func_t)(wait_queue_t *wait, unsigned mode, int sync);
extern int default_wake_function(wait_queue_t *wait, unsigned mode, int sync);
struct __wait_queue {
        unsigned int flags;
        struct task_struct * task;
        wait_queue_func_t func;
        struct list_head task_list;
};
struct __wait_queue_head {
        spinlock_t lock;
        struct list_head task_list;
};
typedef struct __wait_queue_head wait_queue_head_t;
static inline void init_waitqueue_head(wait_queue_head_t *q)
{
        do { (&q->task_list)->next = (&q->task_list); (&q->task_list)->prev = (&q->task_list); } while (0);
}
static inline void init_waitqueue_entry(wait_queue_t *q, struct task_struct *p)
{
        q->flags = 0;
        q->task = p;
        q->func = default_wake_function;
}
static inline void init_waitqueue_func_entry(wait_queue_t *q,
                                        wait_queue_func_t func)
{
        q->flags = 0;
        q->task = ((void *)0);
        q->func = func;
}
static inline int waitqueue_active(wait_queue_head_t *q)
{
        return !list_empty(&q->task_list);
}
extern void add_wait_queue(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));
extern void add_wait_queue_exclusive(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));
extern void remove_wait_queue(wait_queue_head_t *q, wait_queue_t * wait) __attribute__((regparm(3)));
static inline void __add_wait_queue(wait_queue_head_t *head, wait_queue_t *new)
{
        list_add(&new->task_list, &head->task_list);
}
static inline void __add_wait_queue_tail(wait_queue_head_t *head,
                                                wait_queue_t *new)
{
        list_add_tail(&new->task_list, &head->task_list);
}
static inline void __remove_wait_queue(wait_queue_head_t *head,
                                                        wait_queue_t *old)
{
        list_del(&old->task_list);
}
extern void __wake_up(wait_queue_head_t *q, unsigned int mode, int nr) __attribute__((regparm(3)));
extern void __wake_up_locked(wait_queue_head_t *q, unsigned int mode) __attribute__((regparm(3)));
extern void __wake_up_sync(wait_queue_head_t *q, unsigned int mode, int nr) __attribute__((regparm(3)));
static inline void add_wait_queue_exclusive_locked(wait_queue_head_t *q,
                                                   wait_queue_t * wait)
{
        wait->flags |= 0x01;
        __add_wait_queue_tail(q, wait);
}
static inline void remove_wait_queue_locked(wait_queue_head_t *q,
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
int autoremove_wake_function(wait_queue_t *wait, unsigned mode, int sync);
struct rw_semaphore;
struct rwsem_waiter;
extern struct rw_semaphore *rwsem_down_read_failed(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern struct rw_semaphore *rwsem_down_write_failed(struct rw_semaphore *sem) __attribute__((regparm(3)));
extern struct rw_semaphore *rwsem_wake(struct rw_semaphore *) __attribute__((regparm(3)));
extern struct rw_semaphore *rwsem_downgrade_wake(struct rw_semaphore *sem) __attribute__((regparm(3)));
struct rw_semaphore {
        signed long count;
        spinlock_t wait_lock;
        struct list_head wait_list;
};
static inline void init_rwsem(struct rw_semaphore *sem)
{
        sem->count = 0x00000000;
        do { *(&sem->wait_lock) = (spinlock_t) { 1 }; } while(0);
        do { (&sem->wait_list)->next = (&sem->wait_list); (&sem->wait_list)->prev = (&sem->wait_list); } while (0);
}
static inline void __down_read(struct rw_semaphore *sem)
{
        __asm__ __volatile__(
                "# beginning down_read\n\t"
"lock ; " "  incl      (%%eax)\n\t"
                "  js        2f\n\t"
                "1:\n\t"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\n\t"
                "  pushl     %%ecx\n\t"
                "  pushl     %%edx\n\t"
                "  call      rwsem_down_read_failed\n\t"
                "  popl      %%edx\n\t"
                "  popl      %%ecx\n\t"
                "  jmp       1b\n"
                ".previous\n\t"
                "# ending down_read\n\t"
                : "=m"(sem->count)
                : "a"(sem), "m"(sem->count)
                : "memory", "cc");
}
static inline int __down_read_trylock(struct rw_semaphore *sem)
{
        __s32 result, tmp;
        __asm__ __volatile__(
                "# beginning __down_read_trylock\n\t"
                "  movl      %0,%1\n\t"
                "1:\n\t"
                "  movl	     %1,%2\n\t"
                "  addl      %3,%2\n\t"
                "  jle	     2f\n\t"
"lock ; " "  cmpxchgl  %2,%0\n\t"
                "  jnz	     1b\n\t"
                "2:\n\t"
                "# ending __down_read_trylock\n\t"
                : "+m"(sem->count), "=&a"(result), "=&r"(tmp)
                : "i"(0x00000001)
                : "memory", "cc");
        return result>=0 ? 1 : 0;
}
static inline void __down_write(struct rw_semaphore *sem)
{
        int tmp;
        tmp = ((-0x00010000) + 0x00000001);
        __asm__ __volatile__(
                "# beginning down_write\n\t"
"lock ; " "  xadd      %%edx,(%%eax)\n\t"
                "  testl     %%edx,%%edx\n\t"
                "  jnz       2f\n\t"
                "1:\n\t"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\n\t"
                "  pushl     %%ecx\n\t"
                "  call      rwsem_down_write_failed\n\t"
                "  popl      %%ecx\n\t"
                "  jmp       1b\n"
                ".previous\n\t"
                "# ending down_write"
                : "=m"(sem->count), "=d"(tmp)
                : "a"(sem), "1"(tmp), "m"(sem->count)
                : "memory", "cc");
}
static inline int __down_write_trylock(struct rw_semaphore *sem)
{
        signed long ret = ((__typeof__(*(&sem->count)))__cmpxchg((&sem->count),(unsigned long)(0x00000000), (unsigned long)(((-0x00010000) + 0x00000001)),sizeof(*(&sem->count))));
        if (ret == 0x00000000)
                return 1;
        return 0;
}
static inline void __up_read(struct rw_semaphore *sem)
{
        __s32 tmp = -0x00000001;
        __asm__ __volatile__(
                "# beginning __up_read\n\t"
"lock ; " "  xadd      %%edx,(%%eax)\n\t"
                "  js        2f\n\t"
                "1:\n\t"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\n\t"
                "  decw      %%dx\n\t"
                "  jnz       1b\n\t"
                "  pushl     %%ecx\n\t"
                "  call      rwsem_wake\n\t"
                "  popl      %%ecx\n\t"
                "  jmp       1b\n"
                ".previous\n\t"
                "# ending __up_read\n"
                : "=m"(sem->count), "=d"(tmp)
                : "a"(sem), "1"(tmp), "m"(sem->count)
                : "memory", "cc");
}
static inline void __up_write(struct rw_semaphore *sem)
{
        __asm__ __volatile__(
                "# beginning __up_write\n\t"
                "  movl      %2,%%edx\n\t"
"lock ; " "  xaddl     %%edx,(%%eax)\n\t"
                "  jnz       2f\n\t"
                "1:\n\t"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\n\t"
                "  decw      %%dx\n\t"
                "  jnz       1b\n\t"
                "  pushl     %%ecx\n\t"
                "  call      rwsem_wake\n\t"
                "  popl      %%ecx\n\t"
                "  jmp       1b\n"
                ".previous\n\t"
                "# ending __up_write\n"
                : "=m"(sem->count)
                : "a"(sem), "i"(-((-0x00010000) + 0x00000001)), "m"(sem->count)
                : "memory", "cc", "edx");
}
static inline void __downgrade_write(struct rw_semaphore *sem)
{
        __asm__ __volatile__(
                "# beginning __downgrade_write\n\t"
"lock ; " "  addl      %2,(%%eax)\n\t"
                "  js        2f\n\t"
                "1:\n\t"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\n\t"
                "  pushl     %%ecx\n\t"
                "  pushl     %%edx\n\t"
                "  call      rwsem_downgrade_wake\n\t"
                "  popl      %%edx\n\t"
                "  popl      %%ecx\n\t"
                "  jmp       1b\n"
                ".previous\n\t"
                "# ending __downgrade_write\n"
                : "=m"(sem->count)
                : "a"(sem), "i"(-(-0x00010000)), "m"(sem->count)
                : "memory", "cc");
}
static inline void rwsem_atomic_add(int delta, struct rw_semaphore *sem)
{
        __asm__ __volatile__(
"lock ; " "addl %1,%0"
                : "=m"(sem->count)
                : "ir"(delta), "m"(sem->count));
}
static inline int rwsem_atomic_update(int delta, struct rw_semaphore *sem)
{
        int tmp = delta;
        __asm__ __volatile__(
"lock ; " "xadd %0,(%2)"
                : "+r"(tmp), "=m"(sem->count)
                : "r"(sem), "m"(sem->count)
                : "memory");
        return tmp+delta;
}
static inline void down_read(struct rw_semaphore *sem)
{
        do {} while(0);
        ;
        __down_read(sem);
        ;
}
static inline int down_read_trylock(struct rw_semaphore *sem)
{
        int ret;
        ;
        ret = __down_read_trylock(sem);
        ;
        return ret;
}
static inline void down_write(struct rw_semaphore *sem)
{
        do {} while(0);
        ;
        __down_write(sem);
        ;
}
static inline int down_write_trylock(struct rw_semaphore *sem)
{
        int ret;
        ;
        ret = __down_write_trylock(sem);
        ;
        return ret;
}
static inline void up_read(struct rw_semaphore *sem)
{
        ;
        __up_read(sem);
        ;
}
static inline void up_write(struct rw_semaphore *sem)
{
        ;
        __up_write(sem);
        ;
}
static inline void downgrade_write(struct rw_semaphore *sem)
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
static inline void sema_init (struct semaphore *sem, int val)
{
        (((&sem->count)->counter) = (val));
        sem->sleepers = 0;
        init_waitqueue_head(&sem->wait);
}
static inline void init_MUTEX (struct semaphore *sem)
{
        sema_init(sem, 1);
}
static inline void init_MUTEX_LOCKED (struct semaphore *sem)
{
        sema_init(sem, 0);
}
 __attribute__((regparm(0))) void __down_failed(void );
 __attribute__((regparm(0))) int __down_failed_interruptible(void );
 __attribute__((regparm(0))) int __down_failed_trylock(void );
 __attribute__((regparm(0))) void __up_wakeup(void );
 __attribute__((regparm(0))) void __down(struct semaphore * sem);
 __attribute__((regparm(0))) int __down_interruptible(struct semaphore * sem);
 __attribute__((regparm(0))) int __down_trylock(struct semaphore * sem);
 __attribute__((regparm(0))) void __up(struct semaphore * sem);
static inline void down(struct semaphore * sem)
{
        do {} while(0);
        __asm__ __volatile__(
                "# atomic down operation\n\t"
                "lock ; " "decl %0\n\t"
                "js 2f\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\tcall __down_failed\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                :"=m" (sem->count)
                :"c" (sem)
                :"memory");
}
static inline int down_interruptible(struct semaphore * sem)
{
        int result;
        do {} while(0);
        __asm__ __volatile__(
                "# atomic interruptible down operation\n\t"
                "lock ; " "decl %1\n\t"
                "js 2f\n\t"
                "xorl %0,%0\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\tcall __down_failed_interruptible\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                :"=a" (result), "=m" (sem->count)
                :"c" (sem)
                :"memory");
        return result;
}
static inline int down_trylock(struct semaphore * sem)
{
        int result;
        __asm__ __volatile__(
                "# atomic interruptible down operation\n\t"
                "lock ; " "decl %1\n\t"
                "js 2f\n\t"
                "xorl %0,%0\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\tcall __down_failed_trylock\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                :"=a" (result), "=m" (sem->count)
                :"c" (sem)
                :"memory");
        return result;
}
static inline void up(struct semaphore * sem)
{
        __asm__ __volatile__(
                "# atomic up operation\n\t"
                "lock ; " "incl %0\n\t"
                "jle 2f\n"
                "1:\n"
                ".subsection 1\n\t" "" ".ifndef " ".text.lock." "esp" "\n\t" ".text.lock." "esp" ":\n\t" ".endif\n\t"
                "2:\tcall __up_wakeup\n\t"
                "jmp 1b\n"
                ".previous\n\t"
                ".subsection 0\n"
                :"=m" (sem->count)
                :"c" (sem)
                :"memory");
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
typedef struct {
        int size;
        struct semaphore sem;
        void *ldt;
} mm_context_t;
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
__KM_FENCE_9 , KM_PTE2,
__KM_FENCE_10 , KM_IRQ0,
__KM_FENCE_11 , KM_IRQ1,
__KM_FENCE_12 , KM_SOFTIRQ0,
__KM_FENCE_13 , KM_SOFTIRQ1,
__KM_FENCE_14 , KM_TYPE_NR
};
enum fixed_addresses {
        FIX_HOLE,
        FIX_VSYSCALL,
        FIX_APIC_BASE,
        FIX_IO_APIC_BASE_0,
        FIX_IO_APIC_BASE_END = FIX_IO_APIC_BASE_0 + 8 -1,
        FIX_CYCLONE_TIMER,
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
static inline unsigned long fix_to_virt(const unsigned int idx)
{
        if (idx >= __end_of_fixed_addresses)
                __this_fixmap_does_not_exist();
        return ((0xfffff000UL) - ((idx) << 12));
}
static inline unsigned long virt_to_fix(const unsigned long vaddr)
{
        do { if (__builtin_expect(((vaddr >= (0xfffff000UL) || vaddr < ((0xfffff000UL) - (__end_of_permanent_fixed_addresses << 12)))!=0),0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (136), "i" ("include/asm/fixmap.h")); } while(0);
        return (((0xfffff000UL) - ((vaddr)&(~((1UL << 12)-1)))) >> 12);
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
extern int mp_bus_id_to_type [32];
extern int mp_bus_id_to_node [32];
extern int mp_bus_id_to_local [32];
extern int quad_local_to_mp_bus_id [8/4][4];
extern int mp_bus_id_to_pci_bus [32];
extern unsigned int boot_cpu_physical_apicid;
extern unsigned long phys_cpu_present_map;
extern int smp_found_config;
extern void find_smp_config (void);
extern void get_smp_config (void);
extern int nr_ioapics;
extern int apic_version [256];
extern int mp_bus_id_to_type [32];
extern int mp_irq_entries;
extern struct mpc_config_intsrc mp_irqs [256];
extern int mpc_default_type;
extern int mp_bus_id_to_pci_bus [32];
extern int mp_current_pci_id;
extern unsigned long mp_lapic_addr;
extern int pic_mode;
extern int using_apic_timer;
struct IO_APIC_reg_00 {
        __u32 __reserved_2 : 14,
                LTS : 1,
                delivery_type : 1,
                __reserved_1 : 8,
                ID : 4,
                __reserved_0 : 4;
} __attribute__ ((packed));
struct IO_APIC_reg_01 {
        __u32 version : 8,
                __reserved_2 : 7,
                PRQ : 1,
                entries : 8,
                __reserved_1 : 8;
} __attribute__ ((packed));
struct IO_APIC_reg_02 {
        __u32 __reserved_2 : 24,
                arbitration : 4,
                __reserved_1 : 4;
} __attribute__ ((packed));
extern int nr_ioapics;
extern int nr_ioapic_registers[8];
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
extern struct mpc_config_ioapic mp_ioapics[8];
extern int mp_irq_entries;
extern struct mpc_config_intsrc mp_irqs[256];
extern int mpc_default_type;
static inline unsigned int io_apic_read(unsigned int apic, unsigned int reg)
{
        *((volatile int *)(((0xfffff000UL) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        return *(((volatile int *)(((0xfffff000UL) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4);
}
static inline void io_apic_write(unsigned int apic, unsigned int reg, unsigned int value)
{
        *((volatile int *)(((0xfffff000UL) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        *(((volatile int *)(((0xfffff000UL) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4) = value;
}
extern int sis_apic_bug;
static inline void io_apic_modify(unsigned int apic, unsigned int reg, unsigned int value)
{
        if (sis_apic_bug)
                *((volatile int *)(((0xfffff000UL) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1))))) = reg;
        *(((volatile int *)(((0xfffff000UL) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4) = value;
}
static inline void io_apic_sync(unsigned int apic)
{
        (void) *(((volatile int *)(((0xfffff000UL) - ((FIX_IO_APIC_BASE_0 + apic) << 12)) + (mp_ioapics[apic].mpc_apicaddr & ~(~((1UL << 12)-1)))))+4);
}
extern int skip_ioapic_setup;
enum
{
        PM_SUSPEND,
        PM_RESUME,
        PM_SAVE_STATE,
        PM_SET_WAKEUP,
        PM_GET_RESOURCES,
        PM_SET_RESOURCES,
        PM_EJECT,
        PM_LOCK,
};
typedef int pm_request_t;
enum
{
        PM_UNKNOWN_DEV = 0,
        PM_SYS_DEV,
        PM_PCI_DEV,
        PM_USB_DEV,
        PM_SCSI_DEV,
        PM_ISA_DEV,
        PM_MTD_DEV,
};
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
struct pm_dev *pm_register(pm_dev_t type,
                           unsigned long id,
                           pm_callback callback);
void pm_unregister(struct pm_dev *dev);
void pm_unregister_all(pm_callback callback);
int pm_send(struct pm_dev *dev, pm_request_t rqst, void *data);
int pm_send_all(pm_request_t rqst, void *data);
struct pm_dev *pm_find(pm_dev_t type, struct pm_dev *from);
static inline void pm_access(struct pm_dev *dev) {}
static inline void pm_dev_idle(struct pm_dev *dev) {}
extern void (*pm_idle)(void);
extern void (*pm_power_off)(void);
static __inline void apic_write(unsigned long reg, unsigned long v)
{
        *((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)) = v;
}
static __inline void apic_write_atomic(unsigned long reg, unsigned long v)
{
        ((__typeof__(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg))))__xchg((unsigned long)(v),((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)),sizeof(*((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg)))));
}
static __inline unsigned long apic_read(unsigned long reg)
{
        return *((volatile unsigned long *)((fix_to_virt(FIX_APIC_BASE))+reg));
}
static __inline__ void apic_wait_icr_idle(void)
{
        do { } while ( apic_read( 0x300 ) & 0x01000 );
}
static inline void ack_APIC_irq(void)
{
        apic_write((0xB0),(0));
}
extern int get_maxlvt(void);
extern void clear_local_APIC(void);
extern void connect_bsp_APIC (void);
extern void disconnect_bsp_APIC (void);
extern void disable_local_APIC (void);
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
extern inline void nmi_watchdog_tick (struct pt_regs * regs);
extern int APIC_init_uniprocessor (void);
extern void disable_APIC_timer(void);
extern void enable_APIC_timer(void);
extern struct pm_dev *apic_pm_register(pm_dev_t, unsigned long, pm_callback);
extern void apic_pm_unregister(struct pm_dev*);
extern int check_nmi_watchdog (void);
extern void enable_NMI_through_LVT0 (void * dummy);
extern unsigned int nmi_watchdog;
extern void smp_alloc_memory(void);
extern unsigned long phys_cpu_present_map;
extern unsigned long cpu_online_map;
extern volatile unsigned long smp_invalidate_needed;
extern int pic_mode;
extern int smp_num_siblings;
extern int cpu_sibling_map[];
extern void smp_flush_tlb(void);
extern void smp_message_irq(int cpl, void *dev_id, struct pt_regs *regs);
extern void smp_send_reschedule(int cpu);
extern void smp_send_reschedule_all(void);
extern void smp_invalidate_rcv(void);
extern void (*mtrr_hook) (void);
extern void zap_low_mappings (void);
extern volatile unsigned long cpu_callout_map;
extern inline unsigned int num_online_cpus(void)
{
        return generic_hweight32(cpu_online_map);
}
static inline int num_booting_cpus(void)
{
        return generic_hweight32(cpu_callout_map);
}
extern void map_cpu_to_logical_apicid(void);
extern void unmap_cpu_to_logical_apicid(int cpu);
extern inline int any_online_cpu(unsigned int mask)
{
        if (mask & cpu_online_map)
                return __ffs(mask & cpu_online_map);
        return -1;
}
static __inline int hard_smp_processor_id(void)
{
        return (((*(unsigned long *)((fix_to_virt(FIX_APIC_BASE))+0x20))>>24)&0xFF);
}
static __inline int logical_smp_processor_id(void)
{
        return (((*(unsigned long *)((fix_to_virt(FIX_APIC_BASE))+0xD0))>>24)&0xFF);
}
extern void smp_send_stop(void);
extern void smp_send_reschedule(int cpu) __attribute__((regparm(3)));
extern void smp_prepare_cpus(unsigned int max_cpus);
extern int __cpu_up(unsigned int cpunum);
extern void smp_cpus_done(unsigned int max_cpus);
extern int smp_call_function (void (*func) (void *info), void *info,
                              int retry, int wait);
extern int smp_threads_ready;
extern volatile unsigned long smp_msg_data;
extern volatile int smp_src_cpu;
extern volatile int smp_msg_id;
struct notifier_block;
extern int register_cpu_notifier(struct notifier_block *nb);
extern void unregister_cpu_notifier(struct notifier_block *nb);
int cpu_up(unsigned int cpu);
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
        volatile unsigned long add_count;
        struct sem_undo *proc_list;
};
struct sysv_sem {
        struct sem_undo_list *undo_list;
        struct sem_queue *sleep_list;
};
 __attribute__((regparm(0))) long sys_semget (key_t key, int nsems, int semflg);
 __attribute__((regparm(0))) long sys_semop (int semid, struct sembuf *sops, unsigned nsops);
 __attribute__((regparm(0))) long sys_semctl (int semid, int semnum, int cmd, union semun arg);
 __attribute__((regparm(0))) long sys_semtimedop(int semid, struct sembuf *sops,
                        unsigned nsops, const struct timespec *timeout);
struct siginfo;
typedef unsigned long old_sigset_t;
typedef struct {
        unsigned long sig[(64 / 32)];
} sigset_t;
typedef void (*__sighandler_t)(int);
struct old_sigaction {
        __sighandler_t sa_handler;
        old_sigset_t sa_mask;
        unsigned long sa_flags;
        void (*sa_restorer)(void);
};
struct sigaction {
        __sighandler_t sa_handler;
        unsigned long sa_flags;
        void (*sa_restorer)(void);
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
static __inline__ void sigaddset(sigset_t *set, int _sig)
{
        __asm__("btsl %1,%0" : "=m"(*set) : "Ir"(_sig - 1) : "cc");
}
static __inline__ void sigdelset(sigset_t *set, int _sig)
{
        __asm__("btrl %1,%0" : "=m"(*set) : "Ir"(_sig - 1) : "cc");
}
static __inline__ int __const_sigismember(sigset_t *set, int _sig)
{
        unsigned long sig = _sig - 1;
        return 1 & (set->sig[sig / 32] >> (sig % 32));
}
static __inline__ int __gen_sigismember(sigset_t *set, int _sig)
{
        int ret;
        __asm__("btl %2,%1\n\tsbbl %0,%0"
                : "=r"(ret) : "m"(*set), "Ir"(_sig-1) : "cc");
        return ret;
}
static __inline__ int sigfindinword(unsigned long word)
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
                        unsigned int _timer1;
                        unsigned int _timer2;
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
                        int _band;
                        int _fd;
                } _sigpoll;
        } _sifields;
} siginfo_t;
typedef struct sigevent {
        sigval_t sigev_value;
        int sigev_signo;
        int sigev_notify;
        union {
                int _pad[((64/sizeof(int)) - 3)];
                struct {
                        void (*_function)(sigval_t);
                        void *_attribute;
                } _sigev_thread;
        } _sigev_un;
} sigevent_t;
struct siginfo;
extern char * strpbrk(const char *,const char *);
extern char * strsep(char **,const char *);
extern __kernel_size_t strspn(const char *,const char *);
extern __kernel_size_t strcspn(const char *,const char *);
static inline char * strcpy(char * dest,const char *src)
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
static inline char * strncpy(char * dest,const char *src,size_t count)
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
static inline char * strcat(char * dest,const char * src)
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
        : "0" (src), "1" (dest), "2" (0), "3" (0xffffffff):"memory");
return dest;
}
static inline char * strncat(char * dest,const char * src,size_t count)
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
        : "0" (src),"1" (dest),"2" (0),"3" (0xffffffff), "g" (count)
        : "memory");
return dest;
}
static inline int strcmp(const char * cs,const char * ct)
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
                     :"1" (cs),"2" (ct));
return __res;
}
static inline int strncmp(const char * cs,const char * ct,size_t count)
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
                     :"1" (cs),"2" (ct),"3" (count));
return __res;
}
static inline char * strchr(const char * s, int c)
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
        :"=a" (__res), "=&S" (d0) : "1" (s),"0" (c));
return __res;
}
static inline char * strrchr(const char * s, int c)
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
        :"=g" (__res), "=&S" (d0), "=&a" (d1) :"0" (0),"1" (s),"2" (c));
return __res;
}
static inline size_t strlen(const char * s)
{
int d0;
register int __res;
__asm__ __volatile__(
        "repne\n\t"
        "scasb\n\t"
        "notl %0\n\t"
        "decl %0"
        :"=c" (__res), "=&D" (d0) :"1" (s),"a" (0), "0" (0xffffffff));
return __res;
}
static inline void * __memcpy(void * to, const void * from, size_t n)
{
int d0, d1, d2;
__asm__ __volatile__(
        "rep ; movsl\n\t"
        "testb $2,%b4\n\t"
        "je 1f\n\t"
        "movsw\n"
        "1:\ttestb $1,%b4\n\t"
        "je 2f\n\t"
        "movsb\n"
        "2:"
        : "=&c" (d0), "=&D" (d1), "=&S" (d2)
        :"0" (n/4), "q" (n),"1" ((long) to),"2" ((long) from)
        : "memory");
return (to);
}
static inline void * __constant_memcpy(void * to, const void * from, size_t n)
{
        switch (n) {
                case 0:
                        return to;
                case 1:
                        *(unsigned char *)to = *(const unsigned char *)from;
                        return to;
                case 2:
                        *(unsigned short *)to = *(const unsigned short *)from;
                        return to;
                case 3:
                        *(unsigned short *)to = *(const unsigned short *)from;
                        *(2+(unsigned char *)to) = *(2+(const unsigned char *)from);
                        return to;
                case 4:
                        *(unsigned long *)to = *(const unsigned long *)from;
                        return to;
                case 6:
                        *(unsigned long *)to = *(const unsigned long *)from;
                        *(2+(unsigned short *)to) = *(2+(const unsigned short *)from);
                        return to;
                case 8:
                        *(unsigned long *)to = *(const unsigned long *)from;
                        *(1+(unsigned long *)to) = *(1+(const unsigned long *)from);
                        return to;
                case 12:
                        *(unsigned long *)to = *(const unsigned long *)from;
                        *(1+(unsigned long *)to) = *(1+(const unsigned long *)from);
                        *(2+(unsigned long *)to) = *(2+(const unsigned long *)from);
                        return to;
                case 16:
                        *(unsigned long *)to = *(const unsigned long *)from;
                        *(1+(unsigned long *)to) = *(1+(const unsigned long *)from);
                        *(2+(unsigned long *)to) = *(2+(const unsigned long *)from);
                        *(3+(unsigned long *)to) = *(3+(const unsigned long *)from);
                        return to;
                case 20:
                        *(unsigned long *)to = *(const unsigned long *)from;
                        *(1+(unsigned long *)to) = *(1+(const unsigned long *)from);
                        *(2+(unsigned long *)to) = *(2+(const unsigned long *)from);
                        *(3+(unsigned long *)to) = *(3+(const unsigned long *)from);
                        *(4+(unsigned long *)to) = *(4+(const unsigned long *)from);
                        return to;
        }
{
        int d0, d1, d2;
        switch (n % 4) {
                case 0: __asm__ __volatile__( "rep ; movsl" "" : "=&c" (d0), "=&D" (d1), "=&S" (d2) : "0" (n/4),"1" ((long) to),"2" ((long) from) : "memory");; return to;
                case 1: __asm__ __volatile__( "rep ; movsl" "\n\tmovsb" : "=&c" (d0), "=&D" (d1), "=&S" (d2) : "0" (n/4),"1" ((long) to),"2" ((long) from) : "memory");; return to;
                case 2: __asm__ __volatile__( "rep ; movsl" "\n\tmovsw" : "=&c" (d0), "=&D" (d1), "=&S" (d2) : "0" (n/4),"1" ((long) to),"2" ((long) from) : "memory");; return to;
                default: __asm__ __volatile__( "rep ; movsl" "\n\tmovsw\n\tmovsb" : "=&c" (d0), "=&D" (d1), "=&S" (d2) : "0" (n/4),"1" ((long) to),"2" ((long) from) : "memory");; return to;
        }
}
}
extern void __struct_cpy_bug (void);
static inline void * memmove(void * dest,const void * src, size_t n)
{
int d0, d1, d2;
if (dest<src)
__asm__ __volatile__(
        "rep\n\t"
        "movsb"
        : "=&c" (d0), "=&S" (d1), "=&D" (d2)
        :"0" (n),"1" (src),"2" (dest)
        : "memory");
else
__asm__ __volatile__(
        "std\n\t"
        "rep\n\t"
        "movsb\n\t"
        "cld"
        : "=&c" (d0), "=&S" (d1), "=&D" (d2)
        :"0" (n),
         "1" (n-1+(const char *)src),
         "2" (n-1+(char *)dest)
        :"memory");
return dest;
}
static inline void * memchr(const void * cs,int c,size_t count)
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
        :"=D" (__res), "=&c" (d0) : "a" (c),"0" (cs),"1" (count));
return __res;
}
static inline void * __memset_generic(void * s, char c,size_t count)
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
static inline void * __constant_c_memset(void * s, unsigned long c, size_t count)
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
        : "=&c" (d0), "=&D" (d1)
        :"a" (c), "q" (count), "0" (count/4), "1" ((long) s)
        :"memory");
return (s);
}
static inline size_t strnlen(const char * s, size_t count)
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
        :"c" (s),"1" (count));
return __res;
}
extern char *strstr(const char *cs, const char *ct);
static inline void * __constant_c_and_count_memset(void * s, unsigned long pattern, size_t count)
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
static inline void * memscan(void * addr, int c, size_t size)
{
        if (!size)
                return addr;
        __asm__("repnz; scasb\n\t"
                "jnz 1f\n\t"
                "dec %%edi\n"
                "1:"
                : "=D" (addr), "=c" (size)
                : "0" (addr), "1" (size), "a" (c));
        return addr;
}
extern int strnicmp(const char *, const char *, __kernel_size_t);
extern int __builtin_memcmp(const void *,const void *,__kernel_size_t);
static inline void copy_siginfo(struct siginfo *to, struct siginfo *from)
{
        if (from->si_code < 0)
                (__builtin_constant_p(sizeof(*to)) ? __constant_memcpy((to),(from),(sizeof(*to))) : __memcpy((to),(from),(sizeof(*to))));
        else
                (__builtin_constant_p((3 * sizeof(int)) + sizeof(from->_sifields._sigchld)) ? __constant_memcpy((to),(from),((3 * sizeof(int)) + sizeof(from->_sifields._sigchld))) : __memcpy((to),(from),((3 * sizeof(int)) + sizeof(from->_sifields._sigchld))));
}
extern int copy_siginfo_to_user(struct siginfo *to, struct siginfo *from);
struct sigqueue {
        struct sigqueue *next;
        siginfo_t info;
};
struct sigpending {
        struct sigqueue *head, **tail;
        sigset_t signal;
};
static inline void sigorsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) | (b3)); r->sig[2] = ((a2) | (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) | (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) | (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }
static inline void sigandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & (b3)); r->sig[2] = ((a2) & (b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & (b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & (b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }
static inline void signandsets(sigset_t *r, const sigset_t *a, const sigset_t *b) { extern void _NSIG_WORDS_is_unsupported_size(void); unsigned long a0, a1, a2, a3, b0, b1, b2, b3; switch ((64 / 32)) { case 4: a3 = a->sig[3]; a2 = a->sig[2]; b3 = b->sig[3]; b2 = b->sig[2]; r->sig[3] = ((a3) & ~(b3)); r->sig[2] = ((a2) & ~(b2)); case 2: a1 = a->sig[1]; b1 = b->sig[1]; r->sig[1] = ((a1) & ~(b1)); case 1: a0 = a->sig[0]; b0 = b->sig[0]; r->sig[0] = ((a0) & ~(b0)); break; default: _NSIG_WORDS_is_unsupported_size(); } }
static inline void signotset(sigset_t *set) { extern void _NSIG_WORDS_is_unsupported_size(void); switch ((64 / 32)) { case 4: set->sig[3] = (~(set->sig[3])); set->sig[2] = (~(set->sig[2])); case 2: set->sig[1] = (~(set->sig[1])); case 1: set->sig[0] = (~(set->sig[0])); break; default: _NSIG_WORDS_is_unsupported_size(); } }
static inline void sigemptyset(sigset_t *set)
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
static inline void sigfillset(sigset_t *set)
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
static inline void sigaddsetmask(sigset_t *set, unsigned long mask)
{
        set->sig[0] |= mask;
}
static inline void sigdelsetmask(sigset_t *set, unsigned long mask)
{
        set->sig[0] &= ~mask;
}
static inline int sigtestsetmask(sigset_t *set, unsigned long mask)
{
        return (set->sig[0] & mask) != 0;
}
static inline void siginitset(sigset_t *set, unsigned long mask)
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
static inline void siginitsetinv(sigset_t *set, unsigned long mask)
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
static inline void init_sigpending(struct sigpending *sig)
{
        sigemptyset(&sig->signal);
        sig->head = ((void *)0);
        sig->tail = &sig->head;
}
extern long do_sigpending(void *, unsigned long);
extern int sigprocmask(int, sigset_t *, sigset_t *);
struct pt_regs;
extern int get_signal_to_deliver(siginfo_t *info, struct pt_regs *regs, void *cookie);
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
static inline void init_completion(struct completion *x)
{
        x->done = 0;
        init_waitqueue_head(&x->wait);
}
extern void wait_for_completion(struct completion *) __attribute__((regparm(3)));
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
        atomic_t count;
        struct task_struct *task;
        struct list_head task_list;
        struct list_head hash_chain;
};
struct pid_link
{
        struct list_head pid_chain;
        struct pid *pidptr;
        struct pid pid;
};
extern int attach_pid(struct task_struct *task, enum pid_type type, int nr) __attribute__((regparm(3)));
extern void link_pid(struct task_struct *task, struct pid_link *link, struct pid *pid) __attribute__((regparm(3)));
extern void detach_pid(struct task_struct *task, enum pid_type) __attribute__((regparm(3)));
extern struct pid *find_pid(enum pid_type, int) __attribute__((regparm(3)));
extern int alloc_pidmap(void);
extern void free_pidmap(int) __attribute__((regparm(3)));
extern void switch_exec_pids(struct task_struct *leader, struct task_struct *thread);
typedef struct kmem_cache_s kmem_cache_t;
struct free_area {
        struct list_head free_list;
        unsigned long *map;
};
struct pglist_data;
struct zone_padding {
        int x;
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
} __attribute__((__aligned__((1 << (5)))));
struct zone {
        spinlock_t lock;
        unsigned long free_pages;
        unsigned long pages_min, pages_low, pages_high;
        struct zone_padding _pad1_;
        spinlock_t lru_lock;
        struct list_head active_list;
        struct list_head inactive_list;
        atomic_t refill_counter;
        unsigned long nr_active;
        unsigned long nr_inactive;
        int all_unreclaimable;
        unsigned long pages_scanned;
        struct zone_padding _pad2_;
        struct free_area free_area[11];
        wait_queue_head_t * wait_table;
        unsigned long wait_table_size;
        unsigned long wait_table_bits;
        struct zone_padding _pad3_;
        struct per_cpu_pageset pageset[8];
        struct pglist_data *zone_pgdat;
        struct page *zone_mem_map;
        unsigned long zone_start_pfn;
        char *name;
        unsigned long spanned_pages;
        unsigned long present_pages;
} __attribute__((__aligned__(1 << (7))));
struct zonelist {
        struct zone *zones[1 * 3 + 1];
};
struct bootmem_data;
typedef struct pglist_data {
        struct zone node_zones[3];
        struct zonelist node_zonelists[0x0f +1];
        int nr_zones;
        struct page *node_mem_map;
        unsigned long *valid_addr_bitmap;
        struct bootmem_data *bdata;
        unsigned long node_start_pfn;
        unsigned long node_size;
        int node_id;
        struct pglist_data *pgdat_next;
        wait_queue_head_t kswapd_wait;
} pg_data_t;
extern int numnodes;
extern struct pglist_data *pgdat_list;
void get_zone_counts(unsigned long *active, unsigned long *inactive,
                        unsigned long *free);
void build_all_zonelists(void);
void wakeup_kswapd(struct zone *zone);
static inline struct zone *next_zone(struct zone *zone)
{
        pg_data_t *pgdat = zone->zone_pgdat;
        if (zone - pgdat->node_zones < 3 - 1)
                zone++;
        else if (pgdat->pgdat_next) {
                pgdat = pgdat->pgdat_next;
                zone = pgdat->node_zones;
        } else
                zone = ((void *)0);
        return zone;
}
extern struct pglist_data contig_page_data;
extern unsigned long node_online_map[(((1)+32 -1)/32)];
extern unsigned long memblk_online_map[(((1)+32 -1)/32)];
extern struct page * __alloc_pages(unsigned int, unsigned int, struct zonelist *) __attribute__((regparm(3)));
static inline struct page * alloc_pages_node(int nid, unsigned int gfp_mask, unsigned int order)
{
        struct pglist_data *pgdat = (&contig_page_data);
        unsigned int idx = (gfp_mask & 0x0f);
        if (__builtin_expect((order >= 11),0))
                return ((void *)0);
        return __alloc_pages(gfp_mask, order, pgdat->node_zonelists + idx);
}
static inline struct page * alloc_pages(unsigned int gfp_mask, unsigned int order)
{
        struct pglist_data *pgdat = (&contig_page_data);
        unsigned int idx = (gfp_mask & 0x0f);
        if (__builtin_expect((order >= 11),0))
                return ((void *)0);
        return __alloc_pages(gfp_mask, order, pgdat->node_zonelists + idx);
}
extern unsigned long __get_free_pages(unsigned int gfp_mask, unsigned int order) __attribute__((regparm(3)));
extern unsigned long get_zeroed_page(unsigned int gfp_mask) __attribute__((regparm(3)));
extern void __free_pages(struct page *page, unsigned int order) __attribute__((regparm(3)));
extern void free_pages(unsigned long addr, unsigned int order) __attribute__((regparm(3)));
extern void free_hot_page(struct page *page) __attribute__((regparm(3)));
extern void free_cold_page(struct page *page) __attribute__((regparm(3)));
void page_alloc_init(void);
extern void kmem_cache_init(void);
extern void kmem_cache_sizes_init(void);
extern kmem_cache_t *kmem_find_general_cachep(size_t, int gfpflags);
extern kmem_cache_t *kmem_cache_create(const char *, size_t, size_t, unsigned long,
                                       void (*)(void *, kmem_cache_t *, unsigned long),
                                       void (*)(void *, kmem_cache_t *, unsigned long));
extern int kmem_cache_destroy(kmem_cache_t *);
extern int kmem_cache_shrink(kmem_cache_t *);
extern void *kmem_cache_alloc(kmem_cache_t *, int);
extern void kmem_cache_free(kmem_cache_t *, void *);
extern unsigned int kmem_cache_size(kmem_cache_t *);
extern void *kmalloc(size_t, int);
extern void kfree(const void *);
extern unsigned int ksize(const void *);
extern int kmem_cache_reap(int) __attribute__((regparm(3)));
extern kmem_cache_t *vm_area_cachep;
extern kmem_cache_t *mm_cachep;
extern kmem_cache_t *names_cachep;
extern kmem_cache_t *files_cachep;
extern kmem_cache_t *filp_cachep;
extern kmem_cache_t *dquot_cachep;
extern kmem_cache_t *fs_cachep;
extern kmem_cache_t *signal_cachep;
extern kmem_cache_t *sighand_cachep;
extern kmem_cache_t *bio_cachep;
extern unsigned long __per_cpu_offset[8];
struct percpu_data {
        void *ptrs[8];
        void *blkp;
};
extern void *kmalloc_percpu(size_t size, int flags);
extern void kfree_percpu(const void *);
extern void kmalloc_percpu_init(void);
struct exec_domain;
extern unsigned long avenrun[];
extern int nr_threads;
extern int last_pid;
extern __typeof__(unsigned long) process_counts__per_cpu;
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
struct tvec_t_base_s;
struct timer_list {
        struct list_head entry;
        unsigned long expires;
        spinlock_t lock;
        unsigned long magic;
        void (*function)(unsigned long);
        unsigned long data;
        struct tvec_t_base_s *base;
};
static inline void init_timer(struct timer_list * timer)
{
        timer->base = ((void *)0);
        timer->magic = 0x4b87ad6e;
        do { *(&timer->lock) = (spinlock_t) { 1 }; } while(0);
}
static inline int timer_pending(const struct timer_list * timer)
{
        return timer->base != ((void *)0);
}
extern void add_timer(struct timer_list * timer);
extern void add_timer_on(struct timer_list *timer, int cpu);
extern int del_timer(struct timer_list * timer);
extern int mod_timer(struct timer_list *timer, unsigned long expires);
  extern int del_timer_sync(struct timer_list * timer);
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
extern void init_idle(task_t *idle, int cpu);
extern void show_state(void);
extern void show_trace(unsigned long *stack);
extern void show_stack(unsigned long *stack);
extern void show_regs(struct pt_regs *);
void io_schedule(void);
long io_schedule_timeout(long timeout);
extern void cpu_init (void);
extern void trap_init(void);
extern void update_process_times(int user);
extern void update_one_process(struct task_struct *p, unsigned long user,
                               unsigned long system, int cpu);
extern void scheduler_tick(int user_tick, int system);
extern unsigned long cache_decay_ticks;
extern signed long schedule_timeout(signed long timeout) __attribute__((regparm(3)));
 __attribute__((regparm(0))) void schedule(void);
struct namespace;
struct workqueue_struct;
struct work_struct {
        unsigned long pending;
        struct list_head entry;
        void (*func)(void *);
        void *data;
        void *wq_data;
        struct timer_list timer;
};
extern struct workqueue_struct *create_workqueue(const char *name);
extern void destroy_workqueue(struct workqueue_struct *wq);
extern int queue_work(struct workqueue_struct *wq, struct work_struct *work) __attribute__((regparm(3)));
extern int queue_delayed_work(struct workqueue_struct *wq, struct work_struct *work, unsigned long delay) __attribute__((regparm(3)));
extern void flush_workqueue(struct workqueue_struct *wq) __attribute__((regparm(3)));
extern int schedule_work(struct work_struct *work) __attribute__((regparm(3)));
extern int schedule_delayed_work(struct work_struct *work, unsigned long delay) __attribute__((regparm(3)));
extern void flush_scheduled_work(void);
extern int current_is_keventd(void);
extern void init_workqueues(void);
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
        long (*ki_retry)(struct kiocb *);
        struct list_head ki_list;
        void *ki_user_obj;
        __u64 ki_user_data;
        loff_t ki_pos;
        char private[(24 * sizeof(long))];
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
static inline struct kiocb *list_kiocb(struct list_head *h)
{
        return ({ const typeof( ((struct kiocb *)0)->ki_list ) *__mptr = (h); (struct kiocb *)( (char *)__mptr - ((size_t) &((struct kiocb *)0)->ki_list) );});
}
extern unsigned aio_max_nr, aio_max_size, aio_max_pinned;
struct mm_struct {
        struct vm_area_struct * mmap;
        struct rb_root mm_rb;
        struct vm_area_struct * mmap_cache;
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
        unsigned long rss, total_vm, locked_vm;
        unsigned long def_flags;
        unsigned long cpu_vm_mask;
        unsigned long swap_address;
        unsigned dumpable:1;
        mm_context_t context;
        int core_waiters;
        struct completion *core_startup_done, core_done;
        rwlock_t ioctx_list_lock;
        struct kioctx *ioctx_list;
        struct kioctx default_kioctx;
};
extern int mmlist_nr;
struct sighand_struct {
        atomic_t count;
        struct k_sigaction action[64];
        spinlock_t siglock;
};
struct signal_struct {
        atomic_t count;
        task_t *curr_target;
        struct sigpending shared_pending;
        int group_exit;
        int group_exit_code;
        struct task_struct *group_exit_task;
        int group_stop_count;
};
struct user_struct {
        atomic_t __count;
        atomic_t processes;
        atomic_t files;
        struct list_head uidhash_list;
        uid_t uid;
};
extern struct user_struct *find_user(uid_t);
extern struct user_struct root_user;
typedef struct prio_array prio_array_t;
struct backing_dev_info;
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
        unsigned long sleep_avg;
        unsigned long sleep_timestamp;
        unsigned long policy;
        unsigned long cpus_allowed;
        unsigned int time_slice, first_time_slice;
        struct list_head tasks;
        struct list_head ptrace_children;
        struct list_head ptrace_list;
        struct mm_struct *mm, *active_mm;
        struct linux_binfmt *binfmt;
        int exit_code, exit_signal;
        int pdeath_signal;
        unsigned long personality;
        int did_exec:1;
        pid_t pid;
        pid_t pgrp;
        pid_t tty_old_pgrp;
        pid_t session;
        pid_t tgid;
        int leader;
        struct task_struct *real_parent;
        struct task_struct *parent;
        struct list_head children;
        struct list_head sibling;
        struct task_struct *group_leader;
        struct pid_link pids[PIDTYPE_MAX];
        wait_queue_head_t wait_chldexit;
        struct completion *vfork_done;
        int *set_child_tid;
        int *clear_child_tid;
        unsigned long rt_priority;
        unsigned long it_real_value, it_prof_value, it_virt_value;
        unsigned long it_real_incr, it_prof_incr, it_virt_incr;
        struct timer_list real_timer;
        unsigned long utime, stime, cutime, cstime;
        u64 start_time;
        unsigned long min_flt, maj_flt, nswap, cmin_flt, cmaj_flt, cnswap;
        uid_t uid,euid,suid,fsuid;
        gid_t gid,egid,sgid,fsgid;
        int ngroups;
        gid_t groups[32];
        kernel_cap_t cap_effective, cap_inheritable, cap_permitted;
        int keep_capabilities:1;
        struct user_struct *user;
        struct rlimit rlim[11];
        unsigned short used_math;
        char comm[16];
        int link_count, total_link_count;
        struct tty_struct *tty;
        unsigned int locks;
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
        u32 parent_exec_id;
        u32 self_exec_id;
        spinlock_t alloc_lock;
        spinlock_t switch_lock;
        void *journal_info;
        struct dentry *proc_dentry;
        struct backing_dev_info *backing_dev_info;
        unsigned long ptrace_message;
        siginfo_t *last_siginfo;
};
extern void __put_task_struct(struct task_struct *tsk);
extern void set_cpus_allowed(task_t *p, unsigned long new_mask);
extern void set_user_nice(task_t *p, long nice);
extern int task_prio(task_t *p);
extern int task_nice(task_t *p);
extern int task_curr(task_t *p);
extern int idle_cpu(int cpu);
void yield(void);
extern struct exec_domain default_exec_domain;
union thread_union {
        struct thread_info thread_info;
        unsigned long stack[2048*sizeof(long)/sizeof(long)];
};
extern union thread_union init_thread_union;
extern struct task_struct init_task;
extern struct mm_struct init_mm;
extern struct task_struct *find_task_by_pid(int pid);
extern void set_special_pids(pid_t session, pid_t pgrp);
extern void __set_special_pids(pid_t session, pid_t pgrp);
extern struct user_struct * alloc_uid(uid_t);
extern void free_uid(struct user_struct *);
extern void switch_uid(struct user_struct *);
struct task_struct;
static inline struct task_struct * get_current(void)
{
        return current_thread_info()->task;
}
extern unsigned long itimer_ticks;
extern unsigned long itimer_next;
extern void do_timer(struct pt_regs *);
extern int wake_up_state(struct task_struct * tsk, unsigned int state) __attribute__((regparm(3)));
extern int wake_up_process(struct task_struct * tsk) __attribute__((regparm(3)));
extern void wake_up_forked_process(struct task_struct * tsk) __attribute__((regparm(3)));
extern void sched_exit(task_t * p) __attribute__((regparm(3)));
 __attribute__((regparm(0))) long sys_wait4(pid_t pid,unsigned int * stat_addr, int options, struct rusage * ru);
extern int in_group_p(gid_t);
extern int in_egroup_p(gid_t);
extern void proc_caches_init(void);
extern void flush_signals(struct task_struct *);
extern void flush_signal_handlers(struct task_struct *);
extern int dequeue_signal(struct task_struct *tsk, sigset_t *mask, siginfo_t *info);
extern void block_all_signals(int (*notifier)(void *priv), void *priv,
                              sigset_t *mask);
extern void unblock_all_signals(void);
extern void release_task(struct task_struct * p);
extern int send_sig_info(int, struct siginfo *, struct task_struct *);
extern int force_sig_info(int, struct siginfo *, struct task_struct *);
extern int __kill_pg_info(int sig, struct siginfo *info, pid_t pgrp);
extern int kill_pg_info(int, struct siginfo *, pid_t);
extern int kill_sl_info(int, struct siginfo *, pid_t);
extern int kill_proc_info(int, struct siginfo *, pid_t);
extern void notify_parent(struct task_struct *, int);
extern void do_notify_parent(struct task_struct *, int);
extern void force_sig(int, struct task_struct *);
extern void force_sig_specific(int, struct task_struct *);
extern int send_sig(int, struct task_struct *, int);
extern void zap_other_threads(struct task_struct *p);
extern int kill_pg(pid_t, int, int);
extern int kill_sl(pid_t, int, int);
extern int kill_proc(pid_t, int, int);
extern int do_sigaction(int, const struct k_sigaction *, struct k_sigaction *);
extern int do_sigaltstack(const stack_t *, stack_t *, unsigned long);
static inline int on_sig_stack(unsigned long sp)
{
        return (sp - get_current()->sas_ss_sp < get_current()->sas_ss_size);
}
static inline int sas_ss_flags(unsigned long sp)
{
        return (get_current()->sas_ss_size == 0 ? 2
                : on_sig_stack(sp) ? 1 : 0);
}
static inline int capable(int cap)
{
        if (((get_current()->cap_effective) & (1 << (cap)))) {
                get_current()->flags |= 0x00000100;
                return 1;
        }
        return 0;
}
extern struct mm_struct * mm_alloc(void);
extern struct mm_struct * start_lazy_tlb(void);
extern void end_lazy_tlb(struct mm_struct *mm);
extern inline void __mmdrop(struct mm_struct *) __attribute__((regparm(3)));
static inline void mmdrop(struct mm_struct * mm)
{
        if (atomic_dec_and_test(&mm->mm_count))
                __mmdrop(mm);
}
extern void mmput(struct mm_struct *);
extern void mm_release(struct task_struct *, struct mm_struct *);
extern int copy_thread(int, unsigned long, unsigned long, unsigned long, struct task_struct *, struct pt_regs *);
extern void flush_thread(void);
extern void exit_thread(void);
extern void exit_mm(struct task_struct *);
extern void exit_files(struct task_struct *);
extern void exit_signal(struct task_struct *);
extern void __exit_signal(struct task_struct *);
extern void exit_sighand(struct task_struct *);
extern void __exit_sighand(struct task_struct *);
extern void do_group_exit(int);
extern void reparent_to_init(void);
extern void daemonize(const char *, ...);
extern int allow_signal(int);
extern task_t *child_reaper;
extern int do_execve(char *, char **, char **, struct pt_regs *);
extern struct task_struct *do_fork(unsigned long, unsigned long, struct pt_regs *, unsigned long, int *, int *);
extern void wait_task_inactive(task_t * p);
extern void kick_if_running(task_t * p);
extern task_t * next_thread(task_t *p) __attribute__((regparm(3)));
static inline int thread_group_empty(task_t *p)
{
        struct pid *pid = p->pids[PIDTYPE_TGID].pidptr;
        return pid->task_list.next->next == &pid->task_list;
}
extern void unhash_process(struct task_struct *p);
static inline void task_lock(struct task_struct *p)
{
        do { do { } while (0); _raw_spin_lock(&p->alloc_lock); } while(0);
}
static inline void task_unlock(struct task_struct *p)
{
        do { _raw_spin_unlock(&p->alloc_lock); do { } while (0); } while (0);
}
static inline struct mm_struct * get_task_mm(struct task_struct * task)
{
        struct mm_struct * mm;
        task_lock(task);
        mm = task->mm;
        if (mm)
                atomic_inc(&mm->mm_users);
        task_unlock(task);
        return mm;
}
static inline void set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        set_ti_thread_flag(tsk->thread_info,flag);
}
static inline void clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        clear_ti_thread_flag(tsk->thread_info,flag);
}
static inline int test_and_set_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_and_set_ti_thread_flag(tsk->thread_info,flag);
}
static inline int test_and_clear_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_and_clear_ti_thread_flag(tsk->thread_info,flag);
}
static inline int test_tsk_thread_flag(struct task_struct *tsk, int flag)
{
        return test_ti_thread_flag(tsk->thread_info,flag);
}
static inline void set_tsk_need_resched(struct task_struct *tsk)
{
        set_tsk_thread_flag(tsk,3);
}
static inline void clear_tsk_need_resched(struct task_struct *tsk)
{
        clear_tsk_thread_flag(tsk,3);
}
static inline int signal_pending(struct task_struct *p)
{
        return __builtin_expect((test_tsk_thread_flag(p,2)),0);
}
static inline int need_resched(void)
{
        return __builtin_expect((test_thread_flag(3)),0);
}
extern void __cond_resched(void);
static inline void cond_resched(void)
{
        if (need_resched())
                __cond_resched();
}
static inline void cond_resched_lock(spinlock_t * lock)
{
        if (need_resched()) {
                _raw_spin_unlock(lock);
                do { } while (0);
                __cond_resched();
                do { do { } while (0); _raw_spin_lock(lock); } while(0);
        }
}
extern void recalc_sigpending_tsk(struct task_struct *t) __attribute__((regparm(3)));
extern void recalc_sigpending(void);
extern void signal_wake_up(struct task_struct *t, int resume_stopped);
static inline unsigned int task_cpu(struct task_struct *p)
{
        return p->thread_info->cpu;
}
static inline void set_task_cpu(struct task_struct *p, unsigned int cpu)
{
        p->thread_info->cpu = cpu;
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
        unsigned short st_dev;
        unsigned short __pad1;
        unsigned long st_ino;
        unsigned short st_mode;
        unsigned short st_nlink;
        unsigned short st_uid;
        unsigned short st_gid;
        unsigned short st_rdev;
        unsigned short __pad2;
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
        unsigned short st_dev;
        unsigned char __pad0[10];
        unsigned long __st_ino;
        unsigned int st_mode;
        unsigned int st_nlink;
        unsigned long st_uid;
        unsigned long st_gid;
        unsigned short st_rdev;
        unsigned char __pad3[10];
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
        nlink_t nlink;
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
extern int request_module(const char * name);
extern int call_usermodehelper(char *path, char *argv[], char *envp[], int wait);
extern char hotplug_path [];
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
extern void dump_smp_unlazy_fpu(void);
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
struct mod_arch_specific
{
};
int __verify_write(const void *, unsigned long);
static inline int verify_area(int type, const void * addr, unsigned long size)
{
        return (({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (addr),"g" ((int)(size)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0) ? 0 : -14;
}
struct exception_table_entry
{
        unsigned long insn, fixup;
};
extern int fixup_exception(struct pt_regs *regs);
extern void __get_user_1(void);
extern void __get_user_2(void);
extern void __get_user_4(void);
extern void __put_user_1(void);
extern void __put_user_2(void);
extern void __put_user_4(void);
extern void __put_user_8(void);
extern void __put_user_bad(void);
struct __large_struct { unsigned long buf[100]; };
extern long __get_user_bad(void);
unsigned long __copy_to_user_ll(void *to, const void *from, unsigned long n);
unsigned long __copy_from_user_ll(void *to, const void *from, unsigned long n);
static inline unsigned long
__copy_to_user(void *to, const void *from, unsigned long n)
{
        if (__builtin_constant_p(n)) {
                unsigned long ret;
                switch (n) {
                case 1:
                        do { ret = 0; switch (1) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "iq" (*(u8 *)from), "m"((*(struct __large_struct *)((u8 *)to))), "i"(1), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "ir" (*(u8 *)from), "m"((*(struct __large_struct *)((u8 *)to))), "i"(1), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "ir" (*(u8 *)from), "m"((*(struct __large_struct *)((u8 *)to))), "i"(1), "0"(ret)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(ret) : "A" ((__typeof__(*(u8 *)to))(*(u8 *)from)), "r" ((u8 *)to), "i"(-14), "0"(ret)); break; default: __put_user_bad(); } } while (0);
                        return ret;
                case 2:
                        do { ret = 0; switch (2) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "iq" (*(u16 *)from), "m"((*(struct __large_struct *)((u16 *)to))), "i"(2), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "ir" (*(u16 *)from), "m"((*(struct __large_struct *)((u16 *)to))), "i"(2), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "ir" (*(u16 *)from), "m"((*(struct __large_struct *)((u16 *)to))), "i"(2), "0"(ret)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(ret) : "A" ((__typeof__(*(u16 *)to))(*(u16 *)from)), "r" ((u16 *)to), "i"(-14), "0"(ret)); break; default: __put_user_bad(); } } while (0);
                        return ret;
                case 4:
                        do { ret = 0; switch (4) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "iq" (*(u32 *)from), "m"((*(struct __large_struct *)((u32 *)to))), "i"(4), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "ir" (*(u32 *)from), "m"((*(struct __large_struct *)((u32 *)to))), "i"(4), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret) : "ir" (*(u32 *)from), "m"((*(struct __large_struct *)((u32 *)to))), "i"(4), "0"(ret)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(ret) : "A" ((__typeof__(*(u32 *)to))(*(u32 *)from)), "r" ((u32 *)to), "i"(-14), "0"(ret)); break; default: __put_user_bad(); } } while (0);
                        return ret;
                }
        }
        return __copy_to_user_ll(to, from, n);
}
static inline unsigned long
__copy_from_user(void *to, const void *from, unsigned long n)
{
        if (__builtin_constant_p(n)) {
                unsigned long ret;
                switch (n) {
                case 1:
                        do { ret = 0; switch (1) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u8 *)to) : "m"((*(struct __large_struct *)(from))), "i"(1), "0"(ret));break; default: (*(u8 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                case 2:
                        do { ret = 0; switch (2) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u16 *)to) : "m"((*(struct __large_struct *)(from))), "i"(2), "0"(ret));break; default: (*(u16 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                case 4:
                        do { ret = 0; switch (4) { case 1: __asm__ __volatile__( "1:	mov""b"" %2,%""b""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""b"" %""b""1,%""b""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=q" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; case 2: __asm__ __volatile__( "1:	mov""w"" %2,%""w""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""w"" %""w""1,%""w""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; case 4: __asm__ __volatile__( "1:	mov""l"" %2,%""""1\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	xor""l"" %""""1,%""""1\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(ret), "=r" (*(u32 *)to) : "m"((*(struct __large_struct *)(from))), "i"(4), "0"(ret));break; default: (*(u32 *)to) = __get_user_bad(); } } while (0);
                        return ret;
                }
        }
        return __copy_from_user_ll(to, from, n);
}
static inline unsigned long
copy_to_user(void *to, const void *from, unsigned long n)
{
        if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (to),"g" ((int)(n)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0))
                n = __copy_to_user(to, from, n);
        return n;
}
static inline unsigned long
copy_from_user(void *to, const void *from, unsigned long n)
{
        if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (from),"g" ((int)(n)),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0))
                n = __copy_from_user(to, from, n);
        return n;
}
long strncpy_from_user(char *dst, const char *src, long count);
long __strncpy_from_user(char *dst, const char *src, long count);
long strnlen_user(const char *str, long n);
unsigned long clear_user(void *mem, unsigned long len);
unsigned long __clear_user(void *mem, unsigned long len);
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
extern int init_module(void);
extern void cleanup_module(void);
const struct exception_table_entry *
search_extable(const struct exception_table_entry *first,
               const struct exception_table_entry *last,
               unsigned long value);
struct kernel_symbol_group
{
        struct list_head list;
        struct module *owner;
        int gplonly;
        unsigned int num_syms;
        const struct kernel_symbol *syms;
        const unsigned long *crcs;
};
const struct exception_table_entry *search_exception_tables(unsigned long add);
struct exception_table
{
        struct list_head list;
        unsigned int num_entries;
        const struct exception_table_entry *entry;
};
void *__symbol_get(const char *symbol);
void *__symbol_get_gpl(const char *symbol);
struct module_ref
{
        atomic_t count;
} __attribute__((__aligned__((1 << (5)))));
enum module_state
{
        MODULE_STATE_LIVE,
        MODULE_STATE_COMING,
        MODULE_STATE_GOING,
};
struct module
{
        enum module_state state;
        struct list_head list;
        char name[(64 - sizeof(unsigned long))];
        struct kernel_symbol_group symbols;
        struct kernel_symbol_group gpl_symbols;
        struct exception_table extable;
        int (*init)(void);
        void *module_init;
        void *module_core;
        unsigned long init_size, core_size;
        struct mod_arch_specific arch;
        int unsafe;
        int license_gplok;
        Elf32_Sym *symtab;
        unsigned long num_syms;
        char *strtab;
        char *args;
};
static inline int module_is_live(struct module *mod)
{
        return mod->state != MODULE_STATE_GOING;
}
int module_text_address(unsigned long addr);
static inline int try_module_get(struct module *module)
{
        return !module || module_is_live(module);
}
static inline void module_put(struct module *module)
{
}
const char *module_address_lookup(unsigned long addr,
                                  unsigned long *symbolsize,
                                  unsigned long *offset,
                                  char **modname);
const struct exception_table_entry *search_module_extables(unsigned long addr);
extern struct module __this_module;
struct module __this_module
__attribute__((section(".gnu.linkonce.this_module"))) = {
        .name = "esp",
        .symbols = { .owner = &__this_module },
        .gpl_symbols = { .owner = &__this_module, .gplonly = 1 },
        .init = init_module,
};
static inline void __attribute__((deprecated)) __MOD_INC_USE_COUNT(struct module *module)
{
        do { if (module && !(module)->unsafe) { printk("<4>" "Module %s cannot be unloaded due to unsafe usage in" " %s:%u\n", (module)->name, "include/linux/module.h", 429); (module)->unsafe = 1; } } while(0);
        try_module_get(module);
}
static inline void __attribute__((deprecated)) __MOD_DEC_USE_COUNT(struct module *module)
{
        module_put(module);
}
struct obsolete_modparm {
        char name[64];
        char type[64-sizeof(void *)];
        void *addr;
};
static inline void __attribute__((deprecated)) _MOD_INC_USE_COUNT(struct module *module)
{
        do { if (module && !(module)->unsafe) { printk("<4>" "Module %s cannot be unloaded due to unsafe usage in" " %s:%u\n", (module)->name, "include/linux/module.h", 463); (module)->unsafe = 1; } } while(0);
        try_module_get(module);
}
extern spinlock_t modlist_lock;
extern struct list_head extables;
extern struct list_head symbols;
extern void inter_module_register(const char *, struct module *, const void *);
extern void inter_module_unregister(const char *);
extern const void *inter_module_get(const char *);
extern const void *inter_module_get_request(const char *, const char *);
extern void inter_module_put(const char *);
struct irqaction {
        void (*handler)(int, void *, struct pt_regs *);
        unsigned long flags;
        unsigned long mask;
        const char *name;
        void *dev_id;
        struct irqaction *next;
};
enum {
        TIMER_BH = 0,
        TQUEUE_BH,
        DIGI_BH,
        SERIAL_BH,
        RISCOM8_BH,
        SPECIALIX_BH,
        AURORA_BH,
        ESP_BH,
        SCSI_BH,
        IMMEDIATE_BH,
        CYCLADES_BH,
        CM206_BH,
        JS_BH,
        MACSERIAL_BH,
        ISICOM_BH
};
static __inline__ int irq_cannonicalize(int irq)
{
        return ((irq == 2) ? 9 : irq);
}
extern void disable_irq(unsigned int);
extern void disable_irq_nosync(unsigned int);
extern void enable_irq(unsigned int);
extern void release_x86_irqs(struct task_struct *);
struct hw_interrupt_type {
        const char * typename;
        unsigned int (*startup)(unsigned int irq);
        void (*shutdown)(unsigned int irq);
        void (*enable)(unsigned int irq);
        void (*disable)(unsigned int irq);
        void (*ack)(unsigned int irq);
        void (*end)(unsigned int irq);
        void (*set_affinity)(unsigned int irq, unsigned long mask);
};
typedef struct hw_interrupt_type hw_irq_controller;
typedef struct {
        unsigned int status;
        hw_irq_controller *handler;
        struct irqaction *action;
        unsigned int depth;
        spinlock_t lock;
} __attribute__((__aligned__((1 << (5))))) irq_desc_t;
extern irq_desc_t irq_desc [224];
typedef int (*initcall_t)(void);
typedef void (*exitcall_t)(void);
int __attribute__ ((__section__ (".init.text"))) profile_setup(char * str);
void __attribute__ ((__section__ (".init.text"))) profile_init(void);
extern unsigned int * prof_buffer;
extern unsigned long prof_len;
extern unsigned long prof_shift;
extern int prof_on;
enum profile_type {
        EXIT_TASK,
        EXIT_MMAP,
        EXEC_UNMAP
};
struct notifier_block;
struct task_struct;
struct mm_struct;
void profile_exit_task(struct task_struct * task);
void profile_exec_unmap(struct mm_struct * mm);
void profile_exit_mmap(struct mm_struct * mm);
int profile_event_register(enum profile_type, struct notifier_block * n);
int profile_event_unregister(enum profile_type, struct notifier_block * n);
extern int irq_vector[224];
extern void (*interrupt[224])(void);
extern __attribute__((regparm(0))) void reschedule_interrupt(void);
extern __attribute__((regparm(0))) void invalidate_interrupt(void);
extern __attribute__((regparm(0))) void call_function_interrupt(void);
extern __attribute__((regparm(0))) void apic_timer_interrupt(void);
extern __attribute__((regparm(0))) void error_interrupt(void);
extern __attribute__((regparm(0))) void spurious_interrupt(void);
extern __attribute__((regparm(0))) void thermal_interrupt(struct pt_regs);
extern void mask_irq(unsigned int irq);
extern void unmask_irq(unsigned int irq);
extern void disable_8259A_irq(unsigned int irq);
extern void enable_8259A_irq(unsigned int irq);
extern int i8259A_irq_pending(unsigned int irq);
extern void make_8259A_irq(unsigned int irq);
extern void init_8259A(int aeoi);
extern void send_IPI_self(int vector) __attribute__((regparm(3)));
extern void init_VISWS_APIC_irqs(void);
extern void setup_IO_APIC(void);
extern void disable_IO_APIC(void);
extern void print_IO_APIC(void);
extern int IO_APIC_get_PCI_irq_vector(int bus, int slot, int fn);
extern void send_IPI(int dest, int vector);
extern unsigned long io_apic_irqs;
extern atomic_t irq_err_count;
extern atomic_t irq_mis_count;
extern char _stext, _etext;
static inline void x86_do_profile(struct pt_regs * regs)
{
        unsigned long eip;
        extern unsigned long prof_cpu_mask;
        extern void x86_profile_hook(struct pt_regs *);
        x86_profile_hook(regs);
        if (((0x00020000 & (regs)->eflags) || (3 & (regs)->xcs)))
                return;
        if (!prof_buffer)
                return;
        eip = regs->eip;
        if (!((1<<(current_thread_info()->cpu)) & prof_cpu_mask))
                return;
        eip -= (unsigned long) &_stext;
        eip >>= prof_shift;
        if (eip > prof_len-1)
                eip = prof_len-1;
        atomic_inc((atomic_t *)&prof_buffer[eip]);
}
struct notifier_block;
int register_profile_notifier(struct notifier_block * nb);
int unregister_profile_notifier(struct notifier_block * nb);
static inline void hw_resend_irq(struct hw_interrupt_type *h, unsigned int i)
{
        if ((((i) >= 16) || ((1<<(i)) & io_apic_irqs)))
                send_IPI_self(irq_vector[i]);
}
extern int handle_IRQ_event(unsigned int, struct pt_regs *, struct irqaction *);
extern int setup_irq(unsigned int , struct irqaction * );
extern hw_irq_controller no_irq_type;
extern void no_action(int cpl, void *dev_id, struct pt_regs *regs);
typedef struct {
        unsigned int __softirq_pending;
        unsigned int __syscall_count;
        struct task_struct * __ksoftirqd_task;
        unsigned long idle_timestamp;
        unsigned int __nmi_count;
        unsigned int apic_timer_irqs;
} __attribute__((__aligned__((1 << (5))))) irq_cpustat_t;
extern irq_cpustat_t irq_stat[];
  extern void synchronize_irq(unsigned int irq);
enum
{
        HI_SOFTIRQ=0,
        NET_TX_SOFTIRQ,
        NET_RX_SOFTIRQ,
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
extern void cpu_raise_softirq(unsigned int cpu, unsigned int nr) __attribute__((regparm(3)));
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
struct tasklet_head
{
        struct tasklet_struct *list;
} __attribute__ ((__aligned__((1 << (5)))));
extern struct tasklet_head tasklet_vec[8];
extern struct tasklet_head tasklet_hi_vec[8];
static inline int tasklet_trylock(struct tasklet_struct *t)
{
        return !test_and_set_bit(TASKLET_STATE_RUN, &(t)->state);
}
static inline void tasklet_unlock(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        clear_bit(TASKLET_STATE_RUN, &(t)->state);
}
static inline void tasklet_unlock_wait(struct tasklet_struct *t)
{
        while ((__builtin_constant_p(TASKLET_STATE_RUN) ? constant_test_bit((TASKLET_STATE_RUN),(&(t)->state)) : variable_test_bit((TASKLET_STATE_RUN),(&(t)->state)))) { __asm__ __volatile__("": : :"memory"); }
}
extern void __tasklet_schedule(struct tasklet_struct *t) __attribute__((regparm(3)));
static inline void tasklet_schedule(struct tasklet_struct *t)
{
        if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
                __tasklet_schedule(t);
}
extern void __tasklet_hi_schedule(struct tasklet_struct *t) __attribute__((regparm(3)));
static inline void tasklet_hi_schedule(struct tasklet_struct *t)
{
        if (!test_and_set_bit(TASKLET_STATE_SCHED, &t->state))
                __tasklet_hi_schedule(t);
}
static inline void tasklet_disable_nosync(struct tasklet_struct *t)
{
        atomic_inc(&t->count);
        __asm__ __volatile__("": : :"memory");
}
static inline void tasklet_disable(struct tasklet_struct *t)
{
        tasklet_disable_nosync(t);
        tasklet_unlock_wait(t);
        __asm__ __volatile__ ("lock; addl $0,0(%%esp)": : :"memory");
}
static inline void tasklet_enable(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        atomic_dec(&t->count);
}
static inline void tasklet_hi_enable(struct tasklet_struct *t)
{
        __asm__ __volatile__("": : :"memory");
        atomic_dec(&t->count);
}
extern void tasklet_kill(struct tasklet_struct *t);
extern void tasklet_init(struct tasklet_struct *t,
                         void (*func)(unsigned long), unsigned long data);
extern struct tasklet_struct bh_task_vec[];
extern spinlock_t global_bh_lock;
static inline void mark_bh(int nr)
{
        tasklet_hi_schedule(bh_task_vec+nr);
}
extern void init_bh(int nr, void (*routine)(void));
extern void remove_bh(int nr);
extern unsigned long probe_irq_on(void);
extern int probe_irq_off(unsigned long);
extern unsigned int probe_irq_mask(unsigned long);
typedef struct {
        unsigned short value;
} kdev_t;
static inline unsigned int kdev_val(kdev_t dev)
{
        return dev.value;
}
static inline kdev_t val_to_kdev(unsigned int val)
{
        kdev_t dev;
        dev.value = val;
        return dev;
}
extern const char * kdevname(kdev_t);
static inline int kdev_same(kdev_t dev1, kdev_t dev2)
{
        return dev1.value == dev2.value;
}
static inline int kdev_t_to_nr(kdev_t dev)
{
        return ((((((dev).value >> 8) & 0xff)) << 8) | (((dev).value & 0xff)));
}
static inline kdev_t to_kdev_t(int dev)
{
        return ((kdev_t) { (((((unsigned int) ((dev) >> 8))) << 8) + (((unsigned int) ((dev) & ((1U << 8) - 1))))) } );
}
struct rcu_head {
        struct list_head list;
        void (*func)(void *obj);
        void *arg;
};
struct rcu_ctrlblk {
        spinlock_t mutex;
        long curbatch;
        long maxbatch;
        unsigned long rcu_cpu_mask;
};
static inline int rcu_batch_before(long a, long b)
{
        return (a - b) < 0;
}
static inline int rcu_batch_after(long a, long b)
{
        return (a - b) > 0;
}
struct rcu_data {
        long qsctr;
        long last_qsctr;
        long batch;
        struct list_head nxtlist;
        struct list_head curlist;
};
extern __typeof__(struct rcu_data) rcu_data__per_cpu;
extern struct rcu_ctrlblk rcu_ctrlblk;
static inline int rcu_pending(int cpu)
{
        if ((!list_empty(&((*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&rcu_data__per_cpu)); (typeof(&rcu_data__per_cpu)) (__ptr + (__per_cpu_offset[(cpu)])); })).curlist)) &&
             rcu_batch_before(((*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&rcu_data__per_cpu)); (typeof(&rcu_data__per_cpu)) (__ptr + (__per_cpu_offset[(cpu)])); })).batch), rcu_ctrlblk.curbatch)) ||
            (list_empty(&((*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&rcu_data__per_cpu)); (typeof(&rcu_data__per_cpu)) (__ptr + (__per_cpu_offset[(cpu)])); })).curlist)) &&
                         !list_empty(&((*({ unsigned long __ptr; __asm__ ("" : "=g"(__ptr) : "0"(&rcu_data__per_cpu)); (typeof(&rcu_data__per_cpu)) (__ptr + (__per_cpu_offset[(cpu)])); })).nxtlist))) ||
            (__builtin_constant_p(cpu) ? constant_test_bit((cpu),(&rcu_ctrlblk.rcu_cpu_mask)) : variable_test_bit((cpu),(&rcu_ctrlblk.rcu_cpu_mask))))
                return 1;
        else
                return 0;
}
extern void rcu_init(void);
extern void rcu_check_callbacks(int cpu, int user);
extern void call_rcu(struct rcu_head *head, void (*func)(void *arg), void *arg) __attribute__((regparm(3)));
extern void synchronize_kernel(void);
struct vfsmount;
struct qstr {
        const unsigned char * name;
        unsigned int len;
        unsigned int hash;
        char name_str[0];
};
struct dentry_stat_t {
        int nr_dentry;
        int nr_unused;
        int age_limit;
        int want_pages;
        int dummy[2];
};
extern struct dentry_stat_t dentry_stat;
static __inline__ unsigned long partial_name_hash(unsigned long c, unsigned long prevhash)
{
        return (prevhash + (c << 4) + (c >> 4)) * 11;
}
static __inline__ unsigned long end_name_hash(unsigned long hash)
{
        return (unsigned int) hash;
}
static __inline__ unsigned int full_name_hash(const unsigned char * name, unsigned int len)
{
        unsigned long hash = 0;
        while (len--)
                hash = partial_name_hash(*name++, hash);
        return end_name_hash(hash);
}
struct dcookie_struct;
struct dentry {
        atomic_t d_count;
        unsigned long d_vfs_flags;
        spinlock_t d_lock;
        unsigned int d_flags;
        unsigned long d_move_count;
        struct inode * d_inode;
        struct dentry * d_parent;
        struct list_head * d_bucket;
        struct list_head d_hash;
        struct list_head d_lru;
        struct list_head d_child;
        struct list_head d_subdirs;
        struct list_head d_alias;
        int d_mounted;
        struct qstr d_name;
        struct qstr * d_qstr;
        unsigned long d_time;
        struct dentry_operations *d_op;
        struct super_block * d_sb;
        void * d_fsdata;
        struct rcu_head d_rcu;
        struct dcookie_struct * d_cookie;
        unsigned char d_iname[16];
} __attribute__((__aligned__((1 << (5)))));
struct dentry_operations {
        int (*d_revalidate)(struct dentry *, int);
        int (*d_hash) (struct dentry *, struct qstr *);
        int (*d_compare) (struct dentry *, struct qstr *, struct qstr *);
        int (*d_delete)(struct dentry *);
        void (*d_release)(struct dentry *);
        void (*d_iput)(struct dentry *, struct inode *);
};
extern spinlock_t dcache_lock;
extern rwlock_t dparent_lock;
static __inline__ void __d_drop(struct dentry * dentry)
{
        dentry->d_vfs_flags |= 0x0010;
        list_del_rcu(&dentry->d_hash);
}
static __inline__ void d_drop(struct dentry * dentry)
{
        do { do { } while (0); _raw_spin_lock(&dcache_lock); } while(0);
        __d_drop(dentry);
        do { _raw_spin_unlock(&dcache_lock); do { } while (0); } while (0);
}
static __inline__ int dname_external(struct dentry *d)
{
        return d->d_name.name != d->d_iname;
}
extern void d_instantiate(struct dentry *, struct inode *);
extern void d_delete(struct dentry *);
extern struct dentry * d_alloc(struct dentry *, const struct qstr *);
extern struct dentry * d_alloc_anon(struct inode *);
extern struct dentry * d_splice_alias(struct inode *, struct dentry *);
extern void shrink_dcache_sb(struct super_block *);
extern void shrink_dcache_parent(struct dentry *);
extern void shrink_dcache_anon(struct list_head *);
extern int d_invalidate(struct dentry *);
extern struct dentry * d_alloc_root(struct inode *);
extern void d_genocide(struct dentry *);
extern struct dentry *d_find_alias(struct inode *);
extern void d_prune_aliases(struct inode *);
extern int have_submounts(struct dentry *);
extern void d_rehash(struct dentry *);
static __inline__ void d_add(struct dentry * entry, struct inode * inode)
{
        d_instantiate(entry, inode);
        d_rehash(entry);
}
extern void d_move(struct dentry *, struct dentry *);
extern struct dentry * d_lookup(struct dentry *, struct qstr *);
extern int d_validate(struct dentry *, struct dentry *);
extern char * d_path(struct dentry *, struct vfsmount *, char *, int);
static __inline__ struct dentry * dget(struct dentry *dentry)
{
        if (dentry) {
                atomic_inc(&dentry->d_count);
                dentry->d_vfs_flags |= 0x0008;
        }
        return dentry;
}
extern struct dentry * dget_locked(struct dentry *);
static __inline__ int d_unhashed(struct dentry *dentry)
{
        return (dentry->d_vfs_flags & 0x0010);
}
extern void dput(struct dentry *);
static __inline__ int d_mountpoint(struct dentry *dentry)
{
        return dentry->d_mounted;
}
extern struct vfsmount *lookup_mnt(struct vfsmount *, struct dentry *);
struct radix_tree_node;
struct radix_tree_root {
        unsigned int height;
        int gfp_mask;
        struct radix_tree_node *rnode;
};
extern int radix_tree_insert(struct radix_tree_root *, unsigned long, void *);
extern void *radix_tree_lookup(struct radix_tree_root *, unsigned long);
extern int radix_tree_delete(struct radix_tree_root *, unsigned long);
extern unsigned int
radix_tree_gang_lookup(struct radix_tree_root *root, void **results,
                        unsigned long first_index, unsigned int max_items);
int radix_tree_preload(int gfp_mask);
static inline void radix_tree_preload_end(void)
{
        do { } while (0);
}
struct iovec;
struct nameidata;
struct pipe_inode_info;
struct poll_table_struct;
struct statfs;
struct vm_area_struct;
struct vfsmount;
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
extern int leases_enable, dir_notify_enable, lease_break_time;
extern void update_atime (struct inode *);
extern void inode_init(unsigned long);
extern void mnt_init(unsigned long);
extern void files_init(unsigned long);
struct buffer_head;
typedef int (get_block_t)(struct inode *inode, sector_t iblock,
                        struct buffer_head *bh_result, int create);
typedef int (get_blocks_t)(struct inode *inode, sector_t iblock,
                        unsigned long max_blocks,
                        struct buffer_head *bh_result, int create);
struct iattr {
        unsigned int ia_valid;
        umode_t ia_mode;
        uid_t ia_uid;
        gid_t ia_gid;
        loff_t ia_size;
        struct timespec ia_atime;
        struct timespec ia_mtime;
        struct timespec ia_ctime;
        unsigned int ia_attr_flags;
};
typedef __kernel_uid32_t qid_t;
typedef __u64 qsize_t;
extern spinlock_t dq_list_lock;
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
        unsigned long dqi_flags;
        unsigned int dqi_bgrace;
        unsigned int dqi_igrace;
        union {
                struct v1_mem_dqinfo v1_i;
                struct v2_mem_dqinfo v2_i;
        } u;
};
extern inline void mark_info_dirty(struct mem_dqinfo *info)
{
        set_bit(16, &info->dqi_flags);
}
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
        struct list_head dq_hash;
        struct list_head dq_inuse;
        struct list_head dq_free;
        struct semaphore dq_lock;
        atomic_t dq_count;
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
};
struct dquot_operations {
        void (*initialize) (struct inode *, int);
        void (*drop) (struct inode *);
        int (*alloc_space) (struct inode *, qsize_t, int);
        int (*alloc_inode) (const struct inode *, unsigned long);
        void (*free_space) (struct inode *, qsize_t);
        void (*free_inode) (const struct inode *, unsigned long);
        int (*transfer) (struct inode *, struct iattr *);
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
        struct file *files[2];
        struct mem_dqinfo info[2];
        struct quota_format_ops *ops[2];
};
int register_quota_format(struct quota_format_type *fmt);
void unregister_quota_format(struct quota_format_type *fmt);
struct page;
struct address_space;
struct writeback_control;
struct kiocb;
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
        int (*direct_IO)(int, struct kiocb *, const struct iovec *iov,
                        loff_t offset, unsigned long nr_segs);
};
struct backing_dev_info;
struct address_space {
        struct inode *host;
        struct radix_tree_root page_tree;
        rwlock_t page_lock;
        struct list_head clean_pages;
        struct list_head dirty_pages;
        struct list_head locked_pages;
        struct list_head io_pages;
        unsigned long nrpages;
        struct address_space_operations *a_ops;
        struct list_head i_mmap;
        struct list_head i_mmap_shared;
        struct semaphore i_shared_sem;
        unsigned long dirtied_when;
        int gfp_mask;
        struct backing_dev_info *backing_dev_info;
        spinlock_t private_lock;
        struct list_head private_list;
        struct address_space *assoc_mapping;
};
struct char_device {
        struct list_head hash;
        atomic_t count;
        dev_t dev;
};
struct block_device {
        struct list_head bd_hash;
        atomic_t bd_count;
        struct inode * bd_inode;
        dev_t bd_dev;
        int bd_openers;
        struct semaphore bd_sem;
        struct list_head bd_inodes;
        void * bd_holder;
        int bd_holders;
        struct block_device * bd_contains;
        unsigned bd_block_size;
        sector_t bd_offset;
        unsigned bd_part_count;
        int bd_invalidated;
        struct gendisk * bd_disk;
};
struct inode {
        struct list_head i_hash;
        struct list_head i_list;
        struct list_head i_dentry;
        unsigned long i_ino;
        atomic_t i_count;
        umode_t i_mode;
        unsigned int i_nlink;
        uid_t i_uid;
        gid_t i_gid;
        kdev_t i_rdev;
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
        struct char_device *i_cdev;
        unsigned long i_dnotify_mask;
        struct dnotify_struct *i_dnotify;
        unsigned long i_state;
        unsigned int i_flags;
        unsigned char i_sock;
        atomic_t i_writecount;
        void *i_security;
        __u32 i_generation;
        union {
                void *generic_ip;
        } u;
};
struct fown_struct {
        rwlock_t lock;
        int pid;
        uid_t uid, euid;
        int signum;
        void *security;
};
struct file_ra_state {
        unsigned long start;
        unsigned long size;
        unsigned long next_size;
        unsigned long prev_page;
        unsigned long ahead_start;
        unsigned long ahead_size;
        unsigned long ra_pages;
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
        int f_error;
        struct file_ra_state f_ra;
        unsigned long f_version;
        void *f_security;
        void *private_data;
        struct list_head f_ep_links;
        spinlock_t f_ep_lock;
};
extern spinlock_t files_lock;
extern int init_private_file(struct file *, struct dentry *, int);
typedef struct files_struct *fl_owner_t;
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
        NFSERR_READDIR_NOSPC = 10030,
        NFSERR_LEASE_MOVED = 10031,
        NFSERR_ATTRNOTSUPP = 10032,
        NFSERR_NO_GRACE = 10033,
        NFSERR_RECLAIM_BAD = 10034,
        NFSERR_RECLAIM_CONFLICT = 10035,
        NFSERR_BAD_XDR = 10036,
        NFSERR_LOCKS_HELD = 10037
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
enum nfs3_stable_how {
        NFS_UNSTABLE = 0,
        NFS_DATA_SYNC = 1,
        NFS_FILE_SYNC = 2
};
struct nfs_lock_info {
        u32 state;
        u32 flags;
        struct nlm_host *host;
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
        void (*fl_notify)(struct file_lock *);
        void (*fl_insert)(struct file_lock *);
        void (*fl_remove)(struct file_lock *);
        struct fasync_struct * fl_fasync;
        unsigned long fl_break_time;
        union {
                struct nfs_lock_info nfs_fl;
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
extern int fcntl_setlk(struct file *, unsigned int, struct flock *);
extern int fcntl_getlk64(struct file *, struct flock64 *);
extern int fcntl_setlk64(struct file *, unsigned int, struct flock64 *);
extern void locks_init_lock(struct file_lock *);
extern void locks_copy_lock(struct file_lock *, struct file_lock *);
extern void locks_remove_posix(struct file *, fl_owner_t);
extern void locks_remove_flock(struct file *);
extern struct file_lock *posix_test_lock(struct file *, struct file_lock *);
extern int posix_lock_file(struct file *, struct file_lock *);
extern void posix_block_lock(struct file_lock *, struct file_lock *);
extern void posix_unblock_lock(struct file *, struct file_lock *);
extern int posix_locks_deadlock(struct file_lock *, struct file_lock *);
extern int __break_lease(struct inode *inode, unsigned int flags);
extern void lease_get_mtime(struct inode *, struct timespec *time);
extern int lock_may_read(struct inode *, loff_t start, unsigned long count);
extern int lock_may_write(struct inode *, loff_t start, unsigned long count);
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
        struct list_head s_dirty;
        struct list_head s_io;
        struct list_head s_anon;
        struct list_head s_files;
        struct block_device *s_bdev;
        struct list_head s_instances;
        struct quota_info s_dquot;
        char s_id[32];
        void *s_fs_info;
        struct semaphore s_vfs_rename_sem;
};
static inline void lock_super(struct super_block * sb)
{
        down(&sb->s_lock);
}
static inline void unlock_super(struct super_block * sb)
{
        up(&sb->s_lock);
}
extern int vfs_create(struct inode *, struct dentry *, int);
extern int vfs_mkdir(struct inode *, struct dentry *, int);
extern int vfs_mknod(struct inode *, struct dentry *, int, dev_t);
extern int vfs_symlink(struct inode *, struct dentry *, const char *);
extern int vfs_link(struct dentry *, struct inode *, struct dentry *);
extern int vfs_rmdir(struct inode *, struct dentry *);
extern int vfs_unlink(struct inode *, struct dentry *);
extern int vfs_rename(struct inode *, struct dentry *, struct inode *, struct dentry *);
typedef int (*filldir_t)(void *, const char *, int, loff_t, ino_t, unsigned);
struct block_device_operations {
        int (*open) (struct inode *, struct file *);
        int (*release) (struct inode *, struct file *);
        int (*ioctl) (struct inode *, struct file *, unsigned, unsigned long);
        int (*media_changed) (struct gendisk *);
        int (*revalidate_disk) (struct gendisk *);
        struct module *owner;
};
typedef struct {
        size_t written;
        size_t count;
        char * buf;
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
};
struct inode_operations {
        int (*create) (struct inode *,struct dentry *,int);
        struct dentry * (*lookup) (struct inode *,struct dentry *);
        int (*link) (struct dentry *,struct inode *,struct dentry *);
        int (*unlink) (struct inode *,struct dentry *);
        int (*symlink) (struct inode *,struct dentry *,const char *);
        int (*mkdir) (struct inode *,struct dentry *,int);
        int (*rmdir) (struct inode *,struct dentry *);
        int (*mknod) (struct inode *,struct dentry *,int,dev_t);
        int (*rename) (struct inode *, struct dentry *,
                        struct inode *, struct dentry *);
        int (*readlink) (struct dentry *, char *,int);
        int (*follow_link) (struct dentry *, struct nameidata *);
        void (*truncate) (struct inode *);
        int (*permission) (struct inode *, int);
        int (*setattr) (struct dentry *, struct iattr *);
        int (*getattr) (struct vfsmount *mnt, struct dentry *, struct kstat *);
        int (*setxattr) (struct dentry *, const char *,const void *,size_t,int);
        ssize_t (*getxattr) (struct dentry *, const char *, void *, size_t,int);
        ssize_t (*listxattr) (struct dentry *, char *, size_t, int);
        int (*removexattr) (struct dentry *, const char *, int);
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
        void (*write_inode) (struct inode *, int);
        void (*put_inode) (struct inode *);
        void (*drop_inode) (struct inode *);
        void (*delete_inode) (struct inode *);
        void (*put_super) (struct super_block *);
        void (*write_super) (struct super_block *);
        int (*sync_fs)(struct super_block *sb, int wait);
        void (*write_super_lockfs) (struct super_block *);
        void (*unlockfs) (struct super_block *);
        int (*statfs) (struct super_block *, struct statfs *);
        int (*remount_fs) (struct super_block *, int *, char *);
        void (*clear_inode) (struct inode *);
        void (*umount_begin) (struct super_block *);
        int (*show_options)(struct seq_file *, struct vfsmount *);
};
extern void __mark_inode_dirty(struct inode *, int);
static inline void mark_inode_dirty(struct inode *inode)
{
        __mark_inode_dirty(inode, (1 | 2 | 4));
}
static inline void mark_inode_dirty_sync(struct inode *inode)
{
        __mark_inode_dirty(inode, 1);
}
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
struct file_system_type {
        const char *name;
        int fs_flags;
        struct super_block *(*get_sb) (struct file_system_type *, int, char *, void *);
        void (*kill_sb) (struct super_block *);
        struct module *owner;
        struct file_system_type * next;
        struct list_head fs_supers;
};
struct super_block *get_sb_bdev(struct file_system_type *fs_type,
        int flags, char *dev_name, void * data,
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
extern int register_filesystem(struct file_system_type *);
extern int unregister_filesystem(struct file_system_type *);
extern struct vfsmount *kern_mount(struct file_system_type *);
extern int may_umount(struct vfsmount *);
extern long do_mount(char *, char *, char *, unsigned long, void *);
extern int vfs_statfs(struct super_block *, struct statfs *);
extern int locks_mandatory_locked(struct inode *);
extern int locks_mandatory_area(int, struct inode *, struct file *, loff_t, size_t);
static inline int locks_verify_locked(struct inode *inode)
{
        if ((((inode)->i_sb->s_flags & (64)) && ((inode)->i_mode & (0002000 | 00010)) == 0002000))
                return locks_mandatory_locked(inode);
        return 0;
}
static inline int locks_verify_area(int read_write, struct inode *inode,
                                    struct file *filp, loff_t offset,
                                    size_t count)
{
        if (inode->i_flock && (((inode)->i_sb->s_flags & (64)) && ((inode)->i_mode & (0002000 | 00010)) == 0002000))
                return locks_mandatory_area(read_write, inode, filp, offset, count);
        return 0;
}
static inline int locks_verify_truncate(struct inode *inode,
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
static inline int break_lease(struct inode *inode, unsigned int mode)
{
        if (inode->i_flock)
                return __break_lease(inode, mode);
        return 0;
}
 __attribute__((regparm(0))) long sys_open(const char *, int, int);
 __attribute__((regparm(0))) long sys_close(unsigned int);
extern int do_truncate(struct dentry *, loff_t start);
extern struct file *filp_open(const char *, int, int);
extern struct file * dentry_open(struct dentry *, struct vfsmount *, int);
extern int filp_close(struct file *, fl_owner_t id);
extern char * getname(const char *);
extern void vfs_caches_init(unsigned long);
enum {BDEV_FILE, BDEV_SWAP, BDEV_FS, BDEV_RAW};
extern int register_blkdev(unsigned int, const char *, struct block_device_operations *);
extern int unregister_blkdev(unsigned int, const char *);
extern struct block_device *bdget(dev_t);
extern int bd_acquire(struct inode *inode);
extern void bd_forget(struct inode *inode);
extern void bdput(struct block_device *);
extern struct char_device *cdget(dev_t);
extern void cdput(struct char_device *);
extern int blkdev_open(struct inode *, struct file *);
extern int blkdev_close(struct inode *, struct file *);
extern struct file_operations def_blk_fops;
extern struct address_space_operations def_blk_aops;
extern struct file_operations def_chr_fops;
extern struct file_operations bad_sock_fops;
extern struct file_operations def_fifo_fops;
extern int ioctl_by_bdev(struct block_device *, unsigned, unsigned long);
extern int blkdev_ioctl(struct inode *, struct file *, unsigned, unsigned long);
extern int blkdev_get(struct block_device *, mode_t, unsigned, int);
extern int blkdev_put(struct block_device *, int);
extern int bd_claim(struct block_device *, void *);
extern void bd_release(struct block_device *);
extern void blk_run_queues(void);
extern int register_chrdev(unsigned int, const char *, struct file_operations *);
extern int unregister_chrdev(unsigned int, const char *);
extern int chrdev_open(struct inode *, struct file *);
extern const char *__bdevname(dev_t);
extern inline const char *bdevname(struct block_device *bdev)
{
        return __bdevname(bdev->bd_dev);
}
extern struct block_device *lookup_bdev(const char *);
extern struct block_device *open_bdev_excl(const char *, int, int, void *);
extern void close_bdev_excl(struct block_device *, int);
extern const char * cdevname(kdev_t);
extern const char * kdevname(kdev_t);
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
extern int full_check_disk_change(struct block_device *);
extern int __check_disk_change(dev_t);
extern int invalidate_inodes(struct super_block *);
extern int invalidate_device(kdev_t, int);
unsigned long invalidate_mapping_pages(struct address_space *mapping,
                                        unsigned long start, unsigned long end);
unsigned long invalidate_inode_pages(struct address_space *mapping);
extern void invalidate_inode_pages2(struct address_space *mapping);
extern void write_inode_now(struct inode *, int);
extern int filemap_fdatawrite(struct address_space *);
extern int filemap_fdatawait(struct address_space *);
extern void sync_supers(void);
extern void sync_filesystems(int wait);
extern sector_t bmap(struct inode *, sector_t);
extern int setattr_mask(unsigned int);
extern int notify_change(struct dentry *, struct iattr *);
extern int permission(struct inode *, int);
extern int vfs_permission(struct inode *, int);
extern int get_write_access(struct inode *);
extern int deny_write_access(struct file *);
static inline void put_write_access(struct inode * inode)
{
        atomic_dec(&inode->i_writecount);
}
static inline void allow_write_access(struct file *file)
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
static inline void *ERR_PTR(long error)
{
        return (void *) error;
}
static inline long PTR_ERR(const void *ptr)
{
        return (long) ptr;
}
static inline long IS_ERR(const void *ptr)
{
        return (unsigned long)ptr > (unsigned long)-1000L;
}
extern loff_t default_llseek(struct file *file, loff_t offset, int origin);
extern void inode_init_once(struct inode *);
extern void iput(struct inode *);
extern struct inode * igrab(struct inode *);
extern ino_t iunique(struct super_block *, ino_t);
extern int inode_needs_sync(struct inode *inode);
extern void generic_delete_inode(struct inode *inode);
extern struct inode *ilookup5(struct super_block *sb, unsigned long hashval,
                int (*test)(struct inode *, void *), void *data);
extern struct inode *ilookup(struct super_block *sb, unsigned long ino);
extern struct inode * iget5_locked(struct super_block *, unsigned long, int (*test)(struct inode *, void *), int (*set)(struct inode *, void *), void *);
extern struct inode * iget_locked(struct super_block *, unsigned long);
extern void unlock_new_inode(struct inode *);
static inline struct inode *iget(struct super_block *sb, unsigned long ino)
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
extern void remove_suid(struct dentry *);
extern void __insert_inode_hash(struct inode *, unsigned long hashval);
extern void remove_inode_hash(struct inode *);
static inline void insert_inode_hash(struct inode *inode) {
        __insert_inode_hash(inode, inode->i_ino);
}
extern struct file * get_empty_filp(void);
extern void file_move(struct file *f, struct list_head *list);
struct bio;
extern int submit_bio(int, struct bio *);
extern int bdev_read_only(struct block_device *);
extern int set_blocksize(struct block_device *, int);
extern int sb_set_blocksize(struct super_block *, int);
extern int sb_min_blocksize(struct super_block *, int);
extern int generic_file_mmap(struct file *, struct vm_area_struct *);
extern int generic_file_readonly_mmap(struct file *, struct vm_area_struct *);
extern int file_read_actor(read_descriptor_t * desc, struct page *page, unsigned long offset, unsigned long size);
extern int file_send_actor(read_descriptor_t * desc, struct page *page, unsigned long offset, unsigned long size);
extern ssize_t generic_file_read(struct file *, char *, size_t, loff_t *);
int generic_write_checks(struct inode *inode, struct file *file,
                        loff_t *pos, size_t *count, int isblk);
extern ssize_t generic_file_write(struct file *, const char *, size_t, loff_t *);
extern ssize_t generic_file_aio_read(struct kiocb *, char *, size_t, loff_t);
extern ssize_t generic_file_aio_write(struct kiocb *, const char *, size_t, loff_t);
extern ssize_t generic_file_aio_write_nolock(struct kiocb *, const struct iovec *,
                                unsigned long, loff_t *);
extern ssize_t do_sync_read(struct file *filp, char *buf, size_t len, loff_t *ppos);
extern ssize_t do_sync_write(struct file *filp, const char *buf, size_t len, loff_t *ppos);
ssize_t generic_file_write_nolock(struct file *file, const struct iovec *iov,
                                unsigned long nr_segs, loff_t *ppos);
extern ssize_t generic_file_sendfile(struct file *, loff_t *, size_t, read_actor_t, void *);
extern void do_generic_mapping_read(struct address_space *, struct file_ra_state *, struct file *,
                                    loff_t *, read_descriptor_t *, read_actor_t);
extern void
file_ra_state_init(struct file_ra_state *ra, struct address_space *mapping);
extern ssize_t generic_file_direct_IO(int rw, struct kiocb *iocb,
        const struct iovec *iov, loff_t offset, unsigned long nr_segs);
extern int blockdev_direct_IO(int rw, struct kiocb *iocb, struct inode *inode,
        struct block_device *bdev, const struct iovec *iov, loff_t offset,
        unsigned long nr_segs, get_blocks_t *get_blocks);
extern ssize_t generic_file_readv(struct file *filp, const struct iovec *iov,
        unsigned long nr_segs, loff_t *ppos);
ssize_t generic_file_writev(struct file *filp, const struct iovec *iov,
                        unsigned long nr_segs, loff_t *ppos);
extern loff_t no_llseek(struct file *file, loff_t offset, int origin);
extern loff_t generic_file_llseek(struct file *file, loff_t offset, int origin);
extern loff_t remote_llseek(struct file *file, loff_t offset, int origin);
extern int generic_file_open(struct inode * inode, struct file * filp);
static inline void do_generic_file_read(struct file * filp, loff_t *ppos,
                                        read_descriptor_t * desc,
                                        read_actor_t actor)
{
        do_generic_mapping_read(filp->f_dentry->d_inode->i_mapping,
                                &filp->f_ra,
                                filp,
                                ppos,
                                desc,
                                actor);
}
extern struct file_operations generic_ro_fops;
extern int vfs_readlink(struct dentry *, char *, int, const char *);
extern int vfs_follow_link(struct nameidata *, const char *);
extern int page_readlink(struct dentry *, char *, int);
extern int page_follow_link(struct dentry *, struct nameidata *);
extern int page_symlink(struct inode *inode, const char *symname, int len);
extern struct inode_operations page_symlink_inode_operations;
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
extern struct file_system_type *get_fs_type(const char *name);
extern struct super_block *get_super(struct block_device *);
extern struct super_block *user_get_super(dev_t);
extern void drop_super(struct super_block *sb);
extern int dcache_dir_open(struct inode *, struct file *);
extern int dcache_dir_close(struct inode *, struct file *);
extern loff_t dcache_dir_lseek(struct file *, loff_t, int);
extern int dcache_readdir(struct file *, void *, filldir_t);
extern int simple_getattr(struct vfsmount *, struct dentry *, struct kstat *);
extern int simple_statfs(struct super_block *, struct statfs *);
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
extern struct dentry *simple_lookup(struct inode *, struct dentry *);
extern ssize_t generic_read_dir(struct file *, char *, size_t, loff_t *);
extern struct file_operations simple_dir_operations;
extern struct inode_operations simple_dir_inode_operations;
extern unsigned int real_root_dev;
extern int inode_change_ok(struct inode *, struct iattr *);
extern int inode_setattr(struct inode *, struct iattr *);
extern void inode_update_time(struct inode *inode, int ctime_too);
static inline ino_t parent_ino(struct dentry *dentry)
{
        ino_t res;
        do { do { } while (0); _raw_read_lock(&dparent_lock); } while(0);
        res = dentry->d_parent->d_inode->i_ino;
        do { asm volatile("lock ; incl %0" :"=m" ((&dparent_lock)->lock) : : "memory"); do { } while (0); } while(0);
        return res;
}
static __inline__ int scsi_blk_major(int m) {
        return (((m) == 8 || ((m) >= 65 && (m) <= 71)) || (m) == 11);
}
static __inline__ int ide_blk_major(int m)
{
        return ((m) == 3 || (m) == 22 || (m) == 33 || (m) == 34 || (m) == 56 || (m) == 57 || (m) == 88 || (m) == 89 || (m) == 90 || (m) == 91);
}
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
struct tty_driver {
        int magic;
        struct module *owner;
        const char *driver_name;
        const char *name;
        int name_base;
        short major;
        short minor_start;
        short num;
        short type;
        short subtype;
        struct termios init_termios;
        int flags;
        int *refcount;
        struct proc_dir_entry *proc_entry;
        struct tty_driver *other;
        struct tty_struct **table;
        struct termios **termios;
        struct termios **termios_locked;
        void *driver_state;
        int (*open)(struct tty_struct * tty, struct file * filp);
        void (*close)(struct tty_struct * tty, struct file * filp);
        int (*write)(struct tty_struct * tty, int from_user,
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
        struct list_head tty_drivers;
};
extern struct list_head tty_drivers;
extern struct device_class tty_devclass;
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
        void (*receive_buf)(struct tty_struct *, const unsigned char *cp,
                               char *fp, int count);
        int (*receive_room)(struct tty_struct *);
        void (*write_wakeup)(struct tty_struct *);
};
struct screen_info {
        unsigned char orig_x;
        unsigned char orig_y;
        unsigned short dontuse1;
        unsigned short orig_video_page;
        unsigned char orig_video_mode;
        unsigned char orig_video_cols;
        unsigned short unused2;
        unsigned short orig_video_ega_bx;
        unsigned short unused3;
        unsigned char orig_video_lines;
        unsigned char orig_video_isVGA;
        unsigned short orig_video_points;
        unsigned short lfb_width;
        unsigned short lfb_height;
        unsigned short lfb_depth;
        unsigned long lfb_base;
        unsigned long lfb_size;
        unsigned short dontuse2, dontuse3;
        unsigned short lfb_linelength;
        unsigned char red_size;
        unsigned char red_pos;
        unsigned char green_size;
        unsigned char green_pos;
        unsigned char blue_size;
        unsigned char blue_pos;
        unsigned char rsvd_size;
        unsigned char rsvd_pos;
        unsigned short vesapm_seg;
        unsigned short vesapm_off;
        unsigned short pages;
        unsigned short vesa_attributes;
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
struct tty_struct {
        int magic;
        struct tty_driver driver;
        struct tty_ldisc ldisc;
        struct termios *termios, *termios_locked;
        int pgrp;
        int session;
        kdev_t device;
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
        unsigned overrun_time;
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
        spinlock_t read_lock;
        struct work_struct SAK_work;
};
extern void tty_write_flush(struct tty_struct *);
extern struct termios tty_std_termios;
extern struct tty_struct * redirect;
extern struct tty_ldisc ldiscs[];
extern int fg_console, last_console, want_console;
extern int kmsg_redirect;
extern void con_init(void);
extern void console_init(void);
extern int lp_init(void);
extern int pty_init(void);
extern void tty_init(void);
extern int mxser_init(void);
extern int moxa_init(void);
extern int ip2_init(void);
extern int pcxe_init(void);
extern int pc_init(void);
extern int vcs_init(void);
extern int rp_init(void);
extern int cy_init(void);
extern int stl_init(void);
extern int stli_init(void);
extern int specialix_init(void);
extern int espserial_init(void);
extern int macserial_init(void);
extern int a2232board_init(void);
extern int tty_paranoia_check(struct tty_struct *tty, kdev_t device,
                              const char *routine);
extern char *tty_name(struct tty_struct *tty, char *buf);
extern void tty_wait_until_sent(struct tty_struct * tty, long timeout);
extern int tty_check_change(struct tty_struct * tty);
extern void stop_tty(struct tty_struct * tty);
extern void start_tty(struct tty_struct * tty);
extern int tty_register_ldisc(int disc, struct tty_ldisc *new_ldisc);
extern int tty_register_driver(struct tty_driver *driver);
extern int tty_unregister_driver(struct tty_driver *driver);
extern void tty_register_device(struct tty_driver *driver, unsigned minor);
extern void tty_unregister_device(struct tty_driver *driver, unsigned minor);
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
extern struct tty_ldisc tty_ldisc_N_TTY;
extern int n_tty_ioctl(struct tty_struct * tty, struct file * file,
                       unsigned int cmd, unsigned long arg);
extern void serial_console_init(void);
extern int pcxe_open(struct tty_struct *tty, struct file *filp);
extern void console_print(const char *);
extern int vt_ioctl(struct tty_struct *tty, struct file * file,
                    unsigned int cmd, unsigned long arg);
static __inline__ void tty_insert_flip_char(struct tty_struct *tty,
                                   unsigned char ch, char flag)
{
        if (tty->flip.count < 512) {
                tty->flip.count++;
                *tty->flip.flag_buf_ptr++ = flag;
                *tty->flip.char_buf_ptr++ = ch;
        }
}
static __inline__ void tty_schedule_flip(struct tty_struct *tty)
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
        int reserved[1];
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
extern int register_serial(struct serial_struct *req);
extern void unregister_serial(int line);
extern int early_serial_setup(struct serial_struct *req);
struct circ_buf {
        char *buf;
        int head;
        int tail;
};
struct serial_state {
        int magic;
        int baud_base;
        unsigned long port;
        int irq;
        int flags;
        int hub6;
        int type;
        int line;
        int revision;
        int xmit_fifo_size;
        int custom_divisor;
        int count;
        u8 *iomem_base;
        u16 iomem_reg_shift;
        unsigned short close_delay;
        unsigned short closing_wait;
        struct async_icount icount;
        struct termios normal_termios;
        struct termios callout_termios;
        int io_type;
        struct async_struct *info;
        struct pci_dev *dev;
};
struct async_struct {
        int magic;
        unsigned long port;
        int hub6;
        int flags;
        int xmit_fifo_size;
        struct serial_state *state;
        struct tty_struct *tty;
        int read_status_mask;
        int ignore_status_mask;
        int timeout;
        int quot;
        int x_char;
        int close_delay;
        unsigned short closing_wait;
        unsigned short closing_wait2;
        int IER;
        int MCR;
        int LCR;
        int ACR;
        unsigned long event;
        unsigned long last_active;
        int line;
        int blocked_open;
        long session;
        long pgrp;
        struct circ_buf xmit;
        spinlock_t xmit_lock;
        u8 *iomem_base;
        u16 iomem_reg_shift;
        int io_type;
        struct work_struct work;
        wait_queue_head_t open_wait;
        wait_queue_head_t close_wait;
        wait_queue_head_t delta_msr_wait;
        struct async_struct *next_port;
        struct async_struct *prev_port;
};
struct rs_multiport_struct {
        int port1;
        unsigned char mask1, match1;
        int port2;
        unsigned char mask2, match2;
        int port3;
        unsigned char mask3, match3;
        int port4;
        unsigned char mask4, match4;
        int port_monitor;
};
extern int ptrace_readdata(struct task_struct *tsk, unsigned long src, char *dst, int len);
extern int ptrace_writedata(struct task_struct *tsk, char * src, unsigned long dst, int len);
extern int ptrace_attach(struct task_struct *tsk);
extern int ptrace_detach(struct task_struct *, unsigned int);
extern void ptrace_disable(struct task_struct *);
extern int ptrace_check_attach(struct task_struct *task, int kill);
extern int ptrace_request(struct task_struct *child, long request, long addr, long data);
extern void ptrace_notify(int exit_code);
extern void __ptrace_link(struct task_struct *child,
                          struct task_struct *new_parent);
extern void __ptrace_unlink(struct task_struct *child);
static inline void ptrace_link(struct task_struct *child,
                               struct task_struct *new_parent)
{
        if (__builtin_expect((child->ptrace),0))
                __ptrace_link(child, new_parent);
}
static inline void ptrace_unlink(struct task_struct *child)
{
        if (__builtin_expect((child->ptrace),0))
                __ptrace_unlink(child);
}
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
extern int get_resource_list(struct resource *, char *buf, int size);
extern int request_resource(struct resource *root, struct resource *new);
extern int release_resource(struct resource *new);
extern int allocate_resource(struct resource *root, struct resource *new,
                             unsigned long size,
                             unsigned long min, unsigned long max,
                             unsigned long align,
                             void (*alignf)(void *, struct resource *,
                                            unsigned long, unsigned long),
                             void *alignf_data);
extern struct resource * __request_region(struct resource *, unsigned long start, unsigned long n, const char *name);
extern int __attribute__((deprecated)) __check_region(struct resource *, unsigned long, unsigned long);
extern void __release_region(struct resource *, unsigned long, unsigned long);
static inline int current_is_kswapd(void)
{
        return get_current()->flags & 0x00040000;
}
union swap_header {
        struct {
                char reserved[(1UL << 12) - 10];
                char magic[10];
        } magic;
        struct {
                char bootbits[1024];
                unsigned int version;
                unsigned int last_page;
                unsigned int nr_badpages;
                unsigned int padding[125];
                unsigned int badpages[1];
        } info;
};
typedef struct {
        unsigned long val;
} swp_entry_t;
struct sysinfo;
struct address_space;
struct zone;
struct writeback_control;
struct swap_extent {
        struct list_head list;
        unsigned long start_page;
        unsigned long nr_pages;
        sector_t start_block;
};
enum {
        SWP_USED = (1 << 0),
        SWP_WRITEOK = (1 << 1),
        SWP_ACTIVE = (SWP_USED | SWP_WRITEOK),
};
struct swap_info_struct {
        unsigned int flags;
        spinlock_t sdev_lock;
        struct file *swap_file;
        struct block_device *bdev;
        struct list_head extent_list;
        int nr_extents;
        struct swap_extent *curr_swap_extent;
        unsigned old_block_size;
        unsigned short * swap_map;
        unsigned int lowest_bit;
        unsigned int highest_bit;
        unsigned int cluster_next;
        unsigned int cluster_nr;
        int prio;
        int pages;
        unsigned long max;
        unsigned long inuse_pages;
        int next;
};
struct swap_list_t {
        int head;
        int next;
};
extern unsigned long totalram_pages;
extern unsigned long totalhigh_pages;
extern int nr_swap_pages;
extern unsigned int nr_free_pages(void);
extern unsigned int nr_free_pages_pgdat(pg_data_t *pgdat);
extern unsigned int nr_free_buffer_pages(void);
extern unsigned int nr_free_pagecache_pages(void);
extern void mark_page_accessed(struct page *) __attribute__((regparm(3)));
extern void lru_cache_add(struct page *) __attribute__((regparm(3)));
extern void lru_cache_add_active(struct page *) __attribute__((regparm(3)));
extern void activate_page(struct page *) __attribute__((regparm(3)));
extern void lru_add_drain(void);
extern int rotate_reclaimable_page(struct page *page);
extern void swap_setup(void);
extern int try_to_free_pages(struct zone *, unsigned int, unsigned int);
extern int shrink_all_memory(int);
extern int vm_swappiness;
extern void out_of_memory(void);
struct pte_chain;
int page_referenced(struct page *) __attribute__((regparm(3)));
struct pte_chain *page_add_rmap(struct page *, pte_t *, struct pte_chain *) __attribute__((regparm(3)));
void page_remove_rmap(struct page *, pte_t *) __attribute__((regparm(3)));
int try_to_unmap(struct page *) __attribute__((regparm(3)));
int page_over_rsslimit(struct page *) __attribute__((regparm(3)));
extern int shmem_unuse(swp_entry_t entry, struct page *page);
extern int swap_readpage(struct file *, struct page *);
extern int swap_writepage(struct page *page, struct writeback_control *wbc);
extern int rw_swap_page_sync(int, swp_entry_t, struct page *);
extern struct address_space swapper_space;
extern void show_swap_cache_info(void);
extern int add_to_swap_cache(struct page *, swp_entry_t);
extern int add_to_swap(struct page *);
extern void __delete_from_swap_cache(struct page *);
extern void delete_from_swap_cache(struct page *);
extern int move_to_swap_cache(struct page *, swp_entry_t);
extern int move_from_swap_cache(struct page *, unsigned long,
                struct address_space *);
extern void free_page_and_swap_cache(struct page *);
extern void free_pages_and_swap_cache(struct page **, int);
extern struct page * lookup_swap_cache(swp_entry_t);
extern struct page * read_swap_cache_async(swp_entry_t);
extern int total_swap_pages;
extern unsigned int nr_swapfiles;
extern struct swap_info_struct swap_info[];
extern void si_swapinfo(struct sysinfo *);
extern swp_entry_t get_swap_page(void);
extern int swap_duplicate(swp_entry_t);
extern int valid_swaphandles(swp_entry_t, unsigned long *);
extern void swap_free(swp_entry_t);
extern void free_swap_and_cache(swp_entry_t);
extern sector_t map_swap_page(struct swap_info_struct *, unsigned long);
extern struct swap_info_struct *get_swap_info_struct(unsigned);
extern int can_share_swap_page(struct page *);
extern int remove_exclusive_swap_page(struct page *);
extern struct swap_list_t swap_list;
extern spinlock_t swaplock;
extern unsigned long max_mapnr;
extern unsigned long num_physpages;
extern void * high_memory;
extern int page_cluster;
extern pgd_t swapper_pg_dir[1024];
extern void paging_init(void);
extern unsigned long empty_zero_page[1024];
static inline int pgd_none(pgd_t pgd) { return 0; }
static inline int pgd_bad(pgd_t pgd) { return 0; }
static inline int pgd_present(pgd_t pgd) { return 1; }
static inline void set_pte(pte_t *ptep, pte_t pte)
{
        ptep->pte_high = pte.pte_high;
        __asm__ __volatile__ ("": : :"memory");
        ptep->pte_low = pte.pte_low;
}
static inline void pgd_clear (pgd_t * pgd) { }
static inline pte_t ptep_get_and_clear(pte_t *ptep)
{
        pte_t res;
        res.pte_low = ((__typeof__(*(&ptep->pte_low)))__xchg((unsigned long)(0),(&ptep->pte_low),sizeof(*(&ptep->pte_low))));
        res.pte_high = ptep->pte_high;
        ptep->pte_high = 0;
        return res;
}
static inline int pte_same(pte_t a, pte_t b)
{
        return a.pte_low == b.pte_low && a.pte_high == b.pte_high;
}
static inline int pte_none(pte_t pte)
{
        return !pte.pte_low && !pte.pte_high;
}
static inline unsigned long pte_pfn(pte_t pte)
{
        return (pte.pte_low >> 12) |
                (pte.pte_high << (32 - 12));
}
static inline pte_t pfn_pte(unsigned long page_nr, pgprot_t pgprot)
{
        pte_t pte;
        pte.pte_high = page_nr >> (32 - 12);
        pte.pte_low = (page_nr << 12) | ((pgprot).pgprot);
        return pte;
}
static inline pmd_t pfn_pmd(unsigned long page_nr, pgprot_t pgprot)
{
        return ((pmd_t) { (((unsigned long long)page_nr << 12) | ((pgprot).pgprot)) } );
}
void pgtable_cache_init(void);
extern unsigned long __PAGE_KERNEL;
extern unsigned long pg0[1024];
static inline int pte_read(pte_t pte) { return (pte).pte_low & 0x004; }
static inline int pte_exec(pte_t pte) { return (pte).pte_low & 0x004; }
static inline int pte_dirty(pte_t pte) { return (pte).pte_low & 0x040; }
static inline int pte_young(pte_t pte) { return (pte).pte_low & 0x020; }
static inline int pte_write(pte_t pte) { return (pte).pte_low & 0x002; }
static inline pte_t pte_rdprotect(pte_t pte) { (pte).pte_low &= ~0x004; return pte; }
static inline pte_t pte_exprotect(pte_t pte) { (pte).pte_low &= ~0x004; return pte; }
static inline pte_t pte_mkclean(pte_t pte) { (pte).pte_low &= ~0x040; return pte; }
static inline pte_t pte_mkold(pte_t pte) { (pte).pte_low &= ~0x020; return pte; }
static inline pte_t pte_wrprotect(pte_t pte) { (pte).pte_low &= ~0x002; return pte; }
static inline pte_t pte_mkread(pte_t pte) { (pte).pte_low |= 0x004; return pte; }
static inline pte_t pte_mkexec(pte_t pte) { (pte).pte_low |= 0x004; return pte; }
static inline pte_t pte_mkdirty(pte_t pte) { (pte).pte_low |= 0x040; return pte; }
static inline pte_t pte_mkyoung(pte_t pte) { (pte).pte_low |= 0x020; return pte; }
static inline pte_t pte_mkwrite(pte_t pte) { (pte).pte_low |= 0x002; return pte; }
static inline int ptep_test_and_clear_dirty(pte_t *ptep) { return test_and_clear_bit(6, &ptep->pte_low); }
static inline int ptep_test_and_clear_young(pte_t *ptep) { return test_and_clear_bit(5, &ptep->pte_low); }
static inline void ptep_set_wrprotect(pte_t *ptep) { clear_bit(1, &ptep->pte_low); }
static inline void ptep_mkdirty(pte_t *ptep) { set_bit(6, &ptep->pte_low); }
static inline pte_t pte_modify(pte_t pte, pgprot_t newprot)
{
        pte.pte_low &= ((~((1UL << 12)-1)) | 0x020 | 0x040);
        pte.pte_low |= ((newprot).pgprot);
        return pte;
}
typedef pte_t *pte_addr_t;
struct vm_area_struct {
        struct mm_struct * vm_mm;
        unsigned long vm_start;
        unsigned long vm_end;
        struct vm_area_struct *vm_next;
        pgprot_t vm_page_prot;
        unsigned long vm_flags;
        struct rb_node vm_rb;
        struct list_head shared;
        struct vm_operations_struct * vm_ops;
        unsigned long vm_pgoff;
        struct file * vm_file;
        void * vm_private_data;
};
extern pgprot_t protection_map[16];
struct vm_operations_struct {
        void (*open)(struct vm_area_struct * area);
        void (*close)(struct vm_area_struct * area);
        struct page * (*nopage)(struct vm_area_struct * area, unsigned long address, int unused);
        int (*populate)(struct vm_area_struct * area, unsigned long address, unsigned long len, unsigned long prot, unsigned long pgoff, int nonblock);
};
struct pte_chain;
struct mmu_gather;
struct page {
        unsigned long flags;
        atomic_t count;
        struct list_head list;
        struct address_space *mapping;
        unsigned long index;
        struct list_head lru;
        union {
                struct pte_chain *chain;
                pte_addr_t direct;
        } pte;
        unsigned long private;
};
struct page_state {
        unsigned long nr_dirty;
        unsigned long nr_writeback;
        unsigned long nr_pagecache;
        unsigned long nr_page_table_pages;
        unsigned long nr_reverse_maps;
        unsigned long nr_mapped;
        unsigned long nr_slab;
        unsigned long pgpgin;
        unsigned long pgpgout;
        unsigned long pswpin;
        unsigned long pswpout;
        unsigned long pgalloc;
        unsigned long pgfree;
        unsigned long pgactivate;
        unsigned long pgdeactivate;
        unsigned long pgfault;
        unsigned long pgmajfault;
        unsigned long pgscan;
        unsigned long pgrefill;
        unsigned long pgsteal;
        unsigned long pginodesteal;
        unsigned long kswapd_steal;
        unsigned long kswapd_inodesteal;
        unsigned long pageoutrun;
        unsigned long allocstall;
        unsigned long pgrotated;
} __attribute__((__aligned__((1 << (5)))));
extern __typeof__(struct page_state) page_states__per_cpu;
extern void get_page_state(struct page_state *ret);
extern void get_full_page_state(struct page_state *ret);
extern struct address_space swapper_space;
struct page;
int test_clear_page_dirty(struct page *page);
static inline void clear_page_dirty(struct page *page)
{
        test_clear_page_dirty(page);
}
extern void __page_cache_release(struct page *) __attribute__((regparm(3)));
static inline void get_page(struct page *page)
{
        atomic_inc(&page->count);
}
static inline void put_page(struct page *page)
{
        if (!(__builtin_constant_p(11) ? constant_test_bit((11),(&(page)->flags)) : variable_test_bit((11),(&(page)->flags))) && ({ do { if (__builtin_expect(((((&(page)->count)->counter) == 0)!=0),0)) __asm__ __volatile__( "ud2\n" "\t.word %c0\n" "\t.long %c1\n" : : "i" (254), "i" ("include/linux/mm.h")); } while(0); atomic_dec_and_test(&(page)->count); }))
                __page_cache_release(page);
}
struct zone;
extern struct zone *zone_table[];
static inline struct zone *page_zone(struct page *page)
{
        return zone_table[page->flags >> (32 - 8)];
}
static inline void set_page_zone(struct page *page, unsigned long zone_num)
{
        page->flags &= ~(~0UL << (32 - 8));
        page->flags |= zone_num << (32 - 8);
}
static inline void * lowmem_page_address(struct page *page)
{
        return ((void *)((unsigned long)(( (page - page_zone(page)->zone_mem_map) + page_zone(page)->zone_start_pfn) << 12)+((unsigned long)(0xC0000000))));
}
void *page_address(struct page *page);
void set_page_address(struct page *page, void *virtual);
void page_address_init(void);
static inline int page_mapped(struct page *page)
{
        return page->pte.direct != 0;
}
extern struct page *mem_map;
extern void show_free_areas(void);
struct page *shmem_nopage(struct vm_area_struct * vma,
                        unsigned long address, int unused);
struct file *shmem_file_setup(char * name, loff_t size, unsigned long flags);
void shmem_lock(struct file * file, int lock);
int shmem_zero_setup(struct vm_area_struct *);
void zap_page_range(struct vm_area_struct *vma, unsigned long address,
                        unsigned long size);
int unmap_vmas(struct mmu_gather **tlbp, struct mm_struct *mm,
                struct vm_area_struct *start_vma, unsigned long start_addr,
                unsigned long end_addr, unsigned long *nr_accounted);
void unmap_page_range(struct mmu_gather *tlb, struct vm_area_struct *vma,
                        unsigned long address, unsigned long size);
void clear_page_tables(struct mmu_gather *tlb, unsigned long first, int nr);
int copy_page_range(struct mm_struct *dst, struct mm_struct *src,
                        struct vm_area_struct *vma);
int remap_page_range(struct vm_area_struct *vma, unsigned long from,
                        unsigned long to, unsigned long size, pgprot_t prot);
int zeromap_page_range(struct vm_area_struct *vma, unsigned long from,
                        unsigned long size, pgprot_t prot);
extern int vmtruncate(struct inode * inode, loff_t offset);
extern pmd_t *__pmd_alloc(struct mm_struct *mm, pgd_t *pgd, unsigned long address) __attribute__((regparm(3)));
extern pte_t *pte_alloc_kernel(struct mm_struct *mm, pmd_t *pmd, unsigned long address) __attribute__((regparm(3)));
extern pte_t *pte_alloc_map(struct mm_struct *mm, pmd_t *pmd, unsigned long address) __attribute__((regparm(3)));
extern int install_page(struct mm_struct *mm, struct vm_area_struct *vma, unsigned long addr, struct page *page, unsigned long prot);
extern int handle_mm_fault(struct mm_struct *mm,struct vm_area_struct *vma, unsigned long address, int write_access);
extern int make_pages_present(unsigned long addr, unsigned long end);
extern int access_process_vm(struct task_struct *tsk, unsigned long addr, void *buf, int len, int write);
extern int sys_remap_file_pages(unsigned long start, unsigned long size, unsigned long prot, unsigned long pgoff, unsigned long nonblock);
extern struct page * follow_page(struct mm_struct *mm, unsigned long address, int write);
int get_user_pages(struct task_struct *tsk, struct mm_struct *mm, unsigned long start,
                int len, int write, int force, struct page **pages, struct vm_area_struct **vmas);
int __set_page_dirty_buffers(struct page *page);
int __set_page_dirty_nobuffers(struct page *page);
int set_page_dirty_lock(struct page *page);
typedef int (*shrinker_t)(int nr_to_scan, unsigned int gfp_mask);
struct shrinker;
extern struct shrinker *set_shrinker(int, shrinker_t);
extern void remove_shrinker(struct shrinker *shrinker);
static inline int set_page_dirty(struct page *page)
{
        if (page->mapping) {
                int (*spd)(struct page *);
                spd = page->mapping->a_ops->set_page_dirty;
                if (spd)
                        return (*spd)(page);
        }
        return __set_page_dirty_buffers(page);
}
static inline pmd_t *pmd_alloc(struct mm_struct *mm, pgd_t *pgd, unsigned long address)
{
        if (pgd_none(*pgd))
                return __pmd_alloc(mm, pgd, address);
        return ((pmd_t *) ((unsigned long) ((void *)((unsigned long)(((*(pgd)).pgd) & (~((1UL << 12)-1)))+((unsigned long)(0xC0000000))))) + (((address) >> 21) & (512 -1)));
}
extern void free_area_init(unsigned long * zones_size);
extern void free_area_init_node(int nid, pg_data_t *pgdat, struct page *pmap,
        unsigned long * zones_size, unsigned long zone_start_pfn,
        unsigned long *zholes_size);
extern void mem_init(void);
extern void show_mem(void);
extern void si_meminfo(struct sysinfo * val);
extern void swapin_readahead(swp_entry_t);
extern void insert_vm_struct(struct mm_struct *, struct vm_area_struct *);
extern void build_mmap_rb(struct mm_struct *);
extern void exit_mmap(struct mm_struct *);
extern unsigned long get_unmapped_area(struct file *, unsigned long, unsigned long, unsigned long, unsigned long);
extern unsigned long do_mmap_pgoff(struct file *file, unsigned long addr,
        unsigned long len, unsigned long prot,
        unsigned long flag, unsigned long pgoff);
static inline unsigned long do_mmap(struct file *file, unsigned long addr,
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
static inline void
__vma_unlink(struct mm_struct *mm, struct vm_area_struct *vma,
                struct vm_area_struct *prev)
{
        prev->vm_next = vma->vm_next;
        rb_erase(&vma->vm_rb, &mm->mm_rb);
        if (mm->mmap_cache == vma)
                mm->mmap_cache = prev;
}
static inline int
can_vma_merge(struct vm_area_struct *vma, unsigned long vm_flags)
{
        if (!vma->vm_file && vma->vm_flags == vm_flags)
                return 1;
        else
                return 0;
}
extern unsigned long page_unuse(struct page *);
extern void truncate_inode_pages(struct address_space *, loff_t);
extern struct page *filemap_nopage(struct vm_area_struct *, unsigned long, int);
int write_one_page(struct page *page, int wait);
int do_page_cache_readahead(struct address_space *mapping, struct file *filp,
                        unsigned long offset, unsigned long nr_to_read);
void page_cache_readahead(struct address_space *mapping,
                          struct file_ra_state *ra,
                          struct file *filp,
                          unsigned long offset);
void page_cache_readaround(struct address_space *mapping,
                           struct file_ra_state *ra,
                           struct file *filp,
                           unsigned long offset);
void handle_ra_miss(struct address_space *mapping,
                    struct file_ra_state *ra);
unsigned long max_sane_readahead(unsigned long nr);
extern int expand_stack(struct vm_area_struct * vma, unsigned long address);
extern struct vm_area_struct * find_vma(struct mm_struct * mm, unsigned long addr);
extern struct vm_area_struct * find_vma_prev(struct mm_struct * mm, unsigned long addr,
                                             struct vm_area_struct **pprev);
extern int split_vma(struct mm_struct * mm, struct vm_area_struct * vma,
                     unsigned long addr, int new_below);
static inline struct vm_area_struct * find_vma_intersection(struct mm_struct * mm, unsigned long start_addr, unsigned long end_addr)
{
        struct vm_area_struct * vma = find_vma(mm,start_addr);
        if (vma && end_addr <= vma->vm_start)
                vma = ((void *)0);
        return vma;
}
extern struct vm_area_struct *find_extend_vma(struct mm_struct *mm, unsigned long addr);
extern struct page * vmalloc_to_page(void *addr);
extern unsigned long get_page_cache_size(void);
extern unsigned int nr_used_zone_pages(void);
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
extern void *vmalloc_32(unsigned long size);
extern void *__vmalloc(unsigned long size, int gfp_mask, pgprot_t prot);
extern void vfree(void *addr);
extern void *vmap(struct page **pages, unsigned int count);
extern void vunmap(void *addr);
extern struct vm_struct *get_vm_area(unsigned long size, unsigned long flags);
extern struct vm_struct *remove_vm_area(void *addr);
extern int map_vm_area(struct vm_struct *area, pgprot_t prot,
                        struct page ***pages);
extern void unmap_vm_area(struct vm_struct *area);
extern rwlock_t vmlist_lock;
extern struct vm_struct *vmlist;
static inline unsigned long virt_to_phys(volatile void * address)
{
        return ((unsigned long)(address)-((unsigned long)(0xC0000000)));
}
static inline void * phys_to_virt(unsigned long address)
{
        return ((void *)((unsigned long)(address)+((unsigned long)(0xC0000000))));
}
extern void * __ioremap(unsigned long offset, unsigned long size, unsigned long flags);
static inline void * ioremap (unsigned long offset, unsigned long size)
{
        return __ioremap(offset, size, 0);
}
extern void * ioremap_nocache (unsigned long offset, unsigned long size);
extern void iounmap(void *addr);
extern void *bt_ioremap(unsigned long offset, unsigned long size);
extern void bt_iounmap(void *addr, unsigned long size);
static inline int check_signature(unsigned long io_addr,
        const unsigned char *signature, int length)
{
        int retval = 0;
        do {
                if ((*(volatile unsigned char *) ((void *)(io_addr))) != *signature)
                        goto out;
                io_addr++;
                signature++;
                length--;
        } while (length);
        retval = 1;
out:
        return retval;
}
static inline int isa_check_signature(unsigned long io_addr,
        const unsigned char *signature, int length)
{
        int retval = 0;
        do {
                if ((*(volatile unsigned char *) ((void *)(((char *)(((unsigned long)(0xC0000000)))) + (io_addr)))) != *signature)
                        goto out;
                io_addr++;
                signature++;
                length--;
        } while (length);
        retval = 1;
out:
        return retval;
}
static inline void flush_write_buffers(void)
{
        __asm__ __volatile__ ("lock; addl $0,0(%%esp)": : :"memory");
}
static inline void slow_down_io(void) {
        __asm__ __volatile__(
                "outb %%al,$0x80;"
                : : );
}
static inline void outb_local(unsigned char value, int port) { __asm__ __volatile__("out" "b" " %" "b" "0, %w1" : : "a"(value), "Nd"(port)); } static inline unsigned char inb_local(int port) { unsigned char value; __asm__ __volatile__("in" "b" " %w1, %" "b" "0" : "=a"(value) : "Nd"(port)); return value; } static inline void outb_local_p(unsigned char value, int port) { outb_local(value, port); slow_down_io(); } static inline unsigned char inb_local_p(int port) { unsigned char value = inb_local(port); slow_down_io(); return value; } static inline void outb(unsigned char value, int port) { outb_local(value, port); } static inline unsigned char inb(int port) { return inb_local(port); } static inline void outb_p(unsigned char value, int port) { outb(value, port); slow_down_io(); } static inline unsigned char inb_p(int port) { unsigned char value = inb(port); slow_down_io(); return value; } static inline void outsb(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "b" : "+S"(addr), "+c"(count) : "d"(port)); } static inline void insb(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "b" : "+D"(addr), "+c"(count) : "d"(port)); }
static inline void outw_local(unsigned short value, int port) { __asm__ __volatile__("out" "w" " %" "w" "0, %w1" : : "a"(value), "Nd"(port)); } static inline unsigned short inw_local(int port) { unsigned short value; __asm__ __volatile__("in" "w" " %w1, %" "w" "0" : "=a"(value) : "Nd"(port)); return value; } static inline void outw_local_p(unsigned short value, int port) { outw_local(value, port); slow_down_io(); } static inline unsigned short inw_local_p(int port) { unsigned short value = inw_local(port); slow_down_io(); return value; } static inline void outw(unsigned short value, int port) { outw_local(value, port); } static inline unsigned short inw(int port) { return inw_local(port); } static inline void outw_p(unsigned short value, int port) { outw(value, port); slow_down_io(); } static inline unsigned short inw_p(int port) { unsigned short value = inw(port); slow_down_io(); return value; } static inline void outsw(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "w" : "+S"(addr), "+c"(count) : "d"(port)); } static inline void insw(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "w" : "+D"(addr), "+c"(count) : "d"(port)); }
static inline void outl_local(unsigned int value, int port) { __asm__ __volatile__("out" "l" " %" "" "0, %w1" : : "a"(value), "Nd"(port)); } static inline unsigned int inl_local(int port) { unsigned int value; __asm__ __volatile__("in" "l" " %w1, %" "" "0" : "=a"(value) : "Nd"(port)); return value; } static inline void outl_local_p(unsigned int value, int port) { outl_local(value, port); slow_down_io(); } static inline unsigned int inl_local_p(int port) { unsigned int value = inl_local(port); slow_down_io(); return value; } static inline void outl(unsigned int value, int port) { outl_local(value, port); } static inline unsigned int inl(int port) { return inl_local(port); } static inline void outl_p(unsigned int value, int port) { outl(value, port); slow_down_io(); } static inline unsigned int inl_p(int port) { unsigned int value = inl(port); slow_down_io(); return value; } static inline void outsl(int port, const void *addr, unsigned long count) { __asm__ __volatile__("rep; outs" "l" : "+S"(addr), "+c"(count) : "d"(port)); } static inline void insl(int port, void *addr, unsigned long count) { __asm__ __volatile__("rep; ins" "l" : "+D"(addr), "+c"(count) : "d"(port)); }
extern unsigned long loops_per_jiffy;
extern void __bad_udelay(void);
extern void __udelay(unsigned long usecs);
extern void __const_udelay(unsigned long usecs);
extern void __delay(unsigned long loops);
extern spinlock_t dma_spin_lock;
static __inline__ unsigned long claim_dma_lock(void)
{
        unsigned long flags;
        do { __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (flags): :"memory"); do { } while (0); _raw_spin_lock(&dma_spin_lock); } while (0);
        return flags;
}
static __inline__ void release_dma_lock(unsigned long flags)
{
        do { _raw_spin_unlock(&dma_spin_lock); __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc"); do { } while (0); } while (0);
}
static __inline__ void enable_dma(unsigned int dmanr)
{
        if (dmanr<=3)
                outb(dmanr, 0x0A);
        else
                outb(dmanr & 3, 0xD4);
}
static __inline__ void disable_dma(unsigned int dmanr)
{
        if (dmanr<=3)
                outb(dmanr | 4, 0x0A);
        else
                outb((dmanr & 3) | 4, 0xD4);
}
static __inline__ void clear_dma_ff(unsigned int dmanr)
{
        if (dmanr<=3)
                outb(0, 0x0C);
        else
                outb(0, 0xD8);
}
static __inline__ void set_dma_mode(unsigned int dmanr, char mode)
{
        if (dmanr<=3)
                outb(mode | dmanr, 0x0B);
        else
                outb(mode | (dmanr&3), 0xD6);
}
static __inline__ void set_dma_page(unsigned int dmanr, char pagenr)
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
static __inline__ void set_dma_addr(unsigned int dmanr, unsigned int a)
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
static __inline__ void set_dma_count(unsigned int dmanr, unsigned int count)
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
static __inline__ int get_dma_residue(unsigned int dmanr)
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
struct hayes_esp_config {
        short flow_on;
        short flow_off;
        short rx_trigger;
        short tx_trigger;
        short pio_threshold;
        unsigned char rx_timeout;
        char dma_channel;
};
struct esp_struct {
        int magic;
        int port;
        int irq;
        int flags;
        struct tty_struct *tty;
        int read_status_mask;
        int ignore_status_mask;
        int timeout;
        int stat_flags;
        int custom_divisor;
        int close_delay;
        unsigned short closing_wait;
        unsigned short closing_wait2;
        int IER;
        int MCR;
        unsigned long event;
        unsigned long last_active;
        int line;
        int count;
        int blocked_open;
        long session;
        long pgrp;
        unsigned char *xmit_buf;
        int xmit_head;
        int xmit_tail;
        int xmit_cnt;
        struct work_struct tqueue;
        struct work_struct tqueue_hangup;
        struct termios normal_termios;
        struct termios callout_termios;
        wait_queue_head_t open_wait;
        wait_queue_head_t close_wait;
        wait_queue_head_t delta_msr_wait;
        wait_queue_head_t break_wait;
        struct async_icount icount;
        struct hayes_esp_config config;
        struct esp_struct *next_port;
};
struct esp_pio_buffer {
        unsigned char data[1024];
        struct esp_pio_buffer *next;
};
struct tq_struct {
        struct list_head list;
        unsigned long sync;
        void (*routine)(void *);
        void *data;
};
typedef struct list_head task_queue;
extern task_queue tq_timer, tq_immediate, tq_disk;
extern spinlock_t tqueue_lock;
static inline int queue_task(struct tq_struct *bh_pointer, task_queue *bh_list)
{
        int ret = 0;
        if (!test_and_set_bit(0,&bh_pointer->sync)) {
                unsigned long flags;
                do { __asm__ __volatile__("pushfl ; popl %0 ; cli":"=g" (flags): :"memory"); do { } while (0); _raw_spin_lock(&tqueue_lock); } while (0);
                list_add_tail(&bh_pointer->list, bh_list);
                do { _raw_spin_unlock(&tqueue_lock); __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc"); do { } while (0); } while (0);
                ret = 1;
        }
        return ret;
}
extern void __run_task_queue(task_queue *list);
static inline void run_task_queue(task_queue *list)
{
        if ((!list_empty(&*list)))
                __run_task_queue(list);
}
static spinlock_t cli_lock;
static int irq[8];
static unsigned int divisor[8];
static unsigned int dma = 0;
static unsigned int rx_trigger = 768;
static unsigned int tx_trigger = 768;
static unsigned int flow_off = 1016;
static unsigned int flow_on = 944;
static unsigned int rx_timeout = 128;
static unsigned int pio_threshold = 32;
static const char __module_license[] __attribute__((section(".init.license"), unused)) = "GPL";
struct obsolete_modparm __parm_irq __attribute__((section("__obsparm"))) = { "irq", "1-8i" };;
struct obsolete_modparm __parm_divisor __attribute__((section("__obsparm"))) = { "divisor", "1-8i" };;
struct obsolete_modparm __parm_dma __attribute__((section("__obsparm"))) = { "dma", "i" };;
struct obsolete_modparm __parm_rx_trigger __attribute__((section("__obsparm"))) = { "rx_trigger", "i" };;
struct obsolete_modparm __parm_tx_trigger __attribute__((section("__obsparm"))) = { "tx_trigger", "i" };;
struct obsolete_modparm __parm_flow_off __attribute__((section("__obsparm"))) = { "flow_off", "i" };;
struct obsolete_modparm __parm_flow_on __attribute__((section("__obsparm"))) = { "flow_on", "i" };;
struct obsolete_modparm __parm_rx_timeout __attribute__((section("__obsparm"))) = { "rx_timeout", "i" };;
struct obsolete_modparm __parm_pio_threshold __attribute__((section("__obsparm"))) = { "pio_threshold", "i" };;
static char *dma_buffer;
static int dma_bytes;
static struct esp_pio_buffer *free_pio_buf;
static char serial_name[] __attribute__ ((__section__ (".init.data"))) = "ESP serial driver";
static char serial_version[] __attribute__ ((__section__ (".init.data"))) = "2.2";
static struct list_head tq_esp = { &(tq_esp), &(tq_esp) };
static struct tty_driver esp_driver, esp_callout_driver;
static int serial_refcount;
static struct esp_struct *ports;
static void change_speed(struct esp_struct *info);
static void rs_wait_until_sent(struct tty_struct *, int);
static struct tty_struct *serial_table[64];
static struct termios *serial_termios[64];
static struct termios *serial_termios_locked[64];
static unsigned char *tmp_buf;
static struct semaphore tmp_buf_sem = { { (1) }, 0, { .lock = (spinlock_t) { 1 }, .task_list = { &((tmp_buf_sem).wait).task_list, &((tmp_buf_sem).wait).task_list } } };
static inline int serial_paranoia_check(struct esp_struct *info,
                                        kdev_t device, const char *routine)
{
        return 0;
}
static inline unsigned int serial_in(struct esp_struct *info, int offset)
{
        return inb(info->port + offset);
}
static inline void serial_out(struct esp_struct *info, int offset,
                              unsigned char value)
{
        outb(value, info->port+offset);
}
static void rs_stop(struct tty_struct *tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        unsigned long flags;
        if (serial_paranoia_check(info, tty->device, "rs_stop"))
                return;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (info->IER & 0x02) {
                info->IER &= ~0x02;
                serial_out(info, 0x04, 0x06);
                serial_out(info, 0x05, info->IER);
        }
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static void rs_start(struct tty_struct *tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        unsigned long flags;
        if (serial_paranoia_check(info, tty->device, "rs_start"))
                return;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (info->xmit_cnt && info->xmit_buf && !(info->IER & 0x02)) {
                info->IER |= 0x02;
                serial_out(info, 0x04, 0x06);
                serial_out(info, 0x05, info->IER);
        }
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static inline void rs_sched_event(struct esp_struct *info,
                                  int event)
{
        info->event |= 1 << event;
        mark_bh(1);
}
static inline struct esp_pio_buffer *get_pio_buffer(void)
{
        struct esp_pio_buffer *buf;
        if (free_pio_buf) {
                buf = free_pio_buf;
                free_pio_buf = buf->next;
        } else {
                buf = kmalloc(sizeof(struct esp_pio_buffer), (0x20));
        }
        return buf;
}
static inline void release_pio_buffer(struct esp_pio_buffer *buf)
{
        buf->next = free_pio_buf;
        free_pio_buf = buf;
}
static inline void receive_chars_pio(struct esp_struct *info, int num_bytes)
{
        struct tty_struct *tty = info->tty;
        int i;
        struct esp_pio_buffer *pio_buf;
        struct esp_pio_buffer *err_buf;
        unsigned char status_mask;
        pio_buf = get_pio_buffer();
        if (!pio_buf)
                return;
        err_buf = get_pio_buffer();
        if (!err_buf) {
                release_pio_buffer(pio_buf);
                return;
        }
        _spin_unlock(&cli_lock);;
        status_mask = (info->read_status_mask >> 2) & 0x07;
        for (i = 0; i < num_bytes - 1; i += 2) {
                *((unsigned short *)(pio_buf->data + i)) =
                        inw(info->port + 0x02);
                err_buf->data[i] = serial_in(info, 0x07);
                err_buf->data[i + 1] = (err_buf->data[i] >> 3) & status_mask;
                err_buf->data[i] &= status_mask;
        }
        if (num_bytes & 0x0001) {
                pio_buf->data[num_bytes - 1] = serial_in(info, 0x02);
                err_buf->data[num_bytes - 1] =
                        (serial_in(info, 0x07) >> 3) & status_mask;
        }
        _spin_lock(&cli_lock);;
        tty = info->tty;
        if (!tty) {
                release_pio_buffer(pio_buf);
                release_pio_buffer(err_buf);
                info->stat_flags &= ~0x01;
                return;
        }
        status_mask = (info->ignore_status_mask >> 2) & 0x07;
        for (i = 0; i < num_bytes; i++) {
                if (!(err_buf->data[i] & status_mask)) {
                        *(tty->flip.char_buf_ptr++) = pio_buf->data[i];
                        if (err_buf->data[i] & 0x04) {
                                *(tty->flip.flag_buf_ptr++) = 1;
                                if (info->flags & 0x0004)
                                        do_SAK(tty);
                        }
                        else if (err_buf->data[i] & 0x02)
                                *(tty->flip.flag_buf_ptr++) = 2;
                        else if (err_buf->data[i] & 0x01)
                                *(tty->flip.flag_buf_ptr++) = 3;
                        else
                                *(tty->flip.flag_buf_ptr++) = 0;
                        tty->flip.count++;
                }
        }
        info->stat_flags &= ~0x01;
        release_pio_buffer(pio_buf);
        release_pio_buffer(err_buf);
}
static inline void receive_chars_dma(struct esp_struct *info, int num_bytes)
{
        unsigned long flags;
        info->stat_flags &= ~0x01;
        dma_bytes = num_bytes;
        info->stat_flags |= 0x02;
        flags=claim_dma_lock();
        disable_dma(dma);
        clear_dma_ff(dma);
        set_dma_mode(dma, 0x44);
        set_dma_addr(dma, virt_to_phys(dma_buffer));
        set_dma_count(dma, dma_bytes);
        enable_dma(dma);
        release_dma_lock(flags);
        serial_out(info, 0x04, 0x16);
}
static inline void receive_chars_dma_done(struct esp_struct *info,
                                            int status)
{
        struct tty_struct *tty = info->tty;
        int num_bytes;
        unsigned long flags;
        flags=claim_dma_lock();
        disable_dma(dma);
        clear_dma_ff(dma);
        info->stat_flags &= ~0x02;
        num_bytes = dma_bytes - get_dma_residue(dma);
        release_dma_lock(flags);
        info->icount.rx += num_bytes;
        (__builtin_constant_p(num_bytes) ? __constant_memcpy((tty->flip.char_buf_ptr),(dma_buffer),(num_bytes)) : __memcpy((tty->flip.char_buf_ptr),(dma_buffer),(num_bytes)));
        tty->flip.char_buf_ptr += num_bytes;
        tty->flip.count += num_bytes;
        (__builtin_constant_p(0) ? (__builtin_constant_p((num_bytes)) ? __constant_c_and_count_memset(((tty->flip.flag_buf_ptr)),((0x01010101UL*(unsigned char)(0))),((num_bytes))) : __constant_c_memset(((tty->flip.flag_buf_ptr)),((0x01010101UL*(unsigned char)(0))),((num_bytes)))) : (__builtin_constant_p((num_bytes)) ? __memset_generic((((tty->flip.flag_buf_ptr))),(((0))),(((num_bytes)))) : __memset_generic(((tty->flip.flag_buf_ptr)),((0)),((num_bytes)))));
        tty->flip.flag_buf_ptr += num_bytes;
        if (num_bytes > 0) {
                tty->flip.flag_buf_ptr--;
                status &= (0x1c & info->read_status_mask);
                if (status & info->ignore_status_mask) {
                        tty->flip.count--;
                        tty->flip.char_buf_ptr--;
                        tty->flip.flag_buf_ptr--;
                } else if (status & 0x10) {
                        *tty->flip.flag_buf_ptr = 1;
                        (info->icount.brk)++;
                        if (info->flags & 0x0004)
                                do_SAK(tty);
                } else if (status & 0x08) {
                        *tty->flip.flag_buf_ptr = 2;
                        (info->icount.frame)++;
                }
                else if (status & 0x04) {
                        *tty->flip.flag_buf_ptr = 3;
                        (info->icount.parity)++;
                }
                tty->flip.flag_buf_ptr++;
        }
        if (dma_bytes != num_bytes) {
                num_bytes = dma_bytes - num_bytes;
                dma_bytes = 0;
                receive_chars_dma(info, num_bytes);
        } else
                dma_bytes = 0;
}
static inline void transmit_chars_pio(struct esp_struct *info,
                                        int space_avail)
{
        int i;
        struct esp_pio_buffer *pio_buf;
        pio_buf = get_pio_buffer();
        if (!pio_buf)
                return;
        while (space_avail && info->xmit_cnt) {
                if (info->xmit_tail + space_avail <= 4096) {
                        (__builtin_constant_p(space_avail) ? __constant_memcpy((pio_buf->data),(&(info->xmit_buf[info->xmit_tail])),(space_avail)) : __memcpy((pio_buf->data),(&(info->xmit_buf[info->xmit_tail])),(space_avail)));
                } else {
                        i = 4096 - info->xmit_tail;
                        (__builtin_constant_p(i) ? __constant_memcpy((pio_buf->data),(&(info->xmit_buf[info->xmit_tail])),(i)) : __memcpy((pio_buf->data),(&(info->xmit_buf[info->xmit_tail])),(i)));
                        (__builtin_constant_p(space_avail - i) ? __constant_memcpy((&(pio_buf->data[i])),(info->xmit_buf),(space_avail - i)) : __memcpy((&(pio_buf->data[i])),(info->xmit_buf),(space_avail - i)));
                }
                info->xmit_cnt -= space_avail;
                info->xmit_tail = (info->xmit_tail + space_avail) &
                        (4096 - 1);
                _spin_unlock(&cli_lock);;
                for (i = 0; i < space_avail - 1; i += 2) {
                        outw(*((unsigned short *)(pio_buf->data + i)),
                             info->port + 0x02);
                }
                if (space_avail & 0x0001)
                        serial_out(info, 0x02,
                                   pio_buf->data[space_avail - 1]);
                _spin_lock(&cli_lock);;
                if (info->xmit_cnt) {
                        serial_out(info, 0x04, 0xff);
                        serial_out(info, 0x04, 0x15);
                        space_avail = serial_in(info, 0x04) << 8;
                        space_avail |= serial_in(info, 0x05);
                        if (space_avail > info->xmit_cnt)
                                space_avail = info->xmit_cnt;
                }
        }
        if (info->xmit_cnt < 1024) {
                rs_sched_event(info, 0);
                if (info->xmit_cnt <= 0) {
                        info->IER &= ~0x02;
                        serial_out(info, 0x04,
                                   0x06);
                        serial_out(info, 0x05, info->IER);
                }
        }
        release_pio_buffer(pio_buf);
}
static inline void transmit_chars_dma(struct esp_struct *info, int num_bytes)
{
        unsigned long flags;
        dma_bytes = num_bytes;
        if (info->xmit_tail + dma_bytes <= 4096) {
                (__builtin_constant_p(dma_bytes) ? __constant_memcpy((dma_buffer),(&(info->xmit_buf[info->xmit_tail])),(dma_bytes)) : __memcpy((dma_buffer),(&(info->xmit_buf[info->xmit_tail])),(dma_bytes)));
        } else {
                int i = 4096 - info->xmit_tail;
                (__builtin_constant_p(i) ? __constant_memcpy((dma_buffer),(&(info->xmit_buf[info->xmit_tail])),(i)) : __memcpy((dma_buffer),(&(info->xmit_buf[info->xmit_tail])),(i)));
                (__builtin_constant_p(dma_bytes - i) ? __constant_memcpy((&(dma_buffer[i])),(info->xmit_buf),(dma_bytes - i)) : __memcpy((&(dma_buffer[i])),(info->xmit_buf),(dma_bytes - i)));
        }
        info->xmit_cnt -= dma_bytes;
        info->xmit_tail = (info->xmit_tail + dma_bytes) & (4096 - 1);
        if (info->xmit_cnt < 1024) {
                rs_sched_event(info, 0);
                if (info->xmit_cnt <= 0) {
                        info->IER &= ~0x02;
                        serial_out(info, 0x04, 0x06);
                        serial_out(info, 0x05, info->IER);
                }
        }
        info->stat_flags |= 0x04;
        flags=claim_dma_lock();
        disable_dma(dma);
        clear_dma_ff(dma);
        set_dma_mode(dma, 0x48);
        set_dma_addr(dma, virt_to_phys(dma_buffer));
        set_dma_count(dma, dma_bytes);
        enable_dma(dma);
        release_dma_lock(flags);
        serial_out(info, 0x04, 0x17);
}
static inline void transmit_chars_dma_done(struct esp_struct *info)
{
        int num_bytes;
        unsigned long flags;
        flags=claim_dma_lock();
        disable_dma(dma);
        clear_dma_ff(dma);
        num_bytes = dma_bytes - get_dma_residue(dma);
        info->icount.tx += dma_bytes;
        release_dma_lock(flags);
        if (dma_bytes != num_bytes) {
                dma_bytes -= num_bytes;
                memmove(dma_buffer, dma_buffer + num_bytes, dma_bytes);
                flags=claim_dma_lock();
                disable_dma(dma);
                clear_dma_ff(dma);
                set_dma_mode(dma, 0x48);
                set_dma_addr(dma, virt_to_phys(dma_buffer));
                set_dma_count(dma, dma_bytes);
                enable_dma(dma);
                release_dma_lock(flags);
                serial_out(info, 0x04, 0x17);
        } else {
                dma_bytes = 0;
                info->stat_flags &= ~0x04;
        }
}
static inline void check_modem_status(struct esp_struct *info)
{
        int status;
        serial_out(info, 0x04, 0x13);
        status = serial_in(info, 0x05);
        if (status & 0x0F) {
                if (status & 0x04)
                        info->icount.rng++;
                if (status & 0x02)
                        info->icount.dsr++;
                if (status & 0x08)
                        info->icount.dcd++;
                if (status & 0x01)
                        info->icount.cts++;
                __wake_up((&info->delta_msr_wait),1, 1);
        }
        if ((info->flags & 0x02000000) && (status & 0x08)) {
                if (status & 0x80)
                        __wake_up((&info->open_wait),1, 1);
                else if (!((info->flags & 0x40000000) &&
                           (info->flags & 0x0400))) {
                        _MOD_INC_USE_COUNT((&__this_module));
                        if (schedule_task(&info->tqueue_hangup) == 0)
                                __MOD_DEC_USE_COUNT((&__this_module));
                }
        }
}
static void rs_interrupt_single(int irq, void *dev_id, struct pt_regs * regs)
{
        struct esp_struct * info;
        unsigned err_status;
        unsigned int scratch;
        info = (struct esp_struct *)dev_id;
        err_status = 0;
        scratch = serial_in(info, 0x01);
        _spin_lock(&cli_lock);;
        if (!info->tty) {
                _spin_unlock(&cli_lock);;
                return;
        }
        if (scratch & 0x04) {
                serial_out(info, 0x04, 0x12);
                err_status = serial_in(info, 0x04);
                serial_in(info, 0x05);
                if (err_status & 0x01)
                        info->stat_flags |= 0x01;
                if (err_status & 0x20)
                        check_modem_status(info);
                if (err_status & 0x80)
                        __wake_up((&info->break_wait),1, 1);
        }
        if ((scratch & 0x88) ||
            (err_status & 0x1c) ) {
                if (info->stat_flags & 0x02)
                        receive_chars_dma_done(info, err_status);
                else if (info->stat_flags & 0x04)
                        transmit_chars_dma_done(info);
        }
        if (!(info->stat_flags & (0x02 | 0x04)) &&
            ((scratch & 0x01) || (info->stat_flags & 0x01)) &&
            (info->IER & 0x01)) {
                int num_bytes;
                serial_out(info, 0x04, 0xff);
                serial_out(info, 0x04, 0x14);
                num_bytes = serial_in(info, 0x04) << 8;
                num_bytes |= serial_in(info, 0x05);
                if (num_bytes > (512 - info->tty->flip.count))
                  num_bytes = 512 - info->tty->flip.count;
                if (num_bytes) {
                        if (dma_bytes ||
                            (info->stat_flags & 0x10) ||
                            (num_bytes <= info->config.pio_threshold))
                                receive_chars_pio(info, num_bytes);
                        else
                                receive_chars_dma(info, num_bytes);
                }
        }
        if (!(info->stat_flags & (0x02 | 0x04)) &&
            (scratch & 0x02) && (info->IER & 0x02)) {
                if ((info->xmit_cnt <= 0) || info->tty->stopped) {
                        info->IER &= ~0x02;
                        serial_out(info, 0x04, 0x06);
                        serial_out(info, 0x05, info->IER);
                } else {
                        int num_bytes;
                        serial_out(info, 0x04, 0xff);
                        serial_out(info, 0x04, 0x15);
                        num_bytes = serial_in(info, 0x04) << 8;
                        num_bytes |= serial_in(info, 0x05);
                        if (num_bytes > info->xmit_cnt)
                                num_bytes = info->xmit_cnt;
                        if (num_bytes) {
                                if (dma_bytes ||
                                    (info->stat_flags & 0x10) ||
                                    (num_bytes <= info->config.pio_threshold))
                                        transmit_chars_pio(info, num_bytes);
                                else
                                        transmit_chars_dma(info, num_bytes);
                        }
                }
        }
        info->last_active = jiffies;
        _spin_unlock(&cli_lock);;
}
static void do_serial_bh(void)
{
        run_task_queue(&tq_esp);
}
static void do_softint(void *private_)
{
        struct esp_struct *info = (struct esp_struct *) private_;
        struct tty_struct *tty;
        tty = info->tty;
        if (!tty)
                return;
        if (test_and_clear_bit(0, &info->event)) {
                if ((tty->flags & (1 << 5)) &&
                    tty->ldisc.write_wakeup)
                        (tty->ldisc.write_wakeup)(tty);
                __wake_up((&tty->write_wait),1, 1);
        }
}
static void do_serial_hangup(void *private_)
{
        struct esp_struct *info = (struct esp_struct *) private_;
        struct tty_struct *tty;
        tty = info->tty;
        if (tty)
                tty_hangup(tty);
        __MOD_DEC_USE_COUNT((&__this_module));
}
static inline void esp_basic_init(struct esp_struct * info)
{
        serial_out(info, 0x04, 0x10);
        if (info->stat_flags & 0x08)
                serial_out(info, 0x05, 0x01);
        else
                serial_out(info, 0x05, 0x31);
        serial_out(info, 0x04, 0x06);
        serial_out(info, 0x05, 0x00);
        serial_out(info, 0x04, 0x04);
        if (info->stat_flags & 0x08)
                serial_out(info, 0x05, 0x01);
        else
                serial_out(info, 0x05, (dma << 4) | 0x01);
        serial_out(info, 0x04, 0x1f);
        if (info->line % 8)
                serial_out(info, 0x05, 0x0d);
        else if (info->irq == 9)
                serial_out(info, 0x05, 0x02);
        else
                serial_out(info, 0x05, info->irq);
        serial_out(info, 0x04, 0x07);
        if (info->stat_flags & 0x08)
                serial_out(info, 0x05, 0xa1);
        else
                serial_out(info, 0x05, 0xbd);
        serial_out(info, 0x05, 0x00);
        serial_out(info, 0x04, 0x05);
        serial_out(info, 0x05, 0xff);
        serial_out(info, 0x04, 0x0b);
        serial_out(info, 0x05, info->config.rx_trigger >> 8);
        serial_out(info, 0x05, info->config.rx_trigger);
        serial_out(info, 0x05, info->config.tx_trigger >> 8);
        serial_out(info, 0x05, info->config.tx_trigger);
        serial_out(info, 0x04, 0x23);
        serial_out(info, 0x05, 0x04 | 3);
        serial_out(info, 0x04, 0x20);
        serial_out(info, 0x05, 0xff);
}
static int startup(struct esp_struct * info)
{
        unsigned long flags;
        int retval=0;
        unsigned int num_chars;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (info->flags & 0x80000000)
                goto out;
        if (!info->xmit_buf) {
                info->xmit_buf = (unsigned char *)get_zeroed_page((0x10 | 0x40 | 0x80));
                retval = -12;
                if (!info->xmit_buf)
                        goto out;
        }
        serial_out(info, 0x04, 0xff);
        serial_out(info, 0x04, 0x14);
        num_chars = serial_in(info, 0x04) << 8;
        num_chars |= serial_in(info, 0x05);
        while (num_chars > 1) {
                inw(info->port + 0x02);
                num_chars -= 2;
        }
        if (num_chars)
                serial_in(info, 0x02);
        serial_out(info, 0x04, 0x0c);
        serial_out(info, 0x05, info->config.rx_timeout);
        info->stat_flags &= 0x08;
        if (info->stat_flags & 0x08)
                info->stat_flags |= 0x10;
        retval = request_irq(info->irq, rs_interrupt_single, 0x04000000,
                             "esp serial", info);
        if (retval) {
                if (capable(21)) {
                        if (info->tty)
                                set_bit(1,
                                        &info->tty->flags);
                        retval = 0;
                }
                goto out;
        }
        if (!(info->stat_flags & 0x10) && !dma_buffer) {
                dma_buffer = (char *)__get_free_pages(((0x10 | 0x40 | 0x80)) | 0x01,(get_order(1024)));
                if (!dma_buffer)
                        info->stat_flags |= 0x10;
                else if (request_dma(dma, "esp serial")) {
                        free_pages((unsigned long)dma_buffer,
                                   get_order(1024));
                        dma_buffer = 0;
                        info->stat_flags |= 0x10;
                }
        }
        info->MCR = 0x01 | 0x02 | 0x08;
        serial_out(info, 0x04, 0x0e);
        serial_out(info, 0x05, 4);
        serial_out(info, 0x05, info->MCR);
        info->IER = 0x04 | 0x01 | 0x80 |
                        0x08;
        serial_out(info, 0x04, 0x06);
        serial_out(info, 0x05, info->IER);
        if (info->tty)
                clear_bit(1, &info->tty->flags);
        info->xmit_cnt = info->xmit_head = info->xmit_tail = 0;
        if (info->tty) {
                if ((info->flags & 0x1030) == 0x0010)
                        info->tty->alt_speed = 57600;
                if ((info->flags & 0x1030) == 0x0020)
                        info->tty->alt_speed = 115200;
                if ((info->flags & 0x1030) == 0x1000)
                        info->tty->alt_speed = 230400;
                if ((info->flags & 0x1030) == 0x1010)
                        info->tty->alt_speed = 460800;
        }
        change_speed(info);
        info->flags |= 0x80000000;
        retval = 0;
out: _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        return retval;
}
static void shutdown(struct esp_struct * info)
{
        unsigned long flags, f;
        if (!(info->flags & 0x80000000))
                return;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        __wake_up((&info->delta_msr_wait),1, 1);
        __wake_up((&info->break_wait),1, 1);
        if (info->stat_flags & (0x02 | 0x04)) {
                f=claim_dma_lock();
                disable_dma(dma);
                clear_dma_ff(dma);
                release_dma_lock(f);
                dma_bytes = 0;
        }
        free_irq(info->irq, info);
        if (dma_buffer) {
                struct esp_struct *current_port = ports;
                while (current_port) {
                        if ((current_port != info) &&
                            (current_port->flags & 0x80000000))
                                break;
                        current_port = current_port->next_port;
                }
                if (!current_port) {
                        free_dma(dma);
                        free_pages((unsigned long)dma_buffer,
                                   get_order(1024));
                        dma_buffer = 0;
                }
        }
        if (info->xmit_buf) {
                free_pages(((unsigned long) info->xmit_buf),0);
                info->xmit_buf = 0;
        }
        info->IER = 0;
        serial_out(info, 0x04, 0x06);
        serial_out(info, 0x05, 0x00);
        if (!info->tty || (info->tty->termios->c_cflag & 0002000))
                info->MCR &= ~(0x01|0x02);
        info->MCR &= ~0x08;
        serial_out(info, 0x04, 0x0e);
        serial_out(info, 0x05, 4);
        serial_out(info, 0x05, info->MCR);
        if (info->tty)
                set_bit(1, &info->tty->flags);
        info->flags &= ~0x80000000;
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static void change_speed(struct esp_struct *info)
{
        unsigned short port;
        int quot = 0;
        unsigned cflag,cval;
        int baud, bits;
        unsigned char flow1 = 0, flow2 = 0;
        unsigned long flags;
        if (!info->tty || !info->tty->termios)
                return;
        cflag = info->tty->termios->c_cflag;
        port = info->port;
        switch (cflag & 0000060) {
              case 0000000: cval = 0x00; bits = 7; break;
              case 0000020: cval = 0x01; bits = 8; break;
              case 0000040: cval = 0x02; bits = 9; break;
              case 0000060: cval = 0x03; bits = 10; break;
              default: cval = 0x00; bits = 7; break;
        }
        if (cflag & 0000100) {
                cval |= 0x04;
                bits++;
        }
        if (cflag & 0000400) {
                cval |= 0x08;
                bits++;
        }
        if (!(cflag & 0001000))
                cval |= 0x10;
        if (cflag & 010000000000)
                cval |= 0x20;
        baud = tty_get_baud_rate(info->tty);
        if (baud == 38400 &&
            ((info->flags & 0x1030) == 0x0030))
                quot = info->custom_divisor;
        else {
                if (baud == 134)
                        quot = (2*((1843200 / 16) * (1 << 3)) / 269);
                else if (baud)
                        quot = ((1843200 / 16) * (1 << 3)) / baud;
        }
        if (!quot)
                quot = ((1843200 / 16) * (1 << 3)) / 9600;
        info->timeout = ((1024 * 1000 * bits * quot) / ((1843200 / 16) * (1 << 3))) + (1000 / 50);
        if (cflag & 020000000000) {
                info->flags |= 0x04000000;
                flow1 = 0x04;
                flow2 = 0x10;
        } else
                info->flags &= ~0x04000000;
        if (cflag & 0004000)
                info->flags &= ~0x02000000;
        else {
                info->flags |= 0x02000000;
        }
        info->read_status_mask = 0x02 | 0x20 | 0x01;
        if ((((info->tty))->termios->c_iflag & (0000020)))
                info->read_status_mask |= 0x08 | 0x04;
        if ((((info->tty))->termios->c_iflag & (0000002)) || (((info->tty))->termios->c_iflag & (0000010)))
                info->read_status_mask |= 0x10;
        info->ignore_status_mask = 0;
        if ((((info->tty))->termios->c_iflag & (0000001))) {
                info->ignore_status_mask |= 0x10;
                info->read_status_mask |= 0x10;
                if ((((info->tty))->termios->c_iflag & (0000004))) {
                        info->ignore_status_mask |= 0x02 | 0x04 | 0x08;
                        info->read_status_mask |= 0x02 | 0x04 | 0x08;
                }
        }
        if ((((info->tty))->termios->c_iflag & (0010000)))
                flow1 |= 0x81;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        serial_out(info, 0x04, 0x1d);
        serial_out(info, 0x05, quot >> 8);
        serial_out(info, 0x05, quot & 0xff);
        serial_out(info, 0x04, 0x0e);
        serial_out(info, 0x05, 3);
        serial_out(info, 0x05, cval);
        serial_out(info, 0x04, 0x08);
        serial_out(info, 0x05, flow1);
        serial_out(info, 0x05, flow2);
        if ((((info->tty))->termios->c_iflag & (0010000))) {
                serial_out(info, 0x04, 0x09);
                serial_out(info, 0x05, ((info->tty)->termios->c_cc[8]));
                serial_out(info, 0x05, ((info->tty)->termios->c_cc[9]));
                serial_out(info, 0x05, 0x10);
                serial_out(info, 0x05, 0x21);
                switch (cflag & 0000060) {
                        case 0000000:
                                serial_out(info, 0x05, 0x1f);
                                break;
                        case 0000020:
                                serial_out(info, 0x05, 0x3f);
                                break;
                        case 0000040:
                        case 0000060:
                                serial_out(info, 0x05, 0x7f);
                                break;
                        default:
                                serial_out(info, 0x05, 0xff);
                                break;
                }
        }
        serial_out(info, 0x04, 0x0a);
        serial_out(info, 0x05, info->config.flow_off >> 8);
        serial_out(info, 0x05, info->config.flow_off);
        serial_out(info, 0x05, info->config.flow_on >> 8);
        serial_out(info, 0x05, info->config.flow_on);
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static void rs_put_char(struct tty_struct *tty, unsigned char ch)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        unsigned long flags;
        if (serial_paranoia_check(info, tty->device, "rs_put_char"))
                return;
        if (!tty || !info->xmit_buf)
                return;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (info->xmit_cnt >= 4096 - 1) {
                _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
                return;
        }
        info->xmit_buf[info->xmit_head++] = ch;
        info->xmit_head &= 4096 -1;
        info->xmit_cnt++;
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static void rs_flush_chars(struct tty_struct *tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        unsigned long flags;
        if (serial_paranoia_check(info, tty->device, "rs_flush_chars"))
                return;
        if (info->xmit_cnt <= 0 || tty->stopped || !info->xmit_buf)
                return;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (!(info->IER & 0x02)) {
                info->IER |= 0x02;
                serial_out(info, 0x04, 0x06);
                serial_out(info, 0x05, info->IER);
        }
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static int rs_write(struct tty_struct * tty, int from_user,
                    const unsigned char *buf, int count)
{
        int c, t, ret = 0;
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        unsigned long flags;
        if (serial_paranoia_check(info, tty->device, "rs_write"))
                return 0;
        if (!tty || !info->xmit_buf || !tmp_buf)
                return 0;
        if (from_user)
                down(&tmp_buf_sem);
        while (1) {
                c = count;
                t = 4096 - info->xmit_cnt - 1;
                if (t < c)
                        c = t;
                t = 4096 - info->xmit_head;
                if (t < c)
                        c = t;
                if (c <= 0)
                        break;
                if (from_user) {
                        c -= copy_from_user(tmp_buf, buf, c);
                        if (!c) {
                                if (!ret)
                                        ret = -14;
                                break;
                        }
                        (__builtin_constant_p(c) ? __constant_memcpy((info->xmit_buf + info->xmit_head),(tmp_buf),(c)) : __memcpy((info->xmit_buf + info->xmit_head),(tmp_buf),(c)));
                } else
                        (__builtin_constant_p(c) ? __constant_memcpy((info->xmit_buf + info->xmit_head),(buf),(c)) : __memcpy((info->xmit_buf + info->xmit_head),(buf),(c)));
                info->xmit_head = (info->xmit_head + c) & (4096 -1);
                info->xmit_cnt += c;
                buf += c;
                count -= c;
                ret += c;
        }
        if (from_user)
                up(&tmp_buf_sem);
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (info->xmit_cnt && !tty->stopped && !(info->IER & 0x02)) {
                info->IER |= 0x02;
                serial_out(info, 0x04, 0x06);
                serial_out(info, 0x05, info->IER);
        }
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        return ret;
}
static int rs_write_room(struct tty_struct *tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        int ret;
        if (serial_paranoia_check(info, tty->device, "rs_write_room"))
                return 0;
        ret = 4096 - info->xmit_cnt - 1;
        if (ret < 0)
                ret = 0;
        return ret;
}
static int rs_chars_in_buffer(struct tty_struct *tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        if (serial_paranoia_check(info, tty->device, "rs_chars_in_buffer"))
                return 0;
        return info->xmit_cnt;
}
static void rs_flush_buffer(struct tty_struct *tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        if (serial_paranoia_check(info, tty->device, "rs_flush_buffer"))
                return;
        _spin_lock(&cli_lock);;
        info->xmit_cnt = info->xmit_head = info->xmit_tail = 0;
        _spin_unlock(&cli_lock);;
        __wake_up((&tty->write_wait),1, 1);
        if ((tty->flags & (1 << 5)) &&
            tty->ldisc.write_wakeup)
                (tty->ldisc.write_wakeup)(tty);
}
static void rs_throttle(struct tty_struct * tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        if (serial_paranoia_check(info, tty->device, "rs_throttle"))
                return;
        _spin_lock(&cli_lock);;
        info->IER &= ~0x01;
        serial_out(info, 0x04, 0x06);
        serial_out(info, 0x05, info->IER);
        serial_out(info, 0x04, 0x0c);
        serial_out(info, 0x05, 0x00);
        _spin_unlock(&cli_lock);;
}
static void rs_unthrottle(struct tty_struct * tty)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        if (serial_paranoia_check(info, tty->device, "rs_unthrottle"))
                return;
        _spin_lock(&cli_lock);;
        info->IER |= 0x01;
        serial_out(info, 0x04, 0x06);
        serial_out(info, 0x05, info->IER);
        serial_out(info, 0x04, 0x0c);
        serial_out(info, 0x05, info->config.rx_timeout);
        _spin_unlock(&cli_lock);;
}
static int get_serial_info(struct esp_struct * info,
                           struct serial_struct * retinfo)
{
        struct serial_struct tmp;
        if (!retinfo)
                return -14;
        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(tmp))) ? __constant_c_and_count_memset(((&tmp)),((0x01010101UL*(unsigned char)(0))),((sizeof(tmp)))) : __constant_c_memset(((&tmp)),((0x01010101UL*(unsigned char)(0))),((sizeof(tmp))))) : (__builtin_constant_p((sizeof(tmp))) ? __memset_generic((((&tmp))),(((0))),(((sizeof(tmp))))) : __memset_generic(((&tmp)),((0)),((sizeof(tmp))))));
        tmp.type = 4;
        tmp.line = info->line;
        tmp.port = info->port;
        tmp.irq = info->irq;
        tmp.flags = info->flags;
        tmp.xmit_fifo_size = 1024;
        tmp.baud_base = ((1843200 / 16) * (1 << 3));
        tmp.close_delay = info->close_delay;
        tmp.closing_wait = info->closing_wait;
        tmp.custom_divisor = info->custom_divisor;
        tmp.hub6 = 0;
        if (copy_to_user(retinfo,&tmp,sizeof(*retinfo)))
                return -14;
        return 0;
}
static int get_esp_config(struct esp_struct * info,
                          struct hayes_esp_config * retinfo)
{
        struct hayes_esp_config tmp;
        if (!retinfo)
                return -14;
        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(tmp))) ? __constant_c_and_count_memset(((&tmp)),((0x01010101UL*(unsigned char)(0))),((sizeof(tmp)))) : __constant_c_memset(((&tmp)),((0x01010101UL*(unsigned char)(0))),((sizeof(tmp))))) : (__builtin_constant_p((sizeof(tmp))) ? __memset_generic((((&tmp))),(((0))),(((sizeof(tmp))))) : __memset_generic(((&tmp)),((0)),((sizeof(tmp))))));
        tmp.rx_timeout = info->config.rx_timeout;
        tmp.rx_trigger = info->config.rx_trigger;
        tmp.tx_trigger = info->config.tx_trigger;
        tmp.flow_off = info->config.flow_off;
        tmp.flow_on = info->config.flow_on;
        tmp.pio_threshold = info->config.pio_threshold;
        tmp.dma_channel = (info->stat_flags & 0x08 ? 0 : dma);
        return copy_to_user(retinfo, &tmp, sizeof(*retinfo)) ? -14 : 0;
}
static int set_serial_info(struct esp_struct * info,
                           struct serial_struct * new_info)
{
        struct serial_struct new_serial;
        struct esp_struct old_info;
        unsigned int change_irq;
        int retval = 0;
        struct esp_struct *current_async;
        if (copy_from_user(&new_serial,new_info,sizeof(new_serial)))
                return -14;
        old_info = *info;
        if ((new_serial.type != 4) ||
            (new_serial.hub6) ||
            (info->port != new_serial.port) ||
            (new_serial.baud_base != ((1843200 / 16) * (1 << 3))) ||
            (new_serial.irq > 15) ||
            (new_serial.irq < 2) ||
            (new_serial.irq == 6) ||
            (new_serial.irq == 8) ||
            (new_serial.irq == 13))
                return -22;
        change_irq = new_serial.irq != info->irq;
        if (change_irq && (info->line % 8))
                return -22;
        if (!capable(21)) {
                if (change_irq ||
                    (new_serial.close_delay != info->close_delay) ||
                    ((new_serial.flags & ~0x3430) !=
                     (info->flags & ~0x3430)))
                        return -1;
                info->flags = ((info->flags & ~0x3430) |
                               (new_serial.flags & 0x3430));
                info->custom_divisor = new_serial.custom_divisor;
        } else {
                if (new_serial.irq == 2)
                        new_serial.irq = 9;
                if (change_irq) {
                        current_async = ports;
                        while (current_async) {
                                if ((current_async->line >= info->line) &&
                                    (current_async->line < (info->line + 8))) {
                                        if (current_async == info) {
                                                if (current_async->count > 1)
                                                        return -16;
                                        } else if (current_async->count)
                                                return -16;
                                }
                                current_async = current_async->next_port;
                        }
                }
                info->flags = ((info->flags & ~0x7FFF) |
                               (new_serial.flags & 0x7FFF));
                info->custom_divisor = new_serial.custom_divisor;
                info->close_delay = new_serial.close_delay * 1000/100;
                info->closing_wait = new_serial.closing_wait * 1000/100;
                if (change_irq) {
                        shutdown(info);
                        current_async = ports;
                        while (current_async) {
                                if ((current_async->line >= info->line) &&
                                    (current_async->line < (info->line + 8)))
                                        current_async->irq = new_serial.irq;
                                current_async = current_async->next_port;
                        }
                        serial_out(info, 0x04, 0x1f);
                        if (info->irq == 9)
                                serial_out(info, 0x05, 0x02);
                        else
                                serial_out(info, 0x05, info->irq);
                }
        }
        if (info->flags & 0x80000000) {
                if (((old_info.flags & 0x1030) !=
                     (info->flags & 0x1030)) ||
                    (old_info.custom_divisor != info->custom_divisor)) {
                        if ((info->flags & 0x1030) == 0x0010)
                                info->tty->alt_speed = 57600;
                        if ((info->flags & 0x1030) == 0x0020)
                                info->tty->alt_speed = 115200;
                        if ((info->flags & 0x1030) == 0x1000)
                                info->tty->alt_speed = 230400;
                        if ((info->flags & 0x1030) == 0x1010)
                                info->tty->alt_speed = 460800;
                        change_speed(info);
                }
        } else
                retval = startup(info);
        return retval;
}
static int set_esp_config(struct esp_struct * info,
                          struct hayes_esp_config * new_info)
{
        struct hayes_esp_config new_config;
        unsigned int change_dma;
        int retval = 0;
        struct esp_struct *current_async;
        if (!capable(21))
                return -1;
        if (copy_from_user(&new_config, new_info, sizeof(new_config)))
                return -14;
        if ((new_config.flow_on >= new_config.flow_off) ||
            (new_config.rx_trigger < 1) ||
            (new_config.tx_trigger < 1) ||
            (new_config.flow_off < 1) ||
            (new_config.flow_on < 1) ||
            (new_config.rx_trigger > 1023) ||
            (new_config.tx_trigger > 1023) ||
            (new_config.flow_off > 1023) ||
            (new_config.flow_on > 1023) ||
            (new_config.pio_threshold < 0) ||
            (new_config.pio_threshold > 1024))
                return -22;
        if ((new_config.dma_channel != 1) && (new_config.dma_channel != 3))
                new_config.dma_channel = 0;
        if (info->stat_flags & 0x08)
                change_dma = new_config.dma_channel;
        else
                change_dma = (new_config.dma_channel != dma);
        if (change_dma) {
                if (new_config.dma_channel) {
                        current_async = ports;
                        while (current_async) {
                                if (current_async == info) {
                                        if (current_async->count > 1)
                                                return -16;
                                } else if (current_async->count)
                                        return -16;
                                current_async =
                                        current_async->next_port;
                        }
                        shutdown(info);
                        dma = new_config.dma_channel;
                        info->stat_flags &= ~0x08;
                        current_async = ports;
                        while (current_async) {
                                esp_basic_init(current_async);
                                current_async = current_async->next_port;
                        }
                } else {
                        if (info->count > 1)
                                return -16;
                        shutdown(info);
                        info->stat_flags |= 0x08;
                        esp_basic_init(info);
                }
        }
        info->config.pio_threshold = new_config.pio_threshold;
        if ((new_config.flow_off != info->config.flow_off) ||
            (new_config.flow_on != info->config.flow_on)) {
                unsigned long flags;
                info->config.flow_off = new_config.flow_off;
                info->config.flow_on = new_config.flow_on;
                __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
                serial_out(info, 0x04, 0x0a);
                serial_out(info, 0x05, new_config.flow_off >> 8);
                serial_out(info, 0x05, new_config.flow_off);
                serial_out(info, 0x05, new_config.flow_on >> 8);
                serial_out(info, 0x05, new_config.flow_on);
                _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        }
        if ((new_config.rx_trigger != info->config.rx_trigger) ||
            (new_config.tx_trigger != info->config.tx_trigger)) {
                unsigned long flags;
                info->config.rx_trigger = new_config.rx_trigger;
                info->config.tx_trigger = new_config.tx_trigger;
                __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
                serial_out(info, 0x04, 0x0b);
                serial_out(info, 0x05,
                           new_config.rx_trigger >> 8);
                serial_out(info, 0x05, new_config.rx_trigger);
                serial_out(info, 0x05,
                           new_config.tx_trigger >> 8);
                serial_out(info, 0x05, new_config.tx_trigger);
                _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        }
        if (new_config.rx_timeout != info->config.rx_timeout) {
                unsigned long flags;
                info->config.rx_timeout = new_config.rx_timeout;
                __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
                if (info->IER & 0x01) {
                        serial_out(info, 0x04,
                                   0x0c);
                        serial_out(info, 0x05,
                                   new_config.rx_timeout);
                }
                _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        }
        if (!(info->flags & 0x80000000))
                retval = startup(info);
        return retval;
}
static int get_lsr_info(struct esp_struct * info, unsigned int *value)
{
        unsigned char status;
        unsigned int result;
        _spin_lock(&cli_lock);;
        serial_out(info, 0x04, 0x13);
        status = serial_in(info, 0x04);
        _spin_unlock(&cli_lock);;
        result = ((status & 0x40) ? 0x01 : 0);
        return ({ long __pu_err = -14; __typeof__(*((value))) *__pu_addr = ((value)); if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (__pu_addr),"g" ((int)(sizeof(*(value)))),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) do { __pu_err = 0; switch ((sizeof(*(value)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(value)))(result))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(value)))(result))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(value)))(result))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*__pu_addr))(((__typeof__(*(value)))(result)))), "r" (__pu_addr), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; });
}
static int get_modem_info(struct esp_struct * info, unsigned int *value)
{
        unsigned char control, status;
        unsigned int result;
        control = info->MCR;
        _spin_lock(&cli_lock);;
        serial_out(info, 0x04, 0x13);
        status = serial_in(info, 0x05);
        _spin_unlock(&cli_lock);;
        result = ((control & 0x02) ? 0x004 : 0)
                | ((control & 0x01) ? 0x002 : 0)
                | ((status & 0x80) ? 0x040 : 0)
                | ((status & 0x40) ? 0x080 : 0)
                | ((status & 0x20) ? 0x100 : 0)
                | ((status & 0x10) ? 0x020 : 0);
        return ({ long __pu_err = -14; __typeof__(*((value))) *__pu_addr = ((value)); if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (__pu_addr),"g" ((int)(sizeof(*(value)))),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) do { __pu_err = 0; switch ((sizeof(*(value)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(value)))(result))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(value)))(result))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(value)))(result))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*__pu_addr))(((__typeof__(*(value)))(result)))), "r" (__pu_addr), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; });
}
static int set_modem_info(struct esp_struct * info, unsigned int cmd,
                          unsigned int *value)
{
        unsigned int arg;
        if (({ int __ret_gu,__val_gu; switch(sizeof (*(value))) { case 1: __asm__ __volatile__("call __get_user_" "1" :"=a" (__ret_gu),"=d" (__val_gu) :"0" (value)); break; case 2: __asm__ __volatile__("call __get_user_" "2" :"=a" (__ret_gu),"=d" (__val_gu) :"0" (value)); break; case 4: __asm__ __volatile__("call __get_user_" "4" :"=a" (__ret_gu),"=d" (__val_gu) :"0" (value)); break; default: __asm__ __volatile__("call __get_user_" "X" :"=a" (__ret_gu),"=d" (__val_gu) :"0" (value)); break; } (arg) = (__typeof__(*(value)))__val_gu; __ret_gu; }))
                return -14;
        switch (cmd) {
        case 0x5416:
                if (arg & 0x004)
                        info->MCR |= 0x02;
                if (arg & 0x002)
                        info->MCR |= 0x01;
                break;
        case 0x5417:
                if (arg & 0x004)
                        info->MCR &= ~0x02;
                if (arg & 0x002)
                        info->MCR &= ~0x01;
                break;
        case 0x5418:
                info->MCR = ((info->MCR & ~(0x02 | 0x01))
                             | ((arg & 0x004) ? 0x02 : 0)
                             | ((arg & 0x002) ? 0x01 : 0));
                break;
        default:
                return -22;
        }
        _spin_lock(&cli_lock);;
        serial_out(info, 0x04, 0x0e);
        serial_out(info, 0x05, 4);
        serial_out(info, 0x05, info->MCR);
        _spin_unlock(&cli_lock);;
        return 0;
}
static void esp_break(struct tty_struct *tty, int break_state)
{
        struct esp_struct * info = (struct esp_struct *)tty->driver_data;
        unsigned long flags;
        if (serial_paranoia_check(info, tty->device, "esp_break"))
                return;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (break_state == -1) {
                serial_out(info, 0x04, 0x1a);
                serial_out(info, 0x05, 0x01);
                interruptible_sleep_on(&info->break_wait);
        } else {
                serial_out(info, 0x04, 0x1a);
                serial_out(info, 0x05, 0x00);
        }
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static int rs_ioctl(struct tty_struct *tty, struct file * file,
                    unsigned int cmd, unsigned long arg)
{
        struct esp_struct * info = (struct esp_struct *)tty->driver_data;
        struct async_icount cprev, cnow;
        struct serial_icounter_struct *p_cuser;
        if (serial_paranoia_check(info, tty->device, "rs_ioctl"))
                return -19;
        if ((cmd != 0x541E) && (cmd != 0x541F) &&
            (cmd != 0x5453) && (cmd != 0x5454) &&
            (cmd != 0x5455) && (cmd != 0x5458) &&
            (cmd != 0x545C) && (cmd != 0x545D) &&
            (cmd != 0x545E) && (cmd != 0x545F)) {
                if (tty->flags & (1 << 1))
                    return -5;
        }
        switch (cmd) {
                case 0x5415:
                        return get_modem_info(info, (unsigned int *) arg);
                case 0x5416:
                case 0x5417:
                case 0x5418:
                        return set_modem_info(info, cmd, (unsigned int *) arg);
                case 0x541E:
                        return get_serial_info(info,
                                               (struct serial_struct *) arg);
                case 0x541F:
                        return set_serial_info(info,
                                               (struct serial_struct *) arg);
                case 0x5453:
                        return 0;
                case 0x5454:
                        return ({ long __pu_err = -14; __typeof__(*(((unsigned long *) arg))) *__pu_addr = (((unsigned long *) arg)); if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (__pu_addr),"g" ((int)(sizeof(*((unsigned long *) arg)))),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) do { __pu_err = 0; switch ((sizeof(*((unsigned long *) arg)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*((unsigned long *) arg)))(0L))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*((unsigned long *) arg)))(0L))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*((unsigned long *) arg)))(0L))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*__pu_addr))(((__typeof__(*((unsigned long *) arg)))(0L)))), "r" (__pu_addr), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; });
                case 0x5459:
                            return get_lsr_info(info, (unsigned int *) arg);
                case 0x5455:
                        if (!capable(21))
                                return -1;
                        return 0;
                 case 0x545C:
                        _spin_lock(&cli_lock);;
                        cprev = info->icount;
                        _spin_unlock(&cli_lock);;
                        while (1) {
                                interruptible_sleep_on(&info->delta_msr_wait);
                                if (signal_pending(get_current()))
                                        return -512;
                                _spin_lock(&cli_lock);;
                                cnow = info->icount;
                                _spin_unlock(&cli_lock);;
                                if (cnow.rng == cprev.rng &&
                                    cnow.dsr == cprev.dsr &&
                                    cnow.dcd == cprev.dcd &&
                                    cnow.cts == cprev.cts)
                                        return -5;
                                if (((arg & 0x080) &&
                                     (cnow.rng != cprev.rng)) ||
                                     ((arg & 0x100) &&
                                      (cnow.dsr != cprev.dsr)) ||
                                     ((arg & 0x040) &&
                                      (cnow.dcd != cprev.dcd)) ||
                                     ((arg & 0x020) &&
                                      (cnow.cts != cprev.cts)) ) {
                                        return 0;
                                }
                                cprev = cnow;
                        }
                case 0x545D:
                        _spin_lock(&cli_lock);;
                        cnow = info->icount;
                        _spin_unlock(&cli_lock);;
                        p_cuser = (struct serial_icounter_struct *) arg;
                        if (({ long __pu_err = -14; __typeof__(*((&p_cuser->cts))) *__pu_addr = ((&p_cuser->cts)); if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (__pu_addr),"g" ((int)(sizeof(*(&p_cuser->cts)))),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) do { __pu_err = 0; switch ((sizeof(*(&p_cuser->cts)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(&p_cuser->cts)))(cnow.cts))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->cts)))(cnow.cts))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->cts)))(cnow.cts))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*__pu_addr))(((__typeof__(*(&p_cuser->cts)))(cnow.cts)))), "r" (__pu_addr), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; }) ||
                            ({ long __pu_err = -14; __typeof__(*((&p_cuser->dsr))) *__pu_addr = ((&p_cuser->dsr)); if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (__pu_addr),"g" ((int)(sizeof(*(&p_cuser->dsr)))),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) do { __pu_err = 0; switch ((sizeof(*(&p_cuser->dsr)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(&p_cuser->dsr)))(cnow.dsr))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->dsr)))(cnow.dsr))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->dsr)))(cnow.dsr))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*__pu_addr))(((__typeof__(*(&p_cuser->dsr)))(cnow.dsr)))), "r" (__pu_addr), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; }) ||
                            ({ long __pu_err = -14; __typeof__(*((&p_cuser->rng))) *__pu_addr = ((&p_cuser->rng)); if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (__pu_addr),"g" ((int)(sizeof(*(&p_cuser->rng)))),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) do { __pu_err = 0; switch ((sizeof(*(&p_cuser->rng)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(&p_cuser->rng)))(cnow.rng))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->rng)))(cnow.rng))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->rng)))(cnow.rng))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*__pu_addr))(((__typeof__(*(&p_cuser->rng)))(cnow.rng)))), "r" (__pu_addr), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; }) ||
                            ({ long __pu_err = -14; __typeof__(*((&p_cuser->dcd))) *__pu_addr = ((&p_cuser->dcd)); if ((({ unsigned long flag,sum; asm("addl %3,%1 ; sbbl %0,%0; cmpl %1,%4; sbbl $0,%0" :"=&r" (flag), "=r" (sum) :"1" (__pu_addr),"g" ((int)(sizeof(*(&p_cuser->dcd)))),"g" (current_thread_info()->addr_limit.seg)); flag; }) == 0)) do { __pu_err = 0; switch ((sizeof(*(&p_cuser->dcd)))) { case 1: __asm__ __volatile__( "1:	mov""b"" %""b""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "iq" (((__typeof__(*(&p_cuser->dcd)))(cnow.dcd))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 2: __asm__ __volatile__( "1:	mov""w"" %""w""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->dcd)))(cnow.dcd))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err));break; case 4: __asm__ __volatile__( "1:	mov""l"" %""""1,%2\n" "2:\n" ".section .fixup,\"ax\"\n" "3:	movl %3,%0\n" "	jmp 2b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,3b\n" ".previous" : "=r"(__pu_err) : "ir" (((__typeof__(*(&p_cuser->dcd)))(cnow.dcd))), "m"((*(struct __large_struct *)(__pu_addr))), "i"(-14), "0"(__pu_err)); break; case 8: __asm__ __volatile__( "1:	movl %%eax,0(%2)\n" "2:	movl %%edx,4(%2)\n" "3:\n" ".section .fixup,\"ax\"\n" "4:	movl %3,%0\n" "	jmp 3b\n" ".previous\n" ".section __ex_table,\"a\"\n" "	.align 4\n" "	.long 1b,4b\n" "	.long 2b,4b\n" ".previous" : "=r"(__pu_err) : "A" ((__typeof__(*__pu_addr))(((__typeof__(*(&p_cuser->dcd)))(cnow.dcd)))), "r" (__pu_addr), "i"(-14), "0"(__pu_err)); break; default: __put_user_bad(); } } while (0); __pu_err; }))
                                return -14;
                        return 0;
        case 0x545E:
                return (get_esp_config(info, (struct hayes_esp_config *)arg));
        case 0x545F:
                return (set_esp_config(info, (struct hayes_esp_config *)arg));
                default:
                        return -515;
                }
        return 0;
}
static void rs_set_termios(struct tty_struct *tty, struct termios *old_termios)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        if ( (tty->termios->c_cflag == old_termios->c_cflag)
            && ( (tty->termios->c_iflag & (0000001|0000002|0000004|0000010|0000020))
                == (old_termios->c_iflag & (0000001|0000002|0000004|0000010|0000020))))
          return;
        change_speed(info);
        if ((old_termios->c_cflag & 0010017) &&
                !(tty->termios->c_cflag & 0010017)) {
                info->MCR &= ~(0x01|0x02);
                _spin_lock(&cli_lock);;
                serial_out(info, 0x04, 0x0e);
                serial_out(info, 0x05, 4);
                serial_out(info, 0x05, info->MCR);
                _spin_unlock(&cli_lock);;
        }
        if (!(old_termios->c_cflag & 0010017) &&
                (tty->termios->c_cflag & 0010017)) {
                info->MCR |= (0x01 | 0x02);
                _spin_lock(&cli_lock);;
                serial_out(info, 0x04, 0x0e);
                serial_out(info, 0x05, 4);
                serial_out(info, 0x05, info->MCR);
                _spin_unlock(&cli_lock);;
        }
        if ((old_termios->c_cflag & 020000000000) &&
            !(tty->termios->c_cflag & 020000000000)) {
                rs_start(tty);
        }
}
static void rs_close(struct tty_struct *tty, struct file * filp)
{
        struct esp_struct * info = (struct esp_struct *)tty->driver_data;
        unsigned long flags;
        if (!info || serial_paranoia_check(info, tty->device, "rs_close"))
                return;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (tty_hung_up_p(filp)) {
                ;
                goto out;
        }
        if ((tty->count == 1) && (info->count != 1)) {
                printk("rs_close: bad serial port count; tty->count is 1, "
                       "info->count is %d\n", info->count);
                info->count = 1;
        }
        if (--info->count < 0) {
                printk("rs_close: bad serial port count for ttys%d: %d\n",
                       info->line, info->count);
                info->count = 0;
        }
        if (info->count) {
                ;
                goto out;
        }
        info->flags |= 0x08000000;
        if (info->flags & 0x20000000)
                info->normal_termios = *tty->termios;
        if (info->flags & 0x40000000)
                info->callout_termios = *tty->termios;
        tty->closing = 1;
        if (info->closing_wait != 65535)
                tty_wait_until_sent(tty, info->closing_wait);
        info->IER &= ~0x01;
        info->read_status_mask &= ~0x01;
        if (info->flags & 0x80000000) {
                serial_out(info, 0x04, 0x06);
                serial_out(info, 0x05, info->IER);
                serial_out(info, 0x04, 0x0c);
                serial_out(info, 0x05, 0x00);
                rs_wait_until_sent(tty, info->timeout);
        }
        shutdown(info);
        if (tty->driver.flush_buffer)
                tty->driver.flush_buffer(tty);
        if (tty->ldisc.flush_buffer)
                tty->ldisc.flush_buffer(tty);
        tty->closing = 0;
        info->event = 0;
        info->tty = 0;
        if (info->blocked_open) {
                if (info->close_delay) {
                        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
                        schedule_timeout(info->close_delay);
                }
                __wake_up((&info->open_wait),1, 1);
        }
        info->flags &= ~(0x20000000|0x40000000|
                         0x08000000);
        __wake_up((&info->close_wait),1, 1);
out: __MOD_DEC_USE_COUNT((&__this_module));
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
}
static void rs_wait_until_sent(struct tty_struct *tty, int timeout)
{
        struct esp_struct *info = (struct esp_struct *)tty->driver_data;
        unsigned long orig_jiffies, char_time;
        unsigned long flags;
        if (serial_paranoia_check(info, tty->device, "rs_wait_until_sent"))
                return;
        orig_jiffies = jiffies;
        char_time = ((info->timeout - 1000 / 50) / 1024) / 5;
        if (!char_time)
                char_time = 1;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        serial_out(info, 0x04, 0xff);
        serial_out(info, 0x04, 0x15);
        while ((serial_in(info, 0x04) != 0x03) ||
                (serial_in(info, 0x05) != 0xff)) {
                do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
                schedule_timeout(char_time);
                if (signal_pending(get_current()))
                        break;
                if (timeout && ((long)(orig_jiffies + timeout) - (long)(jiffies) < 0))
                        break;
                serial_out(info, 0x04, 0xff);
                serial_out(info, 0x04, 0x15);
        }
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((0)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
}
static void esp_hangup(struct tty_struct *tty)
{
        struct esp_struct * info = (struct esp_struct *)tty->driver_data;
        if (serial_paranoia_check(info, tty->device, "esp_hangup"))
                return;
        rs_flush_buffer(tty);
        shutdown(info);
        info->event = 0;
        info->count = 0;
        info->flags &= ~(0x20000000|0x40000000);
        info->tty = 0;
        __wake_up((&info->open_wait),1, 1);
}
static int block_til_ready(struct tty_struct *tty, struct file * filp,
                           struct esp_struct *info)
{
        wait_queue_t wait = { .task = get_current(), .func = default_wake_function, .task_list = { ((void *)0), ((void *)0) } };
        int retval;
        int do_clocal = 0;
        unsigned long flags;
        if (tty_hung_up_p(filp) ||
            (info->flags & 0x08000000)) {
                if (info->flags & 0x08000000)
                        interruptible_sleep_on(&info->close_wait);
                if (info->flags & 0x0001)
                        return -11;
                else
                        return -512;
        }
        if (tty->driver.subtype == 2) {
                if (info->flags & 0x20000000)
                        return -16;
                if ((info->flags & 0x40000000) &&
                    (info->flags & 0x0100) &&
                    (info->session != get_current()->session))
                    return -16;
                if ((info->flags & 0x40000000) &&
                    (info->flags & 0x0200) &&
                    (info->pgrp != get_current()->pgrp))
                    return -16;
                info->flags |= 0x40000000;
                return 0;
        }
        if ((filp->f_flags & 04000) ||
            (tty->flags & (1 << 1))) {
                if (info->flags & 0x40000000)
                        return -16;
                info->flags |= 0x20000000;
                return 0;
        }
        if (info->flags & 0x40000000) {
                if (info->normal_termios.c_cflag & 0004000)
                        do_clocal = 1;
        } else {
                if (tty->termios->c_cflag & 0004000)
                        do_clocal = 1;
        }
        retval = 0;
        add_wait_queue(&info->open_wait, &wait);
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): );
        _spin_lock(&cli_lock);;
        if (!tty_hung_up_p(filp))
                info->count--;
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        info->blocked_open++;
        while (1) {
                __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): );
                _spin_lock(&cli_lock);;
                if (!(info->flags & 0x40000000) &&
                        (tty->termios->c_cflag & 0010017)) {
                        unsigned int scratch;
                        serial_out(info, 0x04, 0x0f);
                        serial_out(info, 0x05, 4);
                        scratch = serial_in(info, 0x04);
                        serial_out(info, 0x04, 0x0e);
                        serial_out(info, 0x05, 4);
                        serial_out(info, 0x05,
                                scratch | 0x01 | 0x02);
                }
                _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
                do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((1)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
                if (tty_hung_up_p(filp) ||
                    !(info->flags & 0x80000000)) {
                        if (info->flags & 0x0001)
                                retval = -11;
                        else
                                retval = -512;
                        break;
                }
                serial_out(info, 0x04, 0x13);  /* XXXX Race reported by Engler */
                if (serial_in(info, 0x05) & 0x80)
                        do_clocal = 1;
                if (!(info->flags & 0x40000000) &&
                    !(info->flags & 0x08000000) &&
                    (do_clocal))
                        break;
                if (signal_pending(get_current())) {
                        retval = -512;
                        break;
                }
                schedule();
        }
        do { ((__typeof__(*(&get_current()->state)))__xchg((unsigned long)((0)),(&get_current()->state),sizeof(*(&get_current()->state)))); } while (0);
        remove_wait_queue(&info->open_wait, &wait);
        if (!tty_hung_up_p(filp))
                info->count++;
        info->blocked_open--;
        if (retval)
                return retval;
        info->flags |= 0x20000000;
        return 0;
}
static int esp_open(struct tty_struct *tty, struct file * filp)
{
        struct esp_struct *info;
        int retval, line;
        line = ((tty->device).value & 0xff) - tty->driver.minor_start;
        if ((line < 0) || (line >= 64))
                return -19;
        info = ports;
        while (info && (info->line != line))
                info = info->next_port;
        if (!info) {
                serial_paranoia_check(info, tty->device, "esp_open");
                return -19;
        }
        _MOD_INC_USE_COUNT((&__this_module));
        info->count++;
        tty->driver_data = info;
        info->tty = tty;
        if (!tmp_buf) {
                tmp_buf = (unsigned char *) get_zeroed_page((0x10 | 0x40 | 0x80));
                if (!tmp_buf)
                        return -12;
        }
        retval = startup(info);
        if (retval)
                return retval;
        retval = block_til_ready(tty, filp, info);
        if (retval) {
                return retval;
        }
        if ((info->count == 1) && (info->flags & 0x0008)) {
                if (tty->driver.subtype == 1)
                        *tty->termios = info->normal_termios;
                else
                        *tty->termios = info->callout_termios;
                change_speed(info);
        }
        info->session = get_current()->session;
        info->pgrp = get_current()->pgrp;
        return 0;
}
static inline void show_serial_version(void)
{
        printk("<6>" "%s version %s (DMA %u)\n",
                serial_name, serial_version, dma);
}
static inline int autoconfig(struct esp_struct * info, int *region_start)
{
        int port_detected = 0;
        unsigned long flags;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): ); _spin_lock(&cli_lock);;
        if (!__check_region(&ioport_resource, (info->port), (8)) &&
            serial_in(info, 0x00) == 0xf3) {
                serial_out(info, 0x04, 0x00);
                serial_out(info, 0x04, 0x01);
                if ((serial_in(info, 0x05) & 0x70) == 0x20) {
                        port_detected = 1;
                        if (!(info->irq)) {
                                serial_out(info, 0x04, 0x02);
                                if (serial_in(info, 0x04) & 0x01)
                                        info->irq = 3;
                                else
                                        info->irq = 4;
                        }
                        if (ports && (ports->port == (info->port - 8))) {
                                __release_region(&ioport_resource, (*region_start), (info->port - *region_start));
                        } else
                                *region_start = info->port;
                        if (!__request_region(&ioport_resource, (*region_start), (info->port - *region_start + 8), ("esp serial")))
                        {
                                _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
                                return -5;
                        }
                        esp_basic_init(info);
                        serial_out(info, 0x04, 0x0e);
                        serial_out(info, 0x05, 4);
                        serial_out(info, 0x05, 0x00);
                }
        }
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        return (port_detected);
}
int __attribute__ ((__section__ (".init.text"))) espserial_init(void)
{
        int i, offset;
        int region_start;
        struct esp_struct * info;
        struct esp_struct *last_primary = 0;
        int esp[] = {0x100,0x140,0x180,0x200,0x240,0x280,0x300,0x380};
        init_bh(1, do_serial_bh);
        for (i = 0; i < 8; i++) {
                if (irq[i] != 0) {
                        if ((irq[i] < 2) || (irq[i] > 15) || (irq[i] == 6) ||
                            (irq[i] == 8) || (irq[i] == 13))
                                irq[i] = 0;
                        else if (irq[i] == 2)
                                irq[i] = 9;
                }
        }
        if ((dma != 1) && (dma != 3))
                dma = 0;
        if ((rx_trigger < 1) || (rx_trigger > 1023))
                rx_trigger = 768;
        if ((tx_trigger < 1) || (tx_trigger > 1023))
                tx_trigger = 768;
        if ((flow_off < 1) || (flow_off > 1023))
                flow_off = 1016;
        if ((flow_on < 1) || (flow_on > 1023))
                flow_on = 944;
        if ((rx_timeout < 0) || (rx_timeout > 255))
                rx_timeout = 128;
        if (flow_on >= flow_off)
                flow_on = flow_off - 1;
        show_serial_version();
        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(struct tty_driver))) ? __constant_c_and_count_memset(((&esp_driver)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct tty_driver)))) : __constant_c_memset(((&esp_driver)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct tty_driver))))) : (__builtin_constant_p((sizeof(struct tty_driver))) ? __memset_generic((((&esp_driver))),(((0))),(((sizeof(struct tty_driver))))) : __memset_generic(((&esp_driver)),((0)),((sizeof(struct tty_driver))))));
        esp_driver.magic = 0x5402;
        esp_driver.name = "ttyP";
        esp_driver.major = 57;
        esp_driver.minor_start = 0;
        esp_driver.num = 64;
        esp_driver.type = 0x0003;
        esp_driver.subtype = 1;
        esp_driver.init_termios = tty_std_termios;
        esp_driver.init_termios.c_cflag =
                0000015 | 0000060 | 0000200 | 0002000 | 0004000;
        esp_driver.flags = 0x0004;
        esp_driver.refcount = &serial_refcount;
        esp_driver.table = serial_table;
        esp_driver.termios = serial_termios;
        esp_driver.termios_locked = serial_termios_locked;
        esp_driver.open = esp_open;
        esp_driver.close = rs_close;
        esp_driver.write = rs_write;
        esp_driver.put_char = rs_put_char;
        esp_driver.flush_chars = rs_flush_chars;
        esp_driver.write_room = rs_write_room;
        esp_driver.chars_in_buffer = rs_chars_in_buffer;
        esp_driver.flush_buffer = rs_flush_buffer;
        esp_driver.ioctl = rs_ioctl;
        esp_driver.throttle = rs_throttle;
        esp_driver.unthrottle = rs_unthrottle;
        esp_driver.set_termios = rs_set_termios;
        esp_driver.stop = rs_stop;
        esp_driver.start = rs_start;
        esp_driver.hangup = esp_hangup;
        esp_driver.break_ctl = esp_break;
        esp_driver.wait_until_sent = rs_wait_until_sent;
        esp_callout_driver = esp_driver;
        esp_callout_driver.name = "cup";
        esp_callout_driver.major = 58;
        esp_callout_driver.subtype = 2;
        if (tty_register_driver(&esp_driver))
        {
                printk("<3>" "Couldn't register esp serial driver");
                return 1;
        }
        if (tty_register_driver(&esp_callout_driver))
        {
                printk("<3>" "Couldn't register esp callout driver");
                tty_unregister_driver(&esp_driver);
                return 1;
        }
        info = kmalloc(sizeof(struct esp_struct), (0x10 | 0x40 | 0x80));
        if (!info)
        {
                printk("<3>" "Couldn't allocate memory for esp serial device information\n");
                tty_unregister_driver(&esp_driver);
                tty_unregister_driver(&esp_callout_driver);
                return 1;
        }
        (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(struct esp_struct))) ? __constant_c_and_count_memset((((void *)info)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct esp_struct)))) : __constant_c_memset((((void *)info)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct esp_struct))))) : (__builtin_constant_p((sizeof(struct esp_struct))) ? __memset_generic(((((void *)info))),(((0))),(((sizeof(struct esp_struct))))) : __memset_generic((((void *)info)),((0)),((sizeof(struct esp_struct))))));
        info->config.rx_trigger = rx_trigger;
        info->config.tx_trigger = tx_trigger;
        i = 0;
        offset = 0;
        do {
                info->port = esp[i] + offset;
                info->irq = irq[i];
                info->line = (i * 8) + (offset / 8);
                if (!autoconfig(info, &region_start)) {
                        i++;
                        offset = 0;
                        continue;
                }
                info->custom_divisor = (divisor[i] >> (offset / 2)) & 0xf;
                info->flags = (0x10000000 | 0x0040);
                if (info->custom_divisor)
                        info->flags |= 0x0030;
                info->magic = 0x53ee;
                info->close_delay = 5*1000/10;
                info->closing_wait = 30*1000;
                info->tqueue.data = info;
                info->tqueue_hangup.data = info;
                info->callout_termios = esp_callout_driver.init_termios;
                info->normal_termios = esp_driver.init_termios;
                info->config.rx_timeout = rx_timeout;
                info->config.flow_on = flow_on;
                info->config.flow_off = flow_off;
                info->config.pio_threshold = pio_threshold;
                info->next_port = ports;
                init_waitqueue_head(&info->open_wait);
                init_waitqueue_head(&info->close_wait);
                init_waitqueue_head(&info->delta_msr_wait);
                init_waitqueue_head(&info->break_wait);
                ports = info;
                printk("<6>" "ttyP%d at 0x%04x (irq = %d) is an ESP ",
                        info->line, info->port, info->irq);
                if (info->line % 8) {
                        printk("secondary port\n");
                        info->stat_flags |= 0x08;
                        if (last_primary)
                                last_primary->stat_flags |= 0x08;
                } else {
                        printk("primary port\n");
                        last_primary = info;
                        irq[i] = info->irq;
                }
                if (!dma)
                        info->stat_flags |= 0x08;
                info = kmalloc(sizeof(struct esp_struct), (0x10 | 0x40 | 0x80));
                if (!info)
                {
                        printk("<3>" "Couldn't allocate memory for esp serial device information\n");
                        return 0;
                }
                (__builtin_constant_p(0) ? (__builtin_constant_p((sizeof(struct esp_struct))) ? __constant_c_and_count_memset((((void *)info)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct esp_struct)))) : __constant_c_memset((((void *)info)),((0x01010101UL*(unsigned char)(0))),((sizeof(struct esp_struct))))) : (__builtin_constant_p((sizeof(struct esp_struct))) ? __memset_generic(((((void *)info))),(((0))),(((sizeof(struct esp_struct))))) : __memset_generic((((void *)info)),((0)),((sizeof(struct esp_struct))))));
                info->config.rx_trigger = rx_trigger;
                info->config.tx_trigger = tx_trigger;
                if (offset == 56) {
                        i++;
                        offset = 0;
                } else {
                        offset += 8;
                }
        } while (i < 8);
        kfree(info);
        return 0;
}
static void __attribute__ ((__section__(".exit.text"))) espserial_exit(void)
{
        unsigned long flags;
        int e1, e2;
        unsigned int region_start, region_end;
        struct esp_struct *temp_async;
        struct esp_pio_buffer *pio_buf;
        __asm__ __volatile__("pushfl ; popl %0":"=g" (flags): );
        _spin_lock(&cli_lock);;
        remove_bh(1);
        if ((e1 = tty_unregister_driver(&esp_driver)))
                printk("SERIAL: failed to unregister serial driver (%d)\n",
                       e1);
        if ((e2 = tty_unregister_driver(&esp_callout_driver)))
                printk("SERIAL: failed to unregister callout driver (%d)\n",
                       e2);
        _spin_unlock(&cli_lock);; __asm__ __volatile__("pushl %0 ; popfl": :"g" (flags):"memory", "cc");
        while (ports) {
                if (ports->port) {
                        region_start = region_end = ports->port;
                        temp_async = ports;
                        while (temp_async) {
                                if ((region_start - temp_async->port) == 8) {
                                        region_start = temp_async->port;
                                        temp_async->port = 0;
                                        temp_async = ports;
                                } else if ((temp_async->port - region_end)
                                           == 8) {
                                        region_end = temp_async->port;
                                        temp_async->port = 0;
                                        temp_async = ports;
                                } else
                                        temp_async = temp_async->next_port;
                        }
                        __release_region(&ioport_resource, (region_start), (region_end - region_start + 8));
                }
                temp_async = ports->next_port;
                kfree(ports);
                ports = temp_async;
        }
        if (dma_buffer)
                free_pages((unsigned long)dma_buffer,
                        get_order(1024));
        if (tmp_buf)
                free_pages(((unsigned long)tmp_buf),0);
        while (free_pio_buf) {
                pio_buf = free_pio_buf->next;
                kfree(free_pio_buf);
                free_pio_buf = pio_buf;
        }
}
static inline initcall_t __inittest(void) { return espserial_init; } int init_module(void) __attribute__((alias("espserial_init")));;
static inline exitcall_t __exittest(void) { return espserial_exit; } void cleanup_module(void) __attribute__((alias("espserial_exit")));;


struct tty_struct tty;
struct file filp;



void * f(void *ign) {
  esp_open(&tty, &filp);
  return 0;
}


typedef int pthread_t;
typedef int pthread_attr_t;
int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                   void * (*start_routine) (void*), void *arg);


int main() {
  pthread_t t;
  spin_lock_init(&cli_lock);
  /* moved init/exit from f above */
  espserial_init();
  pthread_create(&t, (void*)0, f, (void*)0);
  pthread_create(&t, (void*)0, f, (void*)0);
  espserial_exit();

  return 0;
}
