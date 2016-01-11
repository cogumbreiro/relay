#include "types.h"
#include "page.h"


//from include/asm/processor.h
#define NCAPINTS	7	/* N 32-bit words worth of info */
#define TASK_SIZE	(PAGE_OFFSET)
struct cpuinfo_x86 {
	__u8	x86;		/* CPU family */
	__u8	x86_vendor;	/* CPU vendor */
	__u8	x86_model;
	__u8	x86_mask;
	char	wp_works_ok;	/* It doesn't on 386's */
	char	hlt_works_ok;	/* Problems on some 486Dx4's and old 386's */
	char	hard_math;
	char	rfu;
       	int	cpuid_level;	/* Maximum supported CPUID level, -1=no CPUID */
	unsigned long	x86_capability[NCAPINTS];
	char	x86_vendor_id[16];
	char	x86_model_id[64];
	int 	x86_cache_size;  /* in KB - valid for CPUS which support this
				    call  */
	int 	x86_cache_alignment;	/* In bytes */
	int	fdiv_bug;
	int	f00f_bug;
	int	coma_bug;
	unsigned long loops_per_jiffy;
	unsigned char x86_max_cores;	/* cpuid returned max cores value */
	unsigned char booted_cores;	/* number of cores as seen by OS */
	unsigned char apicid;
}; //__attribute__((__aligned__(SMP_CACHE_BYTES)));
extern struct cpuinfo_x86 boot_cpu_data;


//from include/asm/pgtable-3level-defs.h (alternative is 2level)
#define PGDIR_SHIFT	30
#define PTRS_PER_PGD	4


//from include/asm/pgtable.h
#define PGDIR_SIZE	(1UL << PGDIR_SHIFT)
#define USER_PTRS_PER_PGD	(TASK_SIZE/PGDIR_SIZE)

//from include/asm/pgtable-3level.h
#define set_pud(pudptr,pudval) \
		(*(pudptr) = (pudval))


//from include/asm-generic/pgtable-nopud.h
typedef struct { pgd_t pgd; } pud_t;
#define set_pgd(pgdptr, pgdval)			set_pud((pud_t *)(pgdptr), (pud_t) { pgdval })


//from include/asm/bitops.h
/**
 * test_bit - Determine whether a bit is set
 * @nr: bit number to test
 * @addr: Address to start counting from
 */
static int test_bit(int nr, const volatile void * addr);



//from include/asm/cpufeature.h
#define boot_cpu_has(bit)	test_bit(bit, boot_cpu_data.x86_capability)
#define X86_FEATURE_PGE		(0*32+13) /* Page Global Enable */
#define cpu_has_pge		boot_cpu_has(X86_FEATURE_PGE)


//from include/asm/tlbflush.h
#define __flush_tlb()							\
	do {								\
		unsigned int tmpreg;					\
									\
		__asm__ __volatile__(					\
			"movl %%cr3, %0;              \n"		\
			"movl %0, %%cr3;  # flush TLB \n"		\
			: "=r" (tmpreg)					\
			:: "memory");					\
	} while (0)

/*
 * Global pages have to be flushed a bit differently. Not a real
 * performance problem because this does not happen often.
 */
#define __flush_tlb_global()						\
	do {								\
		unsigned int tmpreg, cr4, cr4_orig;			\
									\
		__asm__ __volatile__(					\
			"movl %%cr4, %2;  # turn off PGE     \n"	\
			"movl %2, %1;                        \n"	\
			"andl %3, %1;                        \n"	\
			"movl %1, %%cr4;                     \n"	\
			"movl %%cr3, %0;                     \n"	\
			"movl %0, %%cr3;  # flush TLB        \n"	\
			"movl %2, %%cr4;  # turn PGE back on \n"	\
			: "=&r" (tmpreg), "=&r" (cr4), "=&r" (cr4_orig)	\
			: "i" (~X86_CR4_PGE)				\
			: "memory");					\
	} while (0)

extern unsigned long pgkern_mask;

# define __flush_tlb_all()						\
	do {								\
		if (cpu_has_pge)					\
			__flush_tlb_global();				\
		else							\
			__flush_tlb();					\
	} while (0)


//from arch/i386/kernel/acpi/sleep.c
void init_low_mapping(pgd_t * pgd, int pgd_limit)
{
	int pgd_ofs = 0;

	while ((pgd_ofs < pgd_limit)
	       && (pgd_ofs + USER_PTRS_PER_PGD < PTRS_PER_PGD)) {
		set_pgd(pgd, *(pgd + USER_PTRS_PER_PGD));
		pgd_ofs++, pgd++;
	}
	flush_tlb_all();
}
