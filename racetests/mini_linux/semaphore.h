#ifndef _SEMAPHORE___FOO_BAR_
#define _SEMAPHORE___FOO_BAR_


#include "types.h"
#include "atomic.h"

struct semaphore {
  atomic_t count;
  int sleepers;
  //	wait_queue_head_t wait;
};

static /* inline */ void down(struct semaphore * sem)
{
  
  //	might_sleep();
  //	__asm__ __volatile__(
  //		"# atomic down operation\n\t"
  //		LOCK "decl %0\n\t"     /* --sem->count */
  //		"js 2f\n"
  //		"1:\n"
  //		LOCK_SECTION_START("")
  //		"2:\tlea %0,%%eax\n\t"
  //		"call __down_failed\n\t"
  //		"jmp 1b\n"
  //		LOCK_SECTION_END
  //		:"=m" (sem->count)
  //		:
  //		:"memory","ax");
}

/*
 * Interruptible try to acquire a semaphore.  If we obtained
 * it, return zero.  If we were interrupted, returns -EINTR
 */
static /* inline */ int down_interruptible(struct semaphore * sem)
{
	int result;

    //	might_sleep();
    //	__asm__ __volatile__(
	//	"# atomic interruptible down operation\n\t"
	//	LOCK "decl %1\n\t"     /* --sem->count */
	//	"js 2f\n\t"
	//	"xorl %0,%0\n"
	//	"1:\n"
	//	LOCK_SECTION_START("")
	//	"2:\tlea %1,%%eax\n\t"
    //  	"call __down_failed_interruptible\n\t"
	//	"jmp 1b\n"
	//	LOCK_SECTION_END
	//	:"=a" (result), "=m" (sem->count)
	//	:
	//	:"memory");
    result = 0;
	return result;
}

/*
 * Non-blockingly attempt to down() a semaphore.
 * Returns zero if we acquired it
 */
static /* inline */ int down_trylock(struct semaphore * sem)
{
	int result;

    //	__asm__ __volatile__(
	//	"# atomic interruptible down operation\n\t"
	//	LOCK "decl %1\n\t"     /* --sem->count */
	//	"js 2f\n\t"
	//	"xorl %0,%0\n"
	//	"1:\n"
	//	LOCK_SECTION_START("")
	//	"2:\tlea %1,%%eax\n\t"
	//	"call __down_failed_trylock\n\t"
	//	"jmp 1b\n"
	//	LOCK_SECTION_END
	//	:"=a" (result), "=m" (sem->count)
	//	:
	//	:"memory");
    result = 0;
	return result;
}

/*
 * Note! This is subtle. We jump to wake people up only if
 * the semaphore was negative (== somebody was waiting on it).
 * The default case (no contention) will result in NO
 * jumps for both down() and up().
 */
static /* inline */ void up(struct semaphore * sem)
{
  //	__asm__ __volatile__(
  //		"# atomic up operation\n\t"
  //		LOCK "incl %0\n\t"     /* ++sem->count */
  //		"jle 2f\n"
  //		"1:\n"
  //		LOCK_SECTION_START("")
  //		"2:\tlea %0,%%eax\n\t"
  //		"call __up_wakeup\n\t"
  //		"jmp 1b\n"
  //		LOCK_SECTION_END
  //		".subsection 0\n"
  //		:"=m" (sem->count)
  //		:
  //		:"memory","ax");
}



#endif
