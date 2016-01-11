#ifndef _SPINLOCK_TYPES___FOO_BAR_
#define _SPINLOCK_TYPES___FOO_BAR_

#include "spinlock_types_up.h"



typedef struct {
  raw_rwlock_t raw_lock;
  unsigned int break_lock;
  unsigned int magic, owner_cpu;
  void *owner;
} rwlock_t;

typedef struct {
	volatile unsigned int slock;
} raw_spinlock_t;

typedef struct {
	raw_spinlock_t raw_lock;
	unsigned int magic, owner_cpu;
	void *owner;
} spinlock_t;

#define __RAW_SPIN_LOCK_UNLOCKED	{ 1 }
#define SPINLOCK_MAGIC		0xdead4ead
#define SPINLOCK_OWNER_INIT	((void *)-1L)
# define SPIN_LOCK_UNLOCKED						\
	(spinlock_t)	{	.raw_lock = __RAW_SPIN_LOCK_UNLOCKED,	\
				.magic = SPINLOCK_MAGIC,		\
				.owner = SPINLOCK_OWNER_INIT,		\
				.owner_cpu = -1 }


#define DEFINE_SPINLOCK(x)	spinlock_t x = SPIN_LOCK_UNLOCKED



#define RWLOCK_MAGIC            0xdeaf1eed

#define RW_LOCK_UNLOCKED \
  (rwlock_t)      {       .raw_lock = __RAW_RW_LOCK_UNLOCKED }

#define DEFINE_RWLOCK(x)        rwlock_t x = RW_LOCK_UNLOCKED


////

#define __raw_spin_is_locked(x)		((x)->slock == 0)

static inline void __raw_spin_lock(raw_spinlock_t *lock)
{
	lock->slock = 0;
}

static inline void __raw_spin_unlock(raw_spinlock_t *lock)
{
	lock->slock = 1;
}

void _spin_lock(spinlock_t *lock)
{
  //	preempt_disable();
	_raw_spin_lock(lock);
}

void _spin_unlock(spinlock_t *lock)
{
  	_raw_spin_unlock(lock);
	//preempt_enable();
}


#define spin_lock(lock)			_spin_lock(lock)

#define spin_unlock(lock)		_spin_unlock(lock)




#endif
