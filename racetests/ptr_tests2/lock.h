#ifndef __LOCK_H_24458358__
#define __LOCK_H_24458358__


typedef enum { FAIL, SUCCESS } l_outcome;

typedef struct _spinlock_t {
  int locked;
} spinlock_t;

void _spin_lock(spinlock_t *lock);
  
int _spin_trylock(spinlock_t *lock);

void _spin_unlock(spinlock_t *lock);



#endif
