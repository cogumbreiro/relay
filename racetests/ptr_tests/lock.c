#include "lock.h"
#include <stdio.h>


void _spin_lock(spinlock_t *lock) {
  lock->locked = 1;
}

/** return 0 on success, non-zero on failure */
int _spin_trylock(spinlock_t *lock) {
  if(lock->locked == 0){
    lock->locked = 1;
    return 0;
  } else {
    return 1;
  }
}

void _spin_unlock(spinlock_t *lock) {
  if(lock->locked == 1){
    lock->locked = 0;
    
  } else {
    fprintf(stderr, "Double unlock!\n");
    
  }
}

