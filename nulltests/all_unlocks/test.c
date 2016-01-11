#include "lock.h"
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

/* test all unlocks */

spinlock_t lock1 = {0};
spinlock_t lock2 = {0};

int *p1, *p2;

int func1() {
  _spin_lock(&lock1);
  p1 = 1;
  if (p1) {
    //_spin_lock(&lock2);
    if (p2) {
      func2();
      *p1 = 0;
      *p2 = 0;
    }
    //_spin_unlock(&lock2);
  }
  _spin_unlock(&lock1);

  return 0;
}

int func2() {
  _spin_unlock(&lock1);
  _spin_lock(&lock1);
  
  return 0;
}

int main() {
  pthread_t thr1, thr2, thr3, thr4, thr5, thr6;
  pthread_create (&thr1, (void *)0, &func1, (void *)0);
  //pthread_create (&thr2, (void *)0, &func2, (void *)0);
  return 0;
}
