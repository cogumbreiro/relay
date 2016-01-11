#ifndef __SIMPLE_LOCK_3244442__
#define __SIMPLE_LOCK_3244442__

#include "lock.h"

int transfer(int );

void deposit1(int );

void deposit2(int );

int withdraw1(int );

int withdraw2(int );

int safeDeposit(spinlock_t *l, int dnum, int a);

void condDeposit1(int cond, int a);

#endif
