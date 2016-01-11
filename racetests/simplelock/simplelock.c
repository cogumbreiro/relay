#include "lock.h"
#include <stdio.h>
#include <stdlib.h>

//simple blood bank

int donor1_blood = 98;
int donor2_blood = 37;
int glob = 41;
int glob2 = 2;

int V1,V2,V3,V4;


spinlock_t gLock1 = {0};
spinlock_t gLock2 = {0};

spinlock_t gLockV1 = {0};
spinlock_t gLockV2 = {0};
spinlock_t gLockV3 = {0};
spinlock_t gLockV4 = {0};


void noCheckDep1(int a) {
  _spin_lock(&gLock1);
  donor1_blood += a;
  _spin_unlock(&gLock1);
}

void deposit1(int a) {
  int res;

  res = _spin_trylock(&gLock1);
  if(res == 0) {
    donor1_blood += a;
    _spin_unlock(&gLock1);
  }
}

void deposit2(int a) {
  int res;

  res = _spin_trylock(&gLock2);
  if(res == 0) {
    donor2_blood += a;
    _spin_unlock(&gLock2);
  }
}

int withdraw1(int a) {
  int res;

  res = _spin_trylock(&gLock1);
  if(res == 0) {
    donor1_blood -= a;
    _spin_unlock(&gLock1);
  }
  return donor1_blood;
}

int withdraw2(int a) {
  int res;

  res = _spin_trylock(&gLock2);
  if(res == 0) {
    donor2_blood -= a;
    _spin_unlock(&gLock2);
  }
  return donor2_blood;
}

int safeDeposit(spinlock_t *l, int dnum, int a) {
  int res;
  int succ = 0;

  res = _spin_trylock(l);
  if(res == 0) {
    switch (dnum) {
    case 1: donor1_blood += a;
      break;
    case 2: donor2_blood += a;
      break;
    }
    succ = 1;

  }


  if(res == 0) {
    donor1_blood += 1;
    _spin_unlock(l);
  }

  return succ;
}

/* conditionally safe deposit */
void condDeposit1(int cond, int a) {
  int junk = 0;
  
  if(cond) {
    _spin_lock(&gLock1);
    printf("Condition is true\n");
    donor1_blood += a;
    srand(junk);
  }

  junk++;
  
  if(cond) {
    _spin_unlock(&gLock1);
    junk--;
    srand(junk);
  }
}

void spinWithdraw1(int junk, int a) {
  int res = 1;
  int tries = 0;
  int i;
  int oldTries;

  while(res != 0) {
    //crap
    oldTries = tries;
    for(i = 0; i < oldTries; i++) {
      tries--;
    }
    tries = oldTries;
    res = _spin_trylock(&gLock1);
    tries++;
    oldTries = tries;
    for(i = 0; i < oldTries; i++) {
      tries--;
    }
    tries = oldTries;
  }

  donor1_blood -= a;

  _spin_unlock(&gLock1);
}

int transfer(int a) {
  int res;
  int succ = 0;

  res = _spin_trylock(&gLock1);
  if(res == 0) {
    res = _spin_trylock(&gLock2);
    if(res == 0) {

      if(donor1_blood >= a) {
        donor1_blood -= a;
        donor2_blood += a;
        succ = 1;
      }
      
      _spin_unlock(&gLock2);
    }
    _spin_unlock(&gLock1);
  }

  return succ;
}


int lockedOnEntry() {
  _spin_unlock(&gLock2);
  glob++;
  return glob;
  
}

void foo() {
  glob2++;
}

void bar() {
  V4++;
}


void munge(int *x, spinlock_t *l){
  munge2(x,l);
  munge2(x,l);
}

void munge2(int *x, spinlock_t *l){
  _spin_lock(l);
  *x++;
  _spin_unlock(l);
}


void recurses(int *x, spinlock_t *l){

  _spin_lock(l);
  *x--;
  _spin_unlock(l);
  if(*x > 0) {
    recurses(x, l);
  }
}

void multi_recurses1(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(glob2 > 0) {
    multi_recurses2(l);
  }

}

void multi_recurses2(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(glob2 > 0) {
    multi_recurses3(l);
  }

}

void multi_recurses3(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(glob2 > 0) {
    multi_recurses4(l);
    multi_recurses5(l,l);
  }

}

void multi_recurses4(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(glob2 > 0) {
    multi_recurses1(l);
  }

}

void multi_recurses5(spinlock_t *l1, spinlock_t *l2){
  
  _spin_lock(l2);
  bar();
  _spin_unlock(l2);
  if(glob2 > 0) {
    multi_recurses5(l2,l1);
  }

}




int main(int argc, char *argv[]) {
  
  int x = 10;
  int y = 11;
  deposit1(10);
  deposit2(11);

  transfer(100);

  safeDeposit(&gLock2, 2, 1337);

  condDeposit1(1, 10);

  _spin_lock(&gLock1);
  foo();
  _spin_lock(&gLock2);

  glob2++;
  lockedOnEntry();

  _spin_unlock(&gLock1);


  if(y < x)
    goto label1; 
  x++;
  y++;

  munge(&V1,&gLockV1);
  munge(&V2,&gLockV2);
  munge(&V3,&gLockV3);

  recurses(&V1,&gLockV1);

  multi_recurses1(&gLockV4);

 label0:
  y++;
  return 0;

 label1:
  x++;
  return 1;
  


}

