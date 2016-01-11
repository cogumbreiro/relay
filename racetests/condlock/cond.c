#include "lock.h"
#include <stdio.h>
#include <stdlib.h>

//simple blood bank

int donor1_blood = 98;
int donor2_blood = 37;

spinlock_t gLock1 = {0};
spinlock_t gLock2 = {0};

typedef struct  {
  int fst;
  int snd;
} intpair;

/* conditionally safe deposit */
void condDeposit(void * arg) {
  intpair *p = (intpair *) arg;
  int cond = p->fst;
  int a = p->snd;
  int junk = 0;
  
  if(cond) {
    spin_lock(&gLock1);
    printf("Condition is true\n");
    junk++;
    donor1_blood++;
  }

  donor1_blood += a;
  
  if(cond) {
    spin_unlock(&gLock1);
    junk--;
    donor1_blood--;
  }
}


int main(int argc, char *argv[]) {
  intpair pr = { 1, 1 };
  pthread_create(NULL, NULL, &condDeposit, &pr);
  pthread_create(NULL, NULL, &condDeposit, &pr);

  return 0;
}

