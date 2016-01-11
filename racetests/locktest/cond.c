#include "lock.h"
#include <stdio.h>
#include <stdlib.h>

//simple blood bank

int donor1_blood = 98;
int donor2_blood = 37;

spinlock_t gLock1 = {0};
spinlock_t gLock2 = {0};


/* conditionally safe deposit */
void condDeposit1(int cond, int a) {
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
  
  condDeposit1(1, 10);

  return 0;
}

