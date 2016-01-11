#include "lock.h"
#include <stdlib.h>

int *glob;


int main (int argc, char *argv[]) {
  spinlock_t l = {0};
  glob = (int *)malloc(sizeof(int));

  spin_lock(&l);
  *glob = 0;
  spin_unlock(&l);

  
  return 0;
}
