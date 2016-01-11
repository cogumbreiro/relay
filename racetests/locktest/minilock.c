
#include <stdlib.h>
#include "lock.h"

typedef struct _list {
  int data;
  spinlock_t *lock;
  struct _list *next;
} list;


void miniMunge2(int *x, spinlock_t *l){
  int i, *p;
  i = 10;
  p = &i;
  (*p)++;          // symstate can do a strong update
  _spin_lock(l);
  (*x)++;          // symstate not doing a strong update, since x is
                   // classified as an ext ptr
  _spin_unlock(l);
}

void miniMunge(int *argX, spinlock_t *argLock){
  miniMunge2(argX, argLock);
  miniMunge2(argX, argLock);
}


void accList(void *arg) {
  list *l;
  int *px;
  spinlock_t *pl;

  l = (list *) arg; 

  while (l) {
    px = & (l->data);
    pl = l->lock;

    munge(px, pl);

    l = l->next;
  }
  
}


spinlock_t *makeLock (void ) {
  return (spinlock_t*) malloc (sizeof(spinlock_t));
}

list *makeList(int size) {
  list *result = 0;
  list *prev = 0;

  while (size > 0) {
    result = (list *) malloc (sizeof(list));
    result->lock = makeLock(); 
    result->data = 0;
    result->next = prev;

    prev = result;
  }

  return result;
}

int main(int argc, char *argv[]) {

  list *aList;

  aList = makeList (10);

  accList((void *) aList);
  accList((void *) aList);


  return 0;
}
