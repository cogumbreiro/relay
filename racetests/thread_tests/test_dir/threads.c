#include "lock.h"
#include "threads2.h"
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <pthread.h>

#define MAX 10

typedef struct _dl {
  int x;
  spinlock_t *l;
} dl;

dl *g_dl_p;


typedef struct _list {
  //void *obj;
  dl *obj;
  struct _list *next;
} list;


void *mungeMax (void *mmArg) {

  dl *data_lock = (dl*) mmArg;

  _spin_lock (data_lock->l);

  if(data_lock->x >= MAX) {
    _spin_unlock (data_lock->l);
    return 0;
  }
  
  data_lock->x = data_lock->x + 1;

  _spin_unlock (data_lock->l);

  return 0;

}

void *lockWithGoto(void *obj) {
  
  dl *data_lock = (dl*) obj;

  _spin_lock (data_lock->l);

  if(data_lock->x >= MAX) {
    _spin_unlock (data_lock->l);
    goto _end;
  }
  
  data_lock->x = data_lock->x + 1;
  
  _spin_unlock (data_lock->l);
  
 _end:
  return 0;
}

void *munge (void *mArg) {

  dl *dl = (dl*) mArg;

  _spin_lock (dl->l);
  
  dl->x = dl->x + 1;

  _spin_unlock (dl->l);


  return 0;
}


void *racyMunge (void *mArg) {

  dl *dl = (dl*) mArg;

  dl->x = dl->x + 1;

  return 0;
}



void makeRunThreads (dl *datlock) {

  pthread_t temp_t;

  pthread_create (&temp_t, (void *)0, &munge, (void*) datlock);
  pthread_create (&temp_t, (void *)0, &mungeMax, (void*) datlock);

}

void makeRunRacy (dl *dlracy) {

  pthread_t temp_t;

  pthread_create (&temp_t, (void *)0, &racyMunge, (void*) dlracy);

}

void makeAlias (dl* arg) {
  g_dl_p = arg;
}

void *globMunge (void *nuthin) {
  g_dl_p->x = g_dl_p->x + 1;

  return 0;
}



void loop(list *list) {
  while (list) {
    _spin_lock (((dl*)(list->obj))->l);
    (((dl*)(list->obj))->x) = (((dl*)(list->obj))->x) + 1;
    _spin_unlock (((dl*)(list->obj))->l);
    list = list->next;
  }
}



void *loopAndRecurse(void *listArg) {
  list *l = (list *) listArg;

  while (l) {
    loop (l);
    loopAndRecurse(l->next);
    l = l->next;
  }
  
  return 0;
}



void *loopWithGoto (void *lis) {

  list *nod;

  nod = lis;

  
  while (nod && 
         (((dl*)nod->obj)->x != 10 )) { // racy read..
    nod = nod->next;
  }
  
  _spin_lock (((dl*)nod->obj)->l);
  if(((dl*)nod->obj)->x >= MAX) {
    _spin_unlock (((dl*)nod->obj)->l);
    goto _end;
  }
  
  ((dl*)nod->obj)->x = ((dl*)nod->obj)->x + 1;
  
  _spin_unlock (((dl*)nod->obj)->l);
  
 _end:
  return 0;
}



list *makeObjList(int size) {
  list *result = 0;
  list *prev = 0;
  dl   *obj = 0;
  
  while (size > 0) {
    result = (list *) malloc (sizeof(list));
    obj = (dl*) malloc (sizeof(dl));
    obj->l = (spinlock_t*) malloc (sizeof(spinlock_t));
    obj->x = 0;
    result->obj = (void *)obj;
    result->next = prev;

    prev = result;
  }

  return result;
}

dl *getObj(list *node) {

  return (dl*)node->obj;
}

spinlock_t *getLock(dl *o) {
  return o->l;
}

void racyGetterUse(list *l) {

  dl *curObj;
  spinlock_t *tempLock;


  curObj = getObj (l);
  tempLock = getLock (curObj);
  
  _spin_lock (tempLock);
  curObj->x = curObj->x + 13;
  _spin_unlock (tempLock);

}

///// SIMPLIFICATION OF REISERFS JOURNAL.C /////


typedef struct _dlist {
  struct _dlist *next, *prev;
} dlist;

typedef struct _dev_st {
  unsigned long junk;
  unsigned long len;
  spinlock_t *commit_lock;
  dlist dev_list;
  dlist junk_list;
} dev_st;


/* Given a pointer to list that is known to be part of a dev_st, 
   find the pointer to the containing dev_st */
#define container_of(ptr, type, member) ({ const typeof( ((type *)0)->member ) *__mptr = (ptr); (type *)( (char *)__mptr - offsetof(type,member) );})
  
#define DEV_OF_LIST (l) (container_of(l, dev_st, dev_list))

int accDListWithPtrA (dev_st *dev) {
  dev_st *otherDev;
  dev_st *firstDev;
  dlist *entry;

  firstDev = dev;
  entry = dev->dev_list.prev;

  //otherDev = DEV_OF_LIST(entry);
  //NOT DONE YET...
  return 0;
}



int main(int argc, char *argv[]) {

  pthread_t temp_t;

  list* list1;
  list* list2;
  list* list3;

  dl *data_and_lock = (dl *) malloc (sizeof(dl));
  dl *data_and_lock2 = (dl *) malloc (sizeof(dl));
  dl *data_and_lock3 = (dl *) malloc (sizeof(dl));
  dl *data_and_lock4 = (dl *) malloc (sizeof(dl));
  dl *data_and_lock5 = (dl *) malloc (sizeof(dl));
  dl *data_and_lock6 = (dl *) malloc (sizeof(dl));
 
  
  data_and_lock->l = (spinlock_t *) malloc (sizeof(spinlock_t));
  data_and_lock2->l = (spinlock_t *) malloc (sizeof(spinlock_t));
  data_and_lock3->l = (spinlock_t *) malloc (sizeof(spinlock_t));
  data_and_lock4->l = (spinlock_t *) malloc (sizeof(spinlock_t));
  data_and_lock5->l = (spinlock_t *) malloc (sizeof(spinlock_t));
  data_and_lock6->l = (spinlock_t *) malloc (sizeof(spinlock_t));


  /* Make two threads that DON'T RACE */
  makeRunThreads (data_and_lock);

  /* Make one thread that races, but w/ no-one else, because it 
     Accesses different data! */
  pthread_create (&temp_t, (void *)0, &racyMunge, (void*) data_and_lock2);

  /* Try the same, but w/ one level of function call indirection */
  makeRunRacy (data_and_lock3);

  /* Now, do the same thing as before, but make the data shared
     so that there REALLY IS A RACE! */
  makeRunThreads (data_and_lock4);
  pthread_create (&temp_t, (void *)0, &racyMunge, (void*) data_and_lock4);

  /* Now, try causing a RACE by having one thread access a global
     that aliases w/ an arg */
  // makeAlias (data_and_lock5);
  g_dl_p = data_and_lock5;
  pthread_create (&temp_t, (void *)0, &munge, (void*) data_and_lock5);
  pthread_create (&temp_t, (void *)0, &globMunge, (void*)0);


  /* Also, see if it can detect a race caused by the parent thread */
  data_and_lock->x = data_and_lock->x + 1;
  

  /* test looping & recursion (safe accesses)*/
  list1 = makeObjList (10);
  
  pthread_create (&temp_t, (void *)0, &loopAndRecurse, (void*) list1);
  pthread_create (&temp_t, (void *)0, &loopAndRecurse, (void*) list1);

  /* try functions w/ gotos */

  pthread_create (&temp_t, (void *)0, &lockWithGoto, (void*) data_and_lock6);
  pthread_create (&temp_t, (void *)0, &lockWithGoto, (void*) data_and_lock6);

  list2 = makeObjList (5);

  pthread_create (&temp_t, (void *)0, &loopWithGoto, (void*) list2);
  pthread_create (&temp_t, (void *)0, &loopWithGoto, (void*) list2);


  /* test usage of getters */
  list3 = makeObjList (14);

  pthread_create (&temp_t, (void *)0, &racyGetterUse, (void*) list3);
  pthread_create (&temp_t, (void *)0, &racyGetterUse, (void*) list3);
  loopWithGoto(list3);


  t2_main (argc, argv);

  return 0;
}
