#include "lock.h"
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <pthread.h>


typedef struct _s {

  int a;
  int b;
  int c;
  spinlock_t *protector;

} s;

void unlockedOnEntry (s* obj, int b) {
  
  if (b) {
    _spin_lock (&obj->protector);
    (obj->c)++;
    _spin_unlock (&obj->protector);
  }

}

void lockedOnEntry (s* obj, int b) {

  if (b) {
    _spin_unlock(&obj->protector);
    unlockedOnEntry(obj, b);
    _spin_lock(&obj->protector);
    (obj->b)++;
  }
  
  (obj->a)++;
  
  
  return;
}

int safeThread(void *arg) {
  s* obj = (s*)arg;

  _spin_lock (&obj->protector);
  lockedOnEntry(obj, 1);
  _spin_unlock (&obj->protector);

  return 0;
}


int unsafeThread(void *arg) {
  s* obj = (s*)arg;

  lockedOnEntry(obj, 0);

  return 0;
}


spinlock_t lForA;
spinlock_t l2ForA;


void getPtr (void **targ, void *src) {
  *targ = src;
}

void modAL(void *a) {
  int *pa;
  
  int **p;
  pa = (int*)a;
  p = (int**) malloc (sizeof(int*));
  _spin_lock (&lForA);
  
  getPtr (p, pa);
  (**p)++;

  _spin_unlock (&lForA);
  
  free(p);
  return;
}

void modAL2(void *a) {
  int *pa = (int*)a;
  _spin_lock (&l2ForA);
  (*pa)++;
  _spin_unlock (&l2ForA);

}

void make (int ***p1, int **p2) {
  *p1 = p2;
}


void branch(void *arg) {
  struct _s *old = arg;
  struct _s *noob;

  noob = old + 1;
  if (old->a > 0) {
    noob->b = 10;
  } else {
    noob = malloc(sizeof(struct _s));
    if (!noob)
      return;
    noob->b = 5;
  }

#define C(x) noob->x = old->x

  C(a);
  C(c);
  return;
}

void modU(void *a) {
  int *pa;
  
  int **p1;
  int *p2;
  pa = a;
  make (&p1, &p2);
  
  (*p1) = pa;
  (*p2)++;
  
  return;
}


void reachedByFP(void *a) {

  int c = 0;
  c++;
  
}

void reachedByName(void *b) {
  int d = 0;
  d++;
}

int t2_main(int argc, char *argv[]) {

  pthread_t temp_t;
  int A;
  void (*fp)(void *a);


  s* s1 = (s*) malloc (sizeof(s));
  s* s2 = (s*) malloc (sizeof(s));
  s* s3 = (s*) malloc (sizeof(s) * 3);

  /* Make threads the safely access the obj */
  pthread_create (&temp_t, (void *)0, &safeThread, (void*) s1);
  pthread_create (&temp_t, (void *)0, &safeThread, (void*) s1);

  /* Make threads where one doesn't safely access the obj */
  pthread_create (&temp_t, (void *)0, &unsafeThread, (void*) s2);
  pthread_create (&temp_t, (void *)0, &unsafeThread, (void*) s2);

  pthread_create (&temp_t, (void *)0, &branch, (void*) s3);

  pthread_create (&temp_t, (void *)0, &modAL, (void*) &A);
  pthread_create (&temp_t, (void *)0, &modAL2, (void*) &A);
  pthread_create (&temp_t, (void *)0, &modU, (void*) &A);

  fp = &reachedByFP;
  pthread_create (&temp_t, (void *)NULL, fp, (void*) NULL);
  pthread_create (&temp_t, (void *)NULL, reachedByName, (void*) NULL);
  

  return 0;
}
