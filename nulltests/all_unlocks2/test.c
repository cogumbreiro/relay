#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

pthread_mutex_t m1;
pthread_mutex_t m2;
pthread_mutex_t m3;
int *g_int;

/******** Test AllUnlocks some more *********/

void foo () {
  pthread_mutex_unlock(&m2);

  //big wait so that thread 2 gets m2 and m3

  pthread_mutex_lock(&m3);
}

void func0 (void *x) {

  // if thread 1 gets m2 first
  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  if (g_int != NULL) {

    foo();

    // thread 1 continues after thread 2
    *g_int = 5;

    pthread_mutex_unlock(&m3);
  } else {
    pthread_mutex_unlock(&m2);
  }
  pthread_mutex_unlock(&m1);
}

void func1 (void *x) {
  pthread_mutex_lock (&m2);
  pthread_mutex_lock (&m3);

  g_int = NULL;

  pthread_mutex_unlock (&m3);
  pthread_mutex_unlock (&m2);

}

/*************** Test thread-local pruning ***************/

void func2 (void *x) {
  
  int **p;
  p = (int **) malloc (sizeof(int *));
  *p = (int *) malloc (sizeof(int));

  if (*p) {
    **p = 1;
    **p = 10;
  }

}

int main() {
  pthread_t thr0, thr1, thr2;

  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  pthread_mutex_lock(&m3);

  g_int = (int *)malloc(sizeof(int));

  pthread_mutex_unlock(&m1);
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m3);

  pthread_create (&thr0, (void *)0, &func0, (void *)0);
  pthread_create (&thr1, (void *)0, &func1, (void *)0);
  pthread_create (&thr2, (void *)0, &func2, (void *)0); 
  pthread_create (&thr2, (void *)0, &func2, (void *)0); 
  return 0;
}
