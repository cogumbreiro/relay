#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

pthread_mutex_t m1;
int *g_int;

/******** Simple test w/ no violation *********/

void func0 (void *x) {
  int ignore;
  ignore = 10;
  pthread_mutex_lock(&m1);
  if (g_int != NULL) {
    *g_int = 5;
    (*g_int)++;
    ignore = 10;
    (*g_int)++;
    (*g_int)++;
  } else {
    g_int = (int *) malloc(sizeof(int));
    *g_int = 0;
  }
  (*g_int)++;
  pthread_mutex_unlock(&m1);
}

void func1 (void *x) {
  pthread_mutex_lock (&m1);

  g_int = NULL;

  pthread_mutex_unlock (&m1);
}

int main() {
  pthread_t thr0, thr1;

  pthread_create (&thr0, (void *)0, &func0, (void *)0);
  pthread_create (&thr1, (void *)0, &func1, (void *)0);
  return 0;
}
