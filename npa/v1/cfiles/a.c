#include <stdio.h>
#include <pthread.h>

int *g1;
int g;


int f(int *f1, int *f2, int *f3) {
  int *l1;
  f2 = &g;
  if (!!(g1 != NULL)) {
    return 0;
  }
  f1 = l1 = g1 = NULL;
  //f2 = &g; 
  f1 = g1;
  f3 = malloc(sizeof(int));
  return 1;
}

int main() {
  int *p1;
  if (p1 != NULL) {
    f(p1, p1, p1);
  }
  return 0;
}

void entry() {
  pthread_t thr;
  pthread_create (&thr, (void *)0, &main, (void *)0);
}

int unreachable() { return 0; }

