#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

int *p1, *p2;

void func0 () {
  p1 = NULL;
  p2 = NULL;
  func1(p1);
  func2(&p2);
}

void func1 (int *f) {
  if (f != NULL) {
    *f = 5;
  }
}

void func2 (int **f) {
  if (*f != NULL) {
    **f = 5;
  }
}

int main() {
  pthread_t thr0, thr1, thr2;
  pthread_create (&thr0, (void *)0, &func0, (void *)0);
  //pthread_create (&thr1, (void *)0, &func1, (void *)0);
  //pthread_create (&thr2, (void *)0, &func2, (void *)0);
  return 0;
}
