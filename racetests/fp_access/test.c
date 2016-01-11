#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>

int x = 0, y = 0;
void write(int *p) { 
  *p = 1; 
}

void read(int *p) { 
  printf("%d\n", *p); 
}

void call_fp(void (*fp)(int *), int *x) { 
  (*fp)(x); 
  return;
}

void *thread_1(void *ignore) {
  call_fp(&read, &x);
}

int main() {
  pthread_t thread;
  pthread_create (&thread, NULL, &thread_1, NULL);
  call_fp(&read, &x);
  call_fp(&write, &y);
  return 0;
}

