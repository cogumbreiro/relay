#include <stdio.h>
#include <pthread.h>

pthread_mutex_t g_mutex ;
int *g_int;

void func0 () {
  pthread_mutex_lock(&g_mutex);
  if (g_int != NULL) {
    pthread_mutex_unlock(&g_mutex);
    *g_int = 5;
  } else {
    pthread_mutex_unlock(&g_mutex);
  }
}

void func1 () {
  pthread_mutex_lock(&g_mutex);
  g_int = NULL;
  pthread_mutex_unlock(&g_mutex);
}

int main() {
  pthread_t thr0, thr1, thr2;
  pthread_create (&thr0, (void *)0, &func0, (void *)0);
  pthread_create (&thr1, (void *)0, &func1, (void *)0);
  return 0;
}
