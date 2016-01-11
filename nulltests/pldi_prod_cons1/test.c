#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

// PLDI 08 example (safe version)

typedef struct _buffer_list {
  void *data;
  struct _buffer_list *next;
} buffer_list;

#define lock(l) \
  pthread_mutex_lock(l)
#define unlock(l) \
  pthread_mutex_unlock(l)

#define DATA_LEN 100

//////////////////////////////////////////////////////////////
// Safe version

buffer_list * bufs1;
pthread_mutex_t buf_lock1;
int perf_ctr1;

void *makeData1(size_t s) {
  return malloc(s);
}

buffer_list *makeList1(int num) {
  int i;
  buffer_list *result = NULL;
  buffer_list *next = NULL;

  for (i = 0; i < num; i++) {
    next = malloc(sizeof(buffer_list));
    next->next = result;
    result = next;
  }

  return result;
}


int *produce1() {
  int i;
  int *result;
  result = (int *)malloc(sizeof(int) * DATA_LEN);
  for (i = 0; i < DATA_LEN; i++) {
    result[i] = i;
  }
  return result;
}

void consume1(int *data) {
  int i;
  if (data != NULL) {
    for (i = 0; i < DATA_LEN; i++) {
      printf ("%d\n", data[i]);
    }
  }
}

void *thread_producer1(void *ignore) {
  buffer_list *px;
  int *t;
  px = bufs1;
  
  while (px != NULL) {
    lock(&buf_lock1);
    px->data = makeData1(sizeof(int *));
    
    perf_ctr1++;
    t = produce1();
    
    *((int **)(px->data)) = t;
    unlock(&buf_lock1);
    px = px->next;
  }
  return 0;
}


void *thread_consumer1(void *ignore) {
  buffer_list *cx;
  perf_ctr1 = 0;
  cx = bufs1;
  while(cx != NULL) {
    lock(&buf_lock1);
    if (cx->data != NULL) {
      consume1(*((int **)cx->data));
      free(cx->data);
      cx->data = NULL;
      cx = cx->next;
    }
    unlock(&buf_lock1);
  }
  return 0;
}

//////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
  pthread_t thr0, thr1, thr2, thr3;

  pthread_mutex_init(&buf_lock1, 0);

  bufs1 = makeList1(1000);

  pthread_create(&thr0, (void *)0, &thread_consumer1, (void *)0);
  pthread_create(&thr1, (void *)0, &thread_producer1, (void *)0);

  return 0;
}
