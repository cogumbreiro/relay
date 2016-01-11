#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>

// PLDI 08 example (unsafe version)

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
// Buggy version

buffer_list * bufs2;
pthread_mutex_t buf_lock2;
int perf_ctr2;

// SUPER LAME that we need to clone all these functions
// so that we end up with separate Steensgaard rep nodes...

void *makeData2(size_t s) {
  return malloc(s);
}

buffer_list *makeList2(int num) {
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


int *produce2() {
  int i;
  int *result;
  result = (int *)malloc(sizeof(int) * DATA_LEN);
  for (i = 0; i < DATA_LEN; i++) {
    result[i] = i;
  }
  return result;
}

void consume2(int *data) {
  int i;
  if (data != NULL) {
    for (i = 0; i < DATA_LEN; i++) {
      printf ("%d\n", data[i]);
    }
  }
}

void *thread_producer2(void *ignore) {
  buffer_list *px;
  int *t;
  px = bufs2;
  
  while (px != NULL) {
    lock(&buf_lock2);
    px->data = makeData2(sizeof(int *));
    unlock(&buf_lock2); // EXTRA

    perf_ctr2++;
    t = produce2();
    
    lock(&buf_lock2); // EXTRA
    *((int **)(px->data)) = t;
    unlock(&buf_lock2);
    px = px->next;
  }
  return 0;
}

void *thread_consumer2(void *ignore) {
  buffer_list *cx;
  perf_ctr2 = 0;
  cx = bufs2;
  while(cx != NULL) {
    lock(&buf_lock2);
    if (cx->data != NULL) {
      consume2(*((int **)cx->data));
      free(cx->data);
      cx->data = NULL;
      cx = cx->next;
    }
    unlock(&buf_lock2);
  }
  return 0;
}

//////////////////////////////////////////////////////////////

int main(int argc, char *argv[]) {
  pthread_t thr0, thr1, thr2, thr3;

  pthread_mutex_init(&buf_lock2, 0);

  bufs2 = makeList2(1000);

  pthread_create(&thr2, (void *)0, &thread_consumer2, (void *)0);
  pthread_create(&thr3, (void *)0, &thread_producer2, (void *)0);
  
  return 0;
}
