#include<string.h>
#include<stdio.h>
#include<unistd.h>
#include<pthread.h>

FILE *myFile = 0;
pthread_mutex_t myLock;

static doInitialize () {
  pid_t tempPID;
  char tempFName[120];
  char tempITOA[120];
  char *mode = "a+";

  tempPID = getpid();
  strcpy(tempFName, "_logged_calls_");
  sprintf(tempITOA, "%d", tempPID);
  strcat(tempFName, tempITOA);

  myFile = fopen (tempFName, mode);

  pthread_mutex_init(&myLock, (pthread_mutexattr_t *) NULL);
}

void __my_log_calls__(const char * str) {
  pthread_t myID;
  if (myFile == 0) {
    doInitialize();
  }
  myID = pthread_self();
  pthread_mutex_lock(&myLock);
  fprintf(myFile, "%d : %s", (int) myID, str);
  pthread_mutex_unlock(&myLock);
}
