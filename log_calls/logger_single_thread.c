#include<string.h>
#include<stdio.h>
#include<unistd.h>

FILE *myFile = 0;

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
}

void __my_log_calls__(const char * str) {
  if (myFile == 0) {
    doInitialize();
  }
  fprintf(myFile, "%s", str);
}
