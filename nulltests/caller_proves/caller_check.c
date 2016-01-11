/** Some test cases for the null pointer analysis */

#include <stdio.h>
#include <stdlib.h>

/********** caller checks *********/

void derefI (int *p) {
  printf ("%d\n", *p);
}

void derefC (char *pc) {
  *pc = 'C';
  printf ("%c\n", *pc);
}

void checkForCallee (int *arg1) {

  if (arg1 != NULL) {
    derefI(arg1);
  }

}

void checkForCallee2 () {
  int *p1;
  int *p2;

  p1 = (int *)malloc(sizeof(int));
  printf ("Init value is: %d\n", *p1);
  derefI(p1);

  if (p1) {
    derefI(p1);
  }

  /* check how it goes w/ some aliasing too */
  p2 = p1;
  
  printf ("Init value is: %d\n", *p2);
  derefI(p2);

}

void checkForCallee3 (char **p) {

  if (p) {
    if (! *p) {
      printf ("Null...\n");
    } else {
      derefC(*p);
    }
  }

}


void change (int **z) {
  *z = NULL;
}

void changeBefore (int *y) {
  change (&y);
  derefI (y);
}

void checkForCallee4 (int *x) {

  if (x) {
    changeBefore (x);
  }

}

void checkForCallee5 (int *x) {

  if (x) {
    derefI (x);
    change (&x);
  }

}

/**** callee checks *******/

void notCheckedForCallee (int *arg2) {
  derefI (arg2);
}

void exitWrapper () {
  exit(0);
}


void checker1 (int *toCheck1) {
  if (!toCheck1) {
    exit (0);
  }
  return;
}

void calleeChecks1 () {
  int *p = NULL;

  checker1(p);
  printf ("%d\n", *p);

}

void checker2 (int *toCheck2) {

  if (!toCheck2) {
    exitWrapper ();
  }

  return;
}

void calleeChecks2 () {
  int *p = NULL;

  checker2(p);
  printf ("%d\n", *p);

}

void checker3 (int **toCheck3) {

  /* assume caller ensures original pointer ok */
  if (*toCheck3) {
    return;
  } else {
    *toCheck3 = (int *)malloc (sizeof(int));
  }

}

void calleeChecks3 () {
  int *p = NULL;
  
  checker3(&p);
  printf ("%d\n", *p);

}


/****************** With aliasing ******************/

void derefA (int *arg143) {
  int *p98;

  p98 = arg143;
  printf ("%d\n", *p98);
  return;
}

void inputNonNullA (int *arg1249) {

  int *p124 = arg1249;
  int *p182 = arg1249 + 1;

  derefA(p124);
  derefA(p182);

  return;
}



/************/

int main (int argc, char *argv[]) {
  
  int x;
  int *np = NULL;
  int *uninit;

  /* safe */
  checkForCallee (&x);
  checkForCallee (&argc);
  checkForCallee2 ();
  checkForCallee3 (argv);

  /* unsafe */
  notCheckedForCallee (np);

  /* safe, &x will make it a non-null pointer!!! */
  notCheckedForCallee (&x);

  /* unsafe, nobody proves */
  notCheckedForCallee (uninit);

  /* unsafe, was NULL to begin with */
  notCheckedForCallee (NULL);

  /* safe (uses exit) */
  calleeChecks1 ();
  calleeChecks2 ();

  /* safe (callee mallocs) */
  calleeChecks3 ();

  /* unsafe -- Caller-proved to be safe at one level, but fact killed */
  checkForCallee4 (&x);

  /* safe -- Caller-proved to be safe at one level, and fact killed AFTER */
  checkForCallee5 (&x);

  /* safe, but currently conservative about arguments */
  inputNonNullA (&x);

  return 0;
}
