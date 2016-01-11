
#include <stdio.h>

int *g_cache_tail;

void func1 () {
  int *remove___0;

  remove___0 = g_cache_tail;
  if (! (remove___0 != NULL)) {
    printf ("bam");
  }
  cache_remove(remove___0);
  
  return;
}

void func2 () {
  
  //TODO: make like cache_get in knot_comb.c
  return;
}

void cache_remove (int *entry) {

  if (! (entry != NULL ) ) {
    printf ("boo");
  }

  *entry = 10;
  return;

}

int main (int argc, char *argv) {
  func1 ();
  return 0;
}
