
#ifndef __ANALYSIS_STUBS___9124
#define __ANALYSIS_STUBS___9124

/** Stubs for function-pointer analysis */

//macro it

#define memset(_s_, _c_, _n_)                   \
  _s_

/*
void *memset(void *s, int c, size_t n) {
  unsigned char c2 = (unsigned char)c;
  unsigned char *sp = (unsigned char*)s;
  for (int i = 0; i < n; i ++) {
    sp[i] = c2;
  }
  return s;
}
*/


typedef struct _reallybig_memcpy_struct {
  unsigned char big_struct_field[1024];
} reallybig_memcpy_struct;

void *memcpy(void *d, void *s, size_t n) {
  reallybig_memcpy_struct *s1 = (reallybig_memcpy_struct *)s;
  reallybig_memcpy_struct *d1 = (reallybig_memcpy_struct *)d;

  *d1 = *s1;
  return (void *)d;
}


// pthread_create (pretend there's an FP call or no?)


// leave most unknown, returning TOP (e.g., strchr, strlen, ...)
// because it just returns pointers within strings

#endif
