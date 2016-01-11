#include <stdio.h>
#include <stdlib.h>
#include "lock.h"
#include "ptrtest.h"

typedef unsigned char uint8_t;

typedef unsigned short uint16_t;

typedef unsigned int uint32_t;

typedef struct _pthread_descr_struct { 
  int blah;
} _pthread_descr ;

typedef struct _list {
  int data;
  spinlock_t *lock;
  struct _list *next;
} list;

struct _pthread_fastlock {
   long __status ;
   int __spinlock ;
};

struct __anonstruct_pthread_mutex_t_16 {
  int __m_reserved ;
  int __m_count ;
  _pthread_descr __m_owner ;
  int __m_kind ;
  struct _pthread_fastlock __m_lock ;
};

struct __anonstruct_pthread_mutex_t_16 cli_ref_mutex  =    
  {0, 0, (struct _pthread_descr_struct *)0, 0, {0L, 0}};

static int targettab[6] = { 0, 502, 511, 519, 520, 515};


struct cli_bm_patt {
   char *pattern ;
   char *virname ;
   char *offset ;
   char const   *viralias ;
   unsigned int length ;
   unsigned short target ;
   struct cli_bm_patt *next ;
};

struct cli_ac_patt {
   short *pattern ;
   unsigned int length ;
   unsigned int mindist ;
   unsigned int maxdist ;
   char *virname ;
   char *offset ;
   char const   *viralias ;
   unsigned short sigid ;
   unsigned short parts ;
   unsigned short partno ;
   unsigned short alt ;
   unsigned short *altn ;
   unsigned short type ;
   unsigned short target ;
   char **altc ;
   struct cli_ac_patt *next ;
};

struct cli_ac_node {
   char islast ;
   struct cli_ac_patt *list ;
   struct cli_ac_node *trans[256] ;
   struct cli_ac_node *fail ;
};

struct cli_md5_node {
   char *virname ;
   char *viralias ;
   unsigned char *md5 ;
   unsigned int size ;
   unsigned short fp ;
   struct cli_md5_node *next ;
};

struct cli_meta_node {
   int csize ;
   int size ;
   int method ;
   unsigned int crc32 ;
   unsigned int fileno ;
   unsigned int encrypted ;
   unsigned int maxdepth ;
   char *filename ;
   char *virname ;
   struct cli_meta_node *next ;
};

struct cl_node {
   unsigned int refcount ;
   unsigned int maxpatlen ;
   int *bm_shift ;
   struct cli_bm_patt **bm_suffix ;
   struct cli_ac_node *ac_root ;
   struct cli_ac_node **ac_nodetable ;
   unsigned int ac_partsigs ;
   unsigned int ac_nodes ;
   struct cli_md5_node **md5_hlist ;
   struct cli_meta_node *zip_mlist ;
   struct cli_meta_node *rar_mlist ;
};



void miniMunge2(int *x, spinlock_t *l){
  int i, *p;
  i = 10;
  p = &i;
  (*p)++;          // symstate can do a strong update
  _spin_lock(l);
  (*x)++;          // symstate not doing a strong update, since x is
                   // classified as an ext ptr
  _spin_unlock(l);
}

void miniMunge(int *argX, spinlock_t *argLock){
  miniMunge2(argX, argLock);
  miniMunge2(argX, argLock);
}


void accList(void *arg) {
  list *l;
  int *px;
  spinlock_t *pl;

  l = (list *) arg;

  while (l) {
    px = & (l->data);
    pl = l->lock;

    miniMunge(px, pl);

    l = l->next;
  }
  
}


list *makeList(int size) {
  list *result = 0;
  list *prev = 0;

  while (size > 0) {
    result = (list *) malloc (sizeof(list));
    result->lock = (spinlock_t*) malloc (sizeof(spinlock_t));
    result->data = 0;
    result->next = prev;

    prev = result;
  }

  return result;
}


int cli_validatesig(unsigned short target , unsigned short ftype , 
                    char const   *offstr ,
                    unsigned long fileoff , int desc , 
                    char const   *virname ) 
{ long off ;
  long tmp ;
  
  if (target) {
    
    if ((int )target >= 6) {
      
      fprintf(stderr, "Bad target in signature (%s)\n", virname);

      return (0);
    } else {
      if (targettab[target] != (int )ftype) {

        fprintf(stderr, "Type: %d, expected: %d (%s)\n", 
                ftype, targettab[target], virname);
        return (0);
      }
    }
  }

  if (offstr) {

    if (desc != -1) {

      tmp = 10L; /* cli_caloff(offstr, desc); */

      off = tmp;

      if (off == -1L) {

        fprintf(stderr,"Bad offset in signature (%s)\n", virname);

        return (0);
      }

      if (fileoff != (unsigned long )off) {

        fprintf(stderr, "Virus offset: %d, expected: %d (%s)\n", 
               fileoff, off, virname);

        return (0);
      }
    }
  }

  return (1);

}



int cli_bm_scanbuff(char const   *buffer , 
                    unsigned int length , char const   **virname ,
                    struct cl_node const  *root , 
                    unsigned long offset , unsigned short ftype ,
                    int fd ) 
{ unsigned int i ;
  unsigned int j ;
  unsigned int shift ;
  unsigned int off ;
  unsigned int found ;
  uint16_t idx ;
  struct cli_bm_patt *p ;
  char const   *bp ;
  char prefix ;
  int tmp ;

  found = 0U;

  if (! root->bm_shift) {
    return (0);
  }

  if (length < 3U) {
    return (0);
  }

  i = 0U;

  while (i < (length - 3U) + 1U) {
    idx = (unsigned short )
      ((211 * 
        (int )((unsigned char )(*(buffer + i))) +
        37 * 
        (int )((unsigned char )(*(buffer + (i + 1U))))
        ) + 
       (int )((unsigned char )(*(buffer + (i + 2U))))
       );

    shift = (unsigned int )(*(root->bm_shift + (int )idx));

    if (shift == 0U) {

      prefix = (char )(*(buffer + ((i - 3U) + 3U)));

      p = (*(root->bm_suffix + (int )idx));

      while (1) {

        if (p) {

          if (! ((int )(*(p->pattern + 0)) != (int )prefix)) {

            break;
          }
          
        } else {
          
          break;
        }

        p = p->next;
      }

      while (1) {

        if (p) {

          if (! ((int )(*(p->pattern + 0)) == (int )prefix)) {

            break;
          }
        } else {

          break;
        }

        off = (i - 3U) + 3U;

        bp = buffer + off;

        found = 1U;

        j = 0U;

        while (1) {
          
          if (j < p->length) {

            if (! (off < length)) {

              break;
            }
          } else {

            break;
          }

          if ((int const )(*(bp + j)) != 
              (int const )(*(p->pattern + j))) {

            found = 0U;

            break;
          }

          j ++;

          off ++;
        }

        if (found) {

          if (p->length == j) {

            if (p->target) {
              goto _L___0;
            } else {

              if (p->offset) {
                _L___0: 

                off = (unsigned int )
                  (((offset + (unsigned long )i) - 3UL) + 3UL);

                if (fd == -1) {

                  if (! ftype) {

                    p = p->next;

                    continue;
                  } else {
                    goto _L;
                  }
                } else {
                  _L: 

                  tmp = cli_validatesig(p->target, ftype, 
                                        (char const   *)p->offset,
                                        (unsigned long )off, 
                                        fd, 
                                        (char const   *)p->virname);

                  if (! tmp) {

                    p = p->next;

                    continue;
                  }
                }
              }
            }

            if (virname) {

              (*virname) = (char const   *)p->virname;
            }

            return (1);
          }
        }

        p = p->next;
      }

      shift = 1U;
    }

    i += shift;
  }

  return (0);
}


int main(int argc, char *argv[]) {

  list *aList;

  aList = makeList (10);

  accList((void *) aList);
  accList((void *) aList);


  ptrMain(argc, argv);

  return 0;
}
