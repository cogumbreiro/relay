#include "lock.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define min(a,b) ((a < b) ? a : b)

void (*fp_mun) (int *, spinlock_t *);
void (*fp_rec) (spinlock_t *);
void (*fp_rec2) (spinlock_t *);
void (*fp_acc) (void);

int g10 = 41;
int g11 = 2;
int g12 = 1;
int g13 = 4;

int v1 = 1;
int v2 = 2;
int v3 = 3;
int v4 = 4;
int v5 = 5;

int *gp1 = (int*)((void*)0);

spinlock_t gLock10 = {0};
spinlock_t gLock11 = {0};
spinlock_t gLock12 = {0};

spinlock_t gLockV1 = {0};
spinlock_t gLockV2 = {0};
spinlock_t gLockV3 = {0};
spinlock_t gLockV4 = {0};
spinlock_t gLockV5 = {0};



typedef struct _dat_lock {
  spinlock_t l;
  int x;
} dat_lock;

typedef struct _dispatch {
  void (*mun) (int *, spinlock_t *);
  void (*acc) (void);
} dispatch;

#define NUM_KIDS 2

typedef struct _recurStruct {
  struct _recurStruct *kids[NUM_KIDS];
  int data;
} recurStruct;

dat_lock datArr[2][2];
dat_lock **datArr2;
dispatch fArr[2][2];

dispatch *gfp = (dispatch*)0;


recurStruct *recS[2];

void foo() {
  g10++;
}

void bar() {
  v4++;
}

int lockedOnEntry() {
  _spin_unlock(&gLock11);
  g11++;
  return g11;
}

void munge2(int *x, spinlock_t *l){
  int i, *p;
  i = 10;
  p = &i;
  (*p)++;          // symstate can do a strong update
  _spin_lock(l);
  (*x)++;          // symstate not doing a strong update, since x is
                   // classified as an ext ptr
  _spin_unlock(l);
}


void munge(int *g10, spinlock_t *gLock10) {
  munge2(g10,gLock10);
  munge2(g10,gLock10);
}

void doubDerefMunge (int **ppi, spinlock_t *pl) {
  _spin_lock (pl);
  (*(*ppi))++;
  _spin_unlock (pl);
}

void useDDM (int *pi, spinlock_t *pl) {
  doubDerefMunge (&pi, pl);
}

void recurses(int *x, spinlock_t *l){

  _spin_lock(l);
  (*x)--;
  _spin_unlock(l);
  if(*x > 0) {
    recurses(x, l);
  }
}

void scc3_3(spinlock_t *l) {
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(v4 > 0) {
    fp_rec(l);
  }
}


void scc3_2(spinlock_t *l) {
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(v4 > 0) {
    fp_rec(l);
  }
}


void scc3_1(spinlock_t *l) {
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(v4 > 0) {
    fp_rec(l);
  }
}

void multi_recurses3(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(v4 > 0) {
    multi_recurses4(l);
    multi_recurses5(l,l);
    multi_recurses6(l,l);
    multi_recurses7(l,l);
  }

}


void multi_recurses2(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(v4 > 0) {
    multi_recurses3(l);
  }

}

void multi_recurses1(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(v4 > 0) {
    multi_recurses2(l);
  }

}


void multi_recurses4(spinlock_t *l){
  
  _spin_lock(l);
  bar();
  _spin_unlock(l);
  if(v4 > 0) {
    multi_recurses1(l);
  }

}

void multi_recurses5(spinlock_t *l1, spinlock_t *l2){
  
  _spin_lock(l2);
  bar();
  if(v4 > 0) { 
    _spin_unlock(l2);
    multi_recurses5(l2,l1); //swap causes a race (if l1 != l2)
  } else {
    _spin_unlock(l2);
  }
}

void multi_recurses6(spinlock_t *l1, spinlock_t *l2){
  _spin_lock(l2);
  bar();
  if(v4 > 0) { 
    _spin_unlock(l2);
    multi_recurses6(l1,l2); //no swap, so no race
  } else {
    _spin_unlock(l2);
  }
}


void multi_recurses7(spinlock_t *l1, spinlock_t *l2){
  _spin_lock(l2);
  bar();
  _spin_unlock(l2);
  if(v4 > 0) {               //unlocked too early, read/write race
    multi_recurses7(l1,l2); 
  }
}


void structAcc(dat_lock d) {
  munge(&d.x, &d.l);
}

void iterRecStruct(recurStruct *rs) {
  int i;
  
  while(rs) {
    rs->data = 10;
    rs = rs->kids[0]; //only unroll one part of descendants
  }
}

void recurseRecStruct(recurStruct *rs) {
  int i;

  for(i = 0; i < NUM_KIDS; i++){
    if(rs->kids[i])
      recurseRecStruct(rs->kids[i]);
  }
  rs->data = 18;
}


void iterRecStructCast(void *arg) {
  int i;
  recurStruct *rs;

  rs = (recurStruct *)arg;
  
  while((rs) && (rs->kids[0])) {
    rs->kids[0]->data = 24;
    rs = rs->kids[0]; 
  }
}

void recurseRecStructCast(void *arg) {
  int i;
  recurStruct *rs;

  rs = (recurStruct *)arg;

  for(i = 0; i < NUM_KIDS; i++){
    if(rs->kids[i]) {
      rs->kids[i]->data = 56;
      recurseRecStruct(rs->kids[i]);
    }
  }
}

void dyMunge(int *dyX, spinlock_t *dyL) {
  _spin_lock(dyL);
  *dyX = *dyX + 1;
  _spin_unlock(dyL);
}


void dyAlloc() {
  int *d1, *d2, *d3;
  spinlock_t *l1, *l2;
  int *a, *b, *c;

  dat_lock dl;

  recurStruct recStrct;

  dl.x = 10;

  d1 = (int*) malloc (sizeof(int));
  d2 = (int*) malloc (sizeof(int));
  d3 = (int*) malloc (sizeof(int));

  *d1 = 1; *d2 = 2; *d3 = 3;
  
  l1 = (spinlock_t*) malloc (sizeof(spinlock_t));
  l2 = (spinlock_t*) malloc (sizeof(spinlock_t));
 
  a = d1;

  dyMunge(a, l1);

  a = d2;

  dyMunge(a, l2);

  a = d3;

  dyMunge(a, l2);

  a = &(dl.x);

  dyMunge(a, l1);

  a = (&dl) + sizeof(spinlock_t);
 
  dyMunge(a, l2);

  b = (&dl) + sizeof(spinlock_t);
 
  dyMunge(b, l2);

  c = (&dl.x);

  dyMunge(c, l1);

  recStrct.kids[0] = &recStrct;

  dyMunge(recStrct.kids[0], l2);
  
  free(d1); free(d2); free(d3);
  free(l1); free(l2);
 
}

int *mallocInt() {
  int *p;

  p = (int*)malloc(sizeof(*p));

  return p;
}

dispatch *mallocDispatch() {
  dispatch *p;

  p = (dispatch*)malloc(sizeof(dispatch));
  p->mun = munge;
  p->acc = bar;
  return p;
}

void hitDifferentInt(spinlock_t *aLock) {
  _spin_lock(aLock);
  gp1 = mallocInt();
  (*gp1)++;
  free(gp1);
  _spin_unlock(aLock);
}

void hitDifferentDispatch(spinlock_t *aLock) {
  _spin_lock(aLock);
  gfp = mallocDispatch();
  free(gfp);
  _spin_unlock(aLock);
}

static int pointinrect(double const   *point , double (*coords)[2] ) 
{ double max[2] ;
  double min[2] ;
  int tmp ;

  {
  if ((*(coords + 0))[0] > (*(coords + 1))[0]) {
    max[0] = (*(coords + 0))[0];
    min[0] = (*(coords + 1))[0];
  } else {
    max[0] = (*(coords + 1))[0];
    min[0] = (*(coords + 0))[0];
  }
  if ((*(coords + 0))[1] > (*(coords + 1))[1]) {
    max[1] = (*(coords + 0))[1];
    min[1] = (*(coords + 1))[1];
  } else {
    max[1] = (*(coords + 1))[1];
    min[1] = (*(coords + 0))[1];
  }
  if ((*(point + 0)) >= (double const   )min[0]) {
    if ((*(point + 0)) <= (double const   )max[0]) {
      if ((*(point + 1)) >= (double const   )min[1]) {
        if ((*(point + 1)) <= (double const   )max[1]) {
          tmp = 1;
        } else {
          goto _L___0;
        }
      } else {
        _L___0: /* CIL Label */ 
        goto _L___1;
      }
    } else {
      goto _L___1;
    }
  } else {
    _L___1: /* CIL Label */ 
    tmp = 0;
  }
  return (tmp);
}
}


void funcWithLabelAtStart () {
  int x;
  int y;
  {
  retry:
    x = rand ();
    y = rand ();
    if ( x != y ) goto retry;
    return;
  }
}

/**
 * strncpy - Copy a length-limited, %NUL-terminated string
 * @dest: Where to copy the string to
 * @src: Where to copy the string from
 * @count: The maximum number of bytes to copy
 *
 * The result is not %NUL-terminated if the source exceeds
 * @count bytes.
 *
 * In the case where the length of @src is less than  that  of
 * count, the remainder of @dest will be padded with %NUL.
 *
 */
char *strncpy(char *dest, const char *src, size_t count)
{
        char *tmp = dest;

        while (count) {
                if ((*tmp = *src) != 0)
                        src++;
                tmp++;
                count--;
        }
        return dest;
}


/************** From DAC960.c */


/*
  Define a Boolean data type.
*/
typedef enum { false, true } __attribute__ ((packed)) boolean;



/*
  Define the SCSI INQUIRY Standard Data structure.
*/
typedef struct DAC960_SCSI_Inquiry
{
  unsigned char PeripheralDeviceType:5;			/* Byte 0 Bits 0-4 */
  unsigned char PeripheralQualifier:3;			/* Byte 0 Bits 5-7 */
  unsigned char DeviceTypeModifier:7;			/* Byte 1 Bits 0-6 */
  boolean RMB:1;					/* Byte 1 Bit 7 */
  unsigned char ANSI_ApprovedVersion:3;			/* Byte 2 Bits 0-2 */
  unsigned char ECMA_Version:3;				/* Byte 2 Bits 3-5 */
  unsigned char ISO_Version:2;				/* Byte 2 Bits 6-7 */
  unsigned char ResponseDataFormat:4;			/* Byte 3 Bits 0-3 */
  unsigned char :2;					/* Byte 3 Bits 4-5 */
  boolean TrmIOP:1;					/* Byte 3 Bit 6 */
  boolean AENC:1;					/* Byte 3 Bit 7 */
  unsigned char AdditionalLength;			/* Byte 4 */
  unsigned char :8;					/* Byte 5 */
  unsigned char :8;					/* Byte 6 */
  boolean SftRe:1;					/* Byte 7 Bit 0 */
  boolean CmdQue:1;					/* Byte 7 Bit 1 */
  boolean :1;						/* Byte 7 Bit 2 */
  boolean Linked:1;					/* Byte 7 Bit 3 */
  boolean Sync:1;					/* Byte 7 Bit 4 */
  boolean WBus16:1;					/* Byte 7 Bit 5 */
  boolean WBus32:1;					/* Byte 7 Bit 6 */
  boolean RelAdr:1;					/* Byte 7 Bit 7 */
  unsigned char VendorIdentification[8];		/* Bytes 8-15 */
  unsigned char ProductIdentification[16];		/* Bytes 16-31 */
  unsigned char ProductRevisionLevel[4];		/* Bytes 32-35 */
}
DAC960_SCSI_Inquiry_T;

/*
  Define the SCSI INQUIRY Unit Serial Number structure.
*/
typedef struct DAC960_SCSI_Inquiry_UnitSerialNumber
{
  unsigned char PeripheralDeviceType:5;			/* Byte 0 Bits 0-4 */
  unsigned char PeripheralQualifier:3;			/* Byte 0 Bits 5-7 */
  unsigned char PageCode;				/* Byte 1 */
  unsigned char :8;					/* Byte 2 */
  unsigned char PageLength;				/* Byte 3 */
  unsigned char ProductSerialNumber[28];		/* Bytes 4-31 */
}
DAC960_SCSI_Inquiry_UnitSerialNumber_T;


/*
  DAC960_SanitizeInquiryData sanitizes the Vendor, Model, Revision, and
  Product Serial Number fields of the Inquiry Standard Data and Inquiry
  Unit Serial Number structures.
*/
static void DAC960_SanitizeInquiryData(DAC960_SCSI_Inquiry_T
					 *InquiryStandardData,
				       DAC960_SCSI_Inquiry_UnitSerialNumber_T
					 *InquiryUnitSerialNumber,
				       unsigned char *Vendor,
				       unsigned char *Model,
				       unsigned char *Revision,
				       unsigned char *SerialNumber)
{
  int SerialNumberLength, i;
  if (InquiryStandardData->PeripheralDeviceType == 0x1F) return;
  for (i = 0; i < sizeof(InquiryStandardData->VendorIdentification); i++)
    {
      unsigned char VendorCharacter =
	InquiryStandardData->VendorIdentification[i];
      Vendor[i] = (VendorCharacter >= ' ' && VendorCharacter <= '~'
		   ? VendorCharacter : ' ');
    }
  Vendor[sizeof(InquiryStandardData->VendorIdentification)] = '\0';
  for (i = 0; i < sizeof(InquiryStandardData->ProductIdentification); i++)
    {
      unsigned char ModelCharacter =
	InquiryStandardData->ProductIdentification[i];
      Model[i] = (ModelCharacter >= ' ' && ModelCharacter <= '~'
		  ? ModelCharacter : ' ');
    }
  Model[sizeof(InquiryStandardData->ProductIdentification)] = '\0';
  for (i = 0; i < sizeof(InquiryStandardData->ProductRevisionLevel); i++)
    {
      unsigned char RevisionCharacter =
	InquiryStandardData->ProductRevisionLevel[i];
      Revision[i] = (RevisionCharacter >= ' ' && RevisionCharacter <= '~'
		     ? RevisionCharacter : ' ');
    }
  Revision[sizeof(InquiryStandardData->ProductRevisionLevel)] = '\0';
  if (InquiryUnitSerialNumber->PeripheralDeviceType == 0x1F) return;
  SerialNumberLength = InquiryUnitSerialNumber->PageLength;
  if (SerialNumberLength >
      sizeof(InquiryUnitSerialNumber->ProductSerialNumber))
    SerialNumberLength = sizeof(InquiryUnitSerialNumber->ProductSerialNumber);
  for (i = 0; i < SerialNumberLength; i++)
    {
      unsigned char SerialNumberCharacter =
	InquiryUnitSerialNumber->ProductSerialNumber[i];
      SerialNumber[i] =
	(SerialNumberCharacter >= ' ' && SerialNumberCharacter <= '~'
	 ? SerialNumberCharacter : ' ');
    }
  SerialNumber[SerialNumberLength] = '\0';
}


/**************** From linux/lib/idr.c (and .h)   */

#define IDR_BITS 5
#define IDR_MASK ((1 << IDR_BITS)-1)

#define MAX_ID_SHIFT (sizeof(int)*8 - 1)
#define MAX_ID_BIT (1U << MAX_ID_SHIFT)
#define MAX_ID_MASK (MAX_ID_BIT - 1)


/* Leave the possibility of an incomplete final layer */
#define MAX_LEVEL (MAX_ID_SHIFT + IDR_BITS - 1) / IDR_BITS

/* Number of id_layer structs to leave in free list */
#define IDR_FREE_MAX MAX_LEVEL + MAX_LEVEL


struct idr_layer {
	unsigned long		 bitmap; /* A zero bit means "space here" */
	struct idr_layer	*ary[1<<IDR_BITS];
	int			 count;	 /* When zero, we can release it */
};

struct idr {
	struct idr_layer *top;
	struct idr_layer *id_free;
	int		  layers;
	int		  id_free_cnt;
	spinlock_t	  lock;
};

static struct idr_layer *alloc_layer(struct idr *idp)
{
	struct idr_layer *p;

	_spin_lock(&idp->lock);
	if ((p = idp->id_free)) {
		idp->id_free = p->ary[0];
		idp->id_free_cnt--;
		p->ary[0] = NULL;
	}
	_spin_unlock(&idp->lock);
	return(p);
}

static void free_layer(struct idr *idp, struct idr_layer *p)
{
	/*
	 * Depends on the return element being zeroed.
	 */
	_spin_lock(&idp->lock);
	p->ary[0] = idp->id_free;
	idp->id_free = p;
	idp->id_free_cnt++;
	_spin_unlock(&idp->lock);
}

static void sub_remove(struct idr *idp, int shift, int id)
{
	struct idr_layer *p = idp->top;
	struct idr_layer **pa[MAX_LEVEL];
	struct idr_layer ***paa = &pa[0];
	int n;

	*paa = NULL;
	*++paa = &idp->top;

	while ((shift > 0) && p) {
		n = (id >> shift) & IDR_MASK;
        //__clear_bit(n, &p->bitmap);
		*++paa = &p->ary[n];
		p = p->ary[n];
		shift -= IDR_BITS;
	}
	n = id & IDR_MASK;
	if (p != NULL) {//likely(p != NULL)) && test_bit(n, &p->bitmap))){
      //__clear_bit(n, &p->bitmap);
		p->ary[n] = NULL;
		while(*paa && ! --((**paa)->count)){
			free_layer(idp, **paa);
			**paa-- = NULL;
		}
		if (!*paa)
			idp->layers = 0;
	} else
      ;//idr_remove_warning(id);
}


/**
 * idr_remove - remove the given id and free it's slot
 * idp: idr handle
 * id: uniqueue key
 */
void idr_remove(struct idr *idp, int id)
{
	struct idr_layer *p;

	/* Mask off upper bits we don't use for the search. */
	id &= MAX_ID_MASK;

	sub_remove(idp, (idp->layers - 1) * IDR_BITS, id);
	if (idp->top && idp->top->count == 1 && (idp->layers > 1) &&
	    idp->top->ary[0]) {  // We can drop a layer

		p = idp->top->ary[0];
		idp->top->bitmap = idp->top->count = 0;
		free_layer(idp, idp->top);
		idp->top = p;
		--idp->layers;
	}
	while (idp->id_free_cnt >= IDR_FREE_MAX) {
		p = alloc_layer(idp);
		//kmem_cache_free(idr_layer_cache, p);
		return;
	}
}


void voidNoReturn(int *dptr, spinlock_t *lptr) {
  
  int i, *p;
  i = 10;
  p = &i;
  (*p)++;          
  _spin_lock(lptr);
  (*dptr)++;
  _spin_unlock(lptr);

}

spinlock_t sccLock = {0};

int main(int argc, char *argv[]) {

  int i;
  int x = 1;
  int y = 2;
  void (*fp_mun2) (int *, spinlock_t *) = 0;
  void (*fp_rec2) (spinlock_t *);
  double *point;
  double *rect[2];
  char *testStr = "hello world!\n";
  char buf[81];

  srand(time(NULL));

  fp_acc = foo; 
  _spin_lock(&gLock10);
  fp_acc();
  _spin_lock(&gLock11);

  munge(&g11, &gLock11);

  g11++;
  lockedOnEntry();
  
  _spin_unlock(&gLock10);


  if(y < x)
    goto label1; 
  x++;
  y++;

  fp_mun2 = munge2;
  fp_mun = munge;
  fp_mun(&v1,&gLockV1);
  fp_mun(&v2,&gLockV2);
  fp_mun(&v3,&gLockV3);

  recurses(&v1,&gLockV1);

  fp_rec = multi_recurses2;
  fp_rec = multi_recurses3;
  fp_rec = multi_recurses1;

  fp_rec2 = scc3_1;
  fp_rec2 = scc3_2;
  fp_rec2 = scc3_3;

  fp_rec(&gLockV4);

  fp_rec2(&sccLock);

  fp_mun(&((*datArr)->x), &((*datArr)->l));

  structAcc(**datArr);

  datArr2 = (dat_lock**)malloc(2 * sizeof(dat_lock*));
  *datArr2 = (dat_lock*)malloc(sizeof(dat_lock));
  *(datArr2+1) = (dat_lock*)malloc(sizeof(dat_lock));
  
  structAcc(**datArr2);

  fArr[0][0].mun = munge;
  fArr[0][0].acc = bar;

  fArr[0][0].mun(&v1,&gLockV1);

  /* nothing is actually set for fArr+1 */
  (*fArr+1)->acc();

  hitDifferentInt(&gLockV1);
  hitDifferentInt(&gLockV2);

  hitDifferentDispatch(&gLockV1);
  hitDifferentDispatch(&gLockV2);


  recS[0] = (recurStruct*)malloc(sizeof(recurStruct));
  recurseRecStruct(recS[0]);

  recurseRecStructCast((void*)(recS[0]));
  iterRecStructCast((void*)(recS[0]));

  useDDM(&v5, &gLockV5);

  voidNoReturn(&v5, &gLockV5);

  point = (double *) malloc (sizeof(double)*2);
  *(point + 0) = 0.0;
  *(point + 1) = 1.0;
  rect[0] = (double *)malloc (sizeof(double));
  rect[1] = (double *)malloc (sizeof(double));
  *(rect + 0)[0] = 0.0;
  *(rect + 1)[0] = 0.0;
  *(rect + 0)[1] = 1.0;
  *(rect + 1)[1] = 1.0;


  i = pointinrect (point, rect);

  funcWithLabelAtStart ();

  strncpy (buf, testStr, min(strlen(testStr), sizeof(buf) - 1));

  DAC960_SanitizeInquiryData((DAC960_SCSI_Inquiry_T *)0,
                             (DAC960_SCSI_Inquiry_UnitSerialNumber_T *)0,
                             (unsigned char *)0,
                             (unsigned char *)0,
                             (unsigned char *)0,
                             (unsigned char *)0);


 label0:
  y++;
  return 0;

 label1:
  x++;
  return 1;
  


}

