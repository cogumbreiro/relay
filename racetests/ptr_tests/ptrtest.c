#include <stdlib.h>
#include <stddef.h>

typedef unsigned char UBYTE;

typedef unsigned short UWORD;

typedef unsigned long UDWORD;

typedef unsigned int u_int32_t;


static UDWORD CRCTab[256] ;

static short crcInitialized  = (short)0;

typedef struct _pairPtr {
  void *fst;
  void *snd;
} pairPtr;

typedef struct _hashThing {
  unsigned long hash;
  pairPtr kids;
} hashThing;


static void InitCRC(void) 
{ int I ;
  int J ;
  UDWORD C ;

  {

  if (crcInitialized) {

    return;
  }

  I = 0;

  while (I < 256) {

    C = (unsigned long )I;

    J = 0;

    while (J < 8) {

      if (C & 1UL) {

        C = (C >> 1) ^ 3988292384UL;
      } else {

        C = C >> 1;
      }

      J ++;
    }

    CRCTab[I] = C;

    I ++;
  }

  crcInitialized = (short)1;

  return;

  }
}

static UDWORD CalcCRC32(UDWORD StartCRC , UBYTE *Addr , UDWORD Size ) { 
  unsigned int I ;

  I = 0U;

  while ((unsigned long )I < Size) {
    
    StartCRC = 
      CRCTab[(int )((unsigned char )StartCRC) ^ 
             (int )(*(Addr + I))] 
      ^ (StartCRC >> 8);

    I ++;
  }

  return (StartCRC);
}

struct AudioVariables {
   int K1 ;
   int K2 ;
   int K3 ;
   int K4 ;
   int K5 ;
   int D1 ;
   int D2 ;
   int D3 ;
   int D4 ;
   int LastDelta ;
   unsigned int Dif[11] ;
   unsigned int ByteCount ;
   int LastChar ;
};

static struct AudioVariables AudV[4]  ;

static int CurChannel  ;

static int ChannelDelta  ;


static UBYTE DecodeAudio(int Delta ) 
{ struct AudioVariables *V ;
  unsigned int Ch ;
  unsigned int NumMinDif ;
  unsigned int MinDif ;
  int PCh ;
  int I ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;
  int tmp___5 ;
  int tmp___6 ;
  int tmp___7 ;
  int tmp___8 ;
  int tmp___9 ;
  
  {
    
  V = & AudV[CurChannel];

  V->ByteCount ++;

  V->D4 = V->D3;

  V->D3 = V->D2;

  V->D2 = V->LastDelta - V->D1;

  V->D1 = V->LastDelta;

  PCh = ((((8 * V->LastChar + V->K1 * V->D1) + V->K2 * V->D2) + V->K3 * V->D3) + V->K4 *
                                                                                 V->D4) +
        V->K5 * ChannelDelta;

  PCh = (PCh >> 3) & 255;

  Ch = (unsigned int )(PCh - Delta);

  I = (int )((signed char )Delta) << 3;

  tmp = abs(I);

  V->Dif[0] += (unsigned int )tmp;

  tmp___0 = abs(I - V->D1);

  V->Dif[1] += (unsigned int )tmp___0;

  tmp___1 = abs(I + V->D1);

  V->Dif[2] += (unsigned int )tmp___1;

  tmp___2 = abs(I - V->D2);

  V->Dif[3] += (unsigned int )tmp___2;

  tmp___3 = abs(I + V->D2);

  V->Dif[4] += (unsigned int )tmp___3;

  tmp___4 = abs(I - V->D3);

  V->Dif[5] += (unsigned int )tmp___4;

  tmp___5 = abs(I + V->D3);

  V->Dif[6] += (unsigned int )tmp___5;

  tmp___6 = abs(I - V->D4);

  V->Dif[7] += (unsigned int )tmp___6;

  tmp___7 = abs(I + V->D4);

  V->Dif[8] += (unsigned int )tmp___7;

  tmp___8 = abs(I - ChannelDelta);

  V->Dif[9] += (unsigned int )tmp___8;

  tmp___9 = abs(I + ChannelDelta);

  V->Dif[10] += (unsigned int )tmp___9;

  V->LastDelta = (int )((signed char )(Ch - (unsigned int )V->LastChar));

  ChannelDelta = V->LastDelta;

  V->LastChar = (int )Ch;

  if ((V->ByteCount & 31U) == 0U) {

    MinDif = V->Dif[0];

    NumMinDif = 0U;

    V->Dif[0] = 0U;

    I = 1;

    while ((unsigned int )I < sizeof(V->Dif) / sizeof(V->Dif[0])) {

      if (V->Dif[I] < MinDif) {

        MinDif = V->Dif[I];

        NumMinDif = (unsigned int )I;
      }

      V->Dif[I] = 0U;

      I ++;
    }

    switch ((int )NumMinDif) {
    case 1: ;

    if (V->K1 >= -16) {

      V->K1 --;
    }

    break;
    case 2: ;

    if (V->K1 < 16) {

      V->K1 ++;
    }

    break;
    case 3: ;

    if (V->K2 >= -16) {

      V->K2 --;
    }

    break;
    case 4: ;

    if (V->K2 < 16) {

      V->K2 ++;
    }

    break;
    case 5: ;

    if (V->K3 >= -16) {

      V->K3 --;
    }

    break;
    case 6: ;

    if (V->K3 < 16) {

      V->K3 ++;
    }

    break;
    case 7: ;

    if (V->K4 >= -16) {

      V->K4 --;
    }

    break;
    case 8: ;

    if (V->K4 < 16) {

      V->K4 ++;
    }

    break;
    case 9: ;

    if (V->K5 >= -16) {

      V->K5 --;
    }

    break;
    case 10: ;

    if (V->K5 < 16) {

      V->K5 ++;
    }

    break;
    }
  }

  return ((unsigned char )Ch);
}
}


void updateHash(hashThing *thing) {
  thing->hash = (unsigned long) thing;

  return;
}

void updateHashVoid (void *vp) {
  if(vp) {
    updateHash((hashThing *)vp);
    updateHashVoid((void *)(((hashThing*)vp)->kids.fst));
  }
  return;
}

void assignHash (pairPtr *p) {
  p->fst = p;
  p->snd = p;
}

void assignFstHash (hashThing *thing) {
  assignHash (&((hashThing *)(thing->kids).fst)->kids);
}

/////////////////////////////////////////////////////////////////////
///// FROM LIBDB


typedef unsigned int size_t;
typedef unsigned int __u_int;
typedef unsigned long __u_long;
typedef long long __quad_t;
typedef long __off_t;
typedef __quad_t __off64_t;
typedef int __pid_t;
typedef long __time_t;
typedef long __suseconds_t;
typedef int __ssize_t;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __ssize_t ssize_t;
typedef __time_t time_t;
typedef int int32_t;
typedef unsigned char u_int8_t;
typedef unsigned int u_int32_t;
struct timeval {
   __time_t tv_sec ;
   __suseconds_t tv_usec ;
};
typedef long __fd_mask;
struct __anonstruct_fd_set_3 {
   __fd_mask __fds_bits[(int )(1024U / (8U * sizeof(__fd_mask )))] ;
};
typedef struct __anonstruct_fd_set_3 fd_set;
struct __sched_param {
   int __sched_priority ;
};
struct __pthread_attr_s {
   int __detachstate ;
   int __schedpolicy ;
   struct __sched_param __schedparam ;
   int __inheritsched ;
   int __scope ;
   size_t __guardsize ;
   int __stackaddr_set ;
   void *__stackaddr ;
   size_t __stacksize ;
};
typedef struct __pthread_attr_s pthread_attr_t;
typedef unsigned long pthread_t;
typedef void (*__sighandler_t)(int  );
struct _IO_FILE;
typedef struct _IO_FILE FILE;
typedef void _IO_lock_t;
struct _IO_marker {
   struct _IO_marker *_next ;
   struct _IO_FILE *_sbuf ;
   int _pos ;
};
struct _IO_FILE {
   int _flags ;
   char *_IO_read_ptr ;
   char *_IO_read_end ;
   char *_IO_read_base ;
   char *_IO_write_base ;
   char *_IO_write_ptr ;
   char *_IO_write_end ;
   char *_IO_buf_base ;
   char *_IO_buf_end ;
   char *_IO_save_base ;
   char *_IO_backup_base ;
   char *_IO_save_end ;
   struct _IO_marker *_markers ;
   struct _IO_FILE *_chain ;
   int _fileno ;
   int _flags2 ;
   __off_t _old_offset ;
   unsigned short _cur_column ;
   signed char _vtable_offset ;
   char _shortbuf[1] ;
   _IO_lock_t *_lock ;
   __off64_t _offset ;
   void *__pad1 ;
   void *__pad2 ;
   int _mode ;
   char _unused2[(int )(15U * sizeof(int ) - 2U * sizeof(void *))] ;
};
typedef u_int32_t db_pgno_t;
typedef u_int32_t db_recno_t;
typedef u_int32_t db_timeout_t;
typedef u_int32_t roff_t;
struct __db;
typedef struct __db DB;
struct __db_dbt;
typedef struct __db_dbt DBT;
struct __db_env;
typedef struct __db_env DB_ENV;
struct __db_ilock;
typedef struct __db_ilock DB_LOCK_ILOCK;
struct __db_lock_stat;
typedef struct __db_lock_stat DB_LOCK_STAT;
struct __db_lock_u;
typedef struct __db_lock_u DB_LOCK;
struct __db_lockreq;
typedef struct __db_lockreq DB_LOCKREQ;
struct __db_log_cursor;
typedef struct __db_log_cursor DB_LOGC;
struct __db_log_stat;
typedef struct __db_log_stat DB_LOG_STAT;
struct __db_lsn;
typedef struct __db_lsn DB_LSN;
struct __db_mpool_fstat;
typedef struct __db_mpool_fstat DB_MPOOL_FSTAT;
struct __db_mpool_stat;
typedef struct __db_mpool_stat DB_MPOOL_STAT;
struct __db_mpoolfile;
typedef struct __db_mpoolfile DB_MPOOLFILE;
struct __db_preplist;
typedef struct __db_preplist DB_PREPLIST;
struct __db_rep_stat;
typedef struct __db_rep_stat DB_REP_STAT;
struct __db_txn;
typedef struct __db_txn DB_TXN;
struct __db_txn_active;
typedef struct __db_txn_active DB_TXN_ACTIVE;
struct __db_txn_stat;
typedef struct __db_txn_stat DB_TXN_STAT;
struct __db_txnmgr;
typedef struct __db_txnmgr DB_TXNMGR;
struct __dbc;
typedef struct __dbc DBC;
struct __dbc_internal;
typedef struct __dbc_internal DBC_INTERNAL;
struct __fh_t;
typedef struct __fh_t DB_FH;
struct __fname;
typedef struct __fname FNAME;
struct __key_range;
typedef struct __key_range DB_KEY_RANGE;
struct __mpoolfile;
typedef struct __mpoolfile MPOOLFILE;
struct __mutex_t;
typedef struct __mutex_t DB_MUTEX;
struct __db_dbt {
   void *data ;
   u_int32_t size ;
   u_int32_t ulen ;
   u_int32_t dlen ;
   u_int32_t doff ;
   u_int32_t flags ;
};
enum __anonenum_db_lockmode_t_50 {
    DB_LOCK_NG = 0,
    DB_LOCK_READ = 1,
    DB_LOCK_WRITE = 2,
    DB_LOCK_WAIT = 3,
    DB_LOCK_IWRITE = 4,
    DB_LOCK_IREAD = 5,
    DB_LOCK_IWR = 6,
    DB_LOCK_DIRTY = 7,
    DB_LOCK_WWRITE = 8
};
typedef enum __anonenum_db_lockmode_t_50 db_lockmode_t;
enum __anonenum_db_lockop_t_51 {
    DB_LOCK_DUMP = 0,
    DB_LOCK_GET = 1,
    DB_LOCK_GET_TIMEOUT = 2,
    DB_LOCK_INHERIT = 3,
    DB_LOCK_PUT = 4,
    DB_LOCK_PUT_ALL = 5,
    DB_LOCK_PUT_OBJ = 6,
    DB_LOCK_PUT_READ = 7,
    DB_LOCK_TIMEOUT = 8,
    DB_LOCK_TRADE = 9,
    DB_LOCK_UPGRADE_WRITE = 10
};
typedef enum __anonenum_db_lockop_t_51 db_lockop_t;
struct __db_lock_stat {
   u_int32_t st_id ;
   u_int32_t st_cur_maxid ;
   u_int32_t st_maxlocks ;
   u_int32_t st_maxlockers ;
   u_int32_t st_maxobjects ;
   u_int32_t st_nmodes ;
   u_int32_t st_nlocks ;
   u_int32_t st_maxnlocks ;
   u_int32_t st_nlockers ;
   u_int32_t st_maxnlockers ;
   u_int32_t st_nobjects ;
   u_int32_t st_maxnobjects ;
   u_int32_t st_nconflicts ;
   u_int32_t st_nrequests ;
   u_int32_t st_nreleases ;
   u_int32_t st_nnowaits ;
   u_int32_t st_ndeadlocks ;
   db_timeout_t st_locktimeout ;
   u_int32_t st_nlocktimeouts ;
   db_timeout_t st_txntimeout ;
   u_int32_t st_ntxntimeouts ;
   u_int32_t st_region_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_regsize ;
};
struct __db_ilock {
   db_pgno_t pgno ;
   u_int8_t fileid[20] ;
   u_int32_t type ;
};
struct __db_lock_u {
   size_t off ;
   u_int32_t ndx ;
   u_int32_t gen ;
   db_lockmode_t mode ;
};
struct __db_lockreq {
   db_lockop_t op ;
   db_lockmode_t mode ;
   db_timeout_t timeout ;
   DBT *obj ;
   DB_LOCK lock ;
};
struct __db_lsn {
   u_int32_t file ;
   u_int32_t offset ;
};
struct __db_log_cursor {
   DB_ENV *dbenv ;
   DB_FH *c_fhp ;
   DB_LSN c_lsn ;
   u_int32_t c_len ;
   u_int32_t c_prev ;
   DBT c_dbt ;
   u_int8_t *bp ;
   u_int32_t bp_size ;
   u_int32_t bp_rlen ;
   DB_LSN bp_lsn ;
   u_int32_t bp_maxrec ;
   int (*close)(DB_LOGC * , u_int32_t  ) ;
   int (*get)(DB_LOGC * , DB_LSN * , DBT * , u_int32_t  ) ;
   u_int32_t flags ;
};
struct __db_log_stat {
   u_int32_t st_magic ;
   u_int32_t st_version ;
   int st_mode ;
   u_int32_t st_lg_bsize ;
   u_int32_t st_lg_size ;
   u_int32_t st_w_bytes ;
   u_int32_t st_w_mbytes ;
   u_int32_t st_wc_bytes ;
   u_int32_t st_wc_mbytes ;
   u_int32_t st_wcount ;
   u_int32_t st_wcount_fill ;
   u_int32_t st_scount ;
   u_int32_t st_region_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_cur_file ;
   u_int32_t st_cur_offset ;
   u_int32_t st_disk_file ;
   u_int32_t st_disk_offset ;
   u_int32_t st_regsize ;
   u_int32_t st_maxcommitperflush ;
   u_int32_t st_mincommitperflush ;
};
enum __anonenum_DB_CACHE_PRIORITY_53 {
    DB_PRIORITY_VERY_LOW = 1,
    DB_PRIORITY_LOW = 2,
    DB_PRIORITY_DEFAULT = 3,
    DB_PRIORITY_HIGH = 4,
    DB_PRIORITY_VERY_HIGH = 5
};
typedef enum __anonenum_DB_CACHE_PRIORITY_53 DB_CACHE_PRIORITY;
struct __anonstruct_q_54 {
   struct __db_mpoolfile *tqe_next ;
   struct __db_mpoolfile **tqe_prev ;
};
struct __db_mpoolfile {
   DB_FH *fhp ;
   u_int32_t ref ;
   u_int32_t pinref ;
   struct __anonstruct_q_54 q ;
   DB_ENV *dbenv ;
   MPOOLFILE *mfp ;
   u_int32_t clear_len ;
   u_int8_t fileid[20] ;
   int ftype ;
   int32_t lsn_offset ;
   u_int32_t gbytes ;
   u_int32_t bytes ;
   DBT *pgcookie ;
   DB_CACHE_PRIORITY priority ;
   void *addr ;
   size_t len ;
   u_int32_t config_flags ;
   int (*close)(DB_MPOOLFILE * , u_int32_t  ) ;
   int (*get)(DB_MPOOLFILE * , db_pgno_t * , u_int32_t  , void * ) ;
   int (*open)(DB_MPOOLFILE * , char const   * , u_int32_t  , int  , size_t  ) ;
   int (*put)(DB_MPOOLFILE * , void * , u_int32_t  ) ;
   int (*set)(DB_MPOOLFILE * , void * , u_int32_t  ) ;
   int (*get_clear_len)(DB_MPOOLFILE * , u_int32_t * ) ;
   int (*set_clear_len)(DB_MPOOLFILE * , u_int32_t  ) ;
   int (*get_fileid)(DB_MPOOLFILE * , u_int8_t * ) ;
   int (*set_fileid)(DB_MPOOLFILE * , u_int8_t * ) ;
   int (*get_flags)(DB_MPOOLFILE * , u_int32_t * ) ;
   int (*set_flags)(DB_MPOOLFILE * , u_int32_t  , int  ) ;
   int (*get_ftype)(DB_MPOOLFILE * , int * ) ;
   int (*set_ftype)(DB_MPOOLFILE * , int  ) ;
   int (*get_lsn_offset)(DB_MPOOLFILE * , int32_t * ) ;
   int (*set_lsn_offset)(DB_MPOOLFILE * , int32_t  ) ;
   int (*get_maxsize)(DB_MPOOLFILE * , u_int32_t * , u_int32_t * ) ;
   int (*set_maxsize)(DB_MPOOLFILE * , u_int32_t  , u_int32_t  ) ;
   int (*get_pgcookie)(DB_MPOOLFILE * , DBT * ) ;
   int (*set_pgcookie)(DB_MPOOLFILE * , DBT * ) ;
   int (*get_priority)(DB_MPOOLFILE * , DB_CACHE_PRIORITY * ) ;
   int (*set_priority)(DB_MPOOLFILE * , DB_CACHE_PRIORITY  ) ;
   int (*sync)(DB_MPOOLFILE * ) ;
   u_int32_t flags ;
};
struct __db_mpool_stat {
   u_int32_t st_gbytes ;
   u_int32_t st_bytes ;
   u_int32_t st_ncache ;
   u_int32_t st_regsize ;
   u_int32_t st_map ;
   u_int32_t st_cache_hit ;
   u_int32_t st_cache_miss ;
   u_int32_t st_page_create ;
   u_int32_t st_page_in ;
   u_int32_t st_page_out ;
   u_int32_t st_ro_evict ;
   u_int32_t st_rw_evict ;
   u_int32_t st_page_trickle ;
   u_int32_t st_pages ;
   u_int32_t st_page_clean ;
   u_int32_t st_page_dirty ;
   u_int32_t st_hash_buckets ;
   u_int32_t st_hash_searches ;
   u_int32_t st_hash_longest ;
   u_int32_t st_hash_examined ;
   u_int32_t st_hash_nowait ;
   u_int32_t st_hash_wait ;
   u_int32_t st_hash_max_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_region_wait ;
   u_int32_t st_alloc ;
   u_int32_t st_alloc_buckets ;
   u_int32_t st_alloc_max_buckets ;
   u_int32_t st_alloc_pages ;
   u_int32_t st_alloc_max_pages ;
};
struct __db_mpool_fstat {
   char *file_name ;
   size_t st_pagesize ;
   u_int32_t st_map ;
   u_int32_t st_cache_hit ;
   u_int32_t st_cache_miss ;
   u_int32_t st_page_create ;
   u_int32_t st_page_in ;
   u_int32_t st_page_out ;
};
enum __anonenum_db_recops_55 {
    DB_TXN_ABORT = 0,
    DB_TXN_APPLY = 1,
    DB_TXN_BACKWARD_ALLOC = 2,
    DB_TXN_BACKWARD_ROLL = 3,
    DB_TXN_FORWARD_ROLL = 4,
    DB_TXN_GETPGNOS = 5,
    DB_TXN_OPENFILES = 6,
    DB_TXN_POPENFILES = 7,
    DB_TXN_PRINT = 8
};
typedef enum __anonenum_db_recops_55 db_recops;
struct __anonstruct_links_56 {
   struct __db_txn *tqe_next ;
   struct __db_txn **tqe_prev ;
};
struct __anonstruct_xalinks_57 {
   struct __db_txn *tqe_next ;
   struct __db_txn **tqe_prev ;
};
struct __txn_event;
struct __anonstruct_events_58 {
   struct __txn_event *tqh_first ;
   struct __txn_event **tqh_last ;
};
struct __txn_logrec;
struct __anonstruct_logs_59 {
   struct __txn_logrec *stqh_first ;
   struct __txn_logrec **stqh_last ;
};
struct __kids {
   struct __db_txn *tqh_first ;
   struct __db_txn **tqh_last ;
};
struct __anonstruct_klinks_60 {
   struct __db_txn *tqe_next ;
   struct __db_txn **tqe_prev ;
};
struct __db_txn {
   DB_TXNMGR *mgrp ;
   DB_TXN *parent ;
   DB_LSN last_lsn ;
   u_int32_t txnid ;
   u_int32_t tid ;
   roff_t off ;
   db_timeout_t lock_timeout ;
   db_timeout_t expire ;
   void *txn_list ;
   struct __anonstruct_links_56 links ;
   struct __anonstruct_xalinks_57 xalinks ;
   struct __anonstruct_events_58 events ;
   struct __anonstruct_logs_59 logs ;
   struct __kids kids ;
   struct __anonstruct_klinks_60 klinks ;
   void *api_internal ;
   u_int32_t cursors ;
   int (*abort)(DB_TXN * ) ;
   int (*commit)(DB_TXN * , u_int32_t  ) ;
   int (*discard)(DB_TXN * , u_int32_t  ) ;
   u_int32_t (*id)(DB_TXN * ) ;
   int (*prepare)(DB_TXN * , u_int8_t * ) ;
   int (*set_timeout)(DB_TXN * , db_timeout_t  , u_int32_t  ) ;
   u_int32_t flags ;
};
struct __db_preplist {
   DB_TXN *txn ;
   u_int8_t gid[128] ;
};
struct __db_txn_active {
   u_int32_t txnid ;
   u_int32_t parentid ;
   DB_LSN lsn ;
   u_int32_t xa_status ;
   u_int8_t xid[128] ;
};
struct __db_txn_stat {
   DB_LSN st_last_ckp ;
   time_t st_time_ckp ;
   u_int32_t st_last_txnid ;
   u_int32_t st_maxtxns ;
   u_int32_t st_naborts ;
   u_int32_t st_nbegins ;
   u_int32_t st_ncommits ;
   u_int32_t st_nactive ;
   u_int32_t st_nrestores ;
   u_int32_t st_maxnactive ;
   DB_TXN_ACTIVE *st_txnarray ;
   u_int32_t st_region_wait ;
   u_int32_t st_region_nowait ;
   u_int32_t st_regsize ;
};
struct __db_rep_stat {
   u_int32_t st_status ;
   DB_LSN st_next_lsn ;
   DB_LSN st_waiting_lsn ;
   u_int32_t st_dupmasters ;
   int st_env_id ;
   int st_env_priority ;
   u_int32_t st_gen ;
   u_int32_t st_in_recovery ;
   u_int32_t st_log_duplicated ;
   u_int32_t st_log_queued ;
   u_int32_t st_log_queued_max ;
   u_int32_t st_log_queued_total ;
   u_int32_t st_log_records ;
   u_int32_t st_log_requested ;
   int st_master ;
   u_int32_t st_master_changes ;
   u_int32_t st_msgs_badgen ;
   u_int32_t st_msgs_processed ;
   u_int32_t st_msgs_recover ;
   u_int32_t st_msgs_send_failures ;
   u_int32_t st_msgs_sent ;
   u_int32_t st_newsites ;
   int st_nsites ;
   u_int32_t st_nthrottles ;
   u_int32_t st_outdated ;
   u_int32_t st_txns_applied ;
   u_int32_t st_elections ;
   u_int32_t st_elections_won ;
   int st_election_cur_winner ;
   u_int32_t st_election_gen ;
   DB_LSN st_election_lsn ;
   int st_election_nsites ;
   int st_election_priority ;
   int st_election_status ;
   int st_election_tiebreaker ;
   int st_election_votes ;
};
enum __anonenum_DBTYPE_61 {
    DB_BTREE = 1,
    DB_HASH = 2,
    DB_RECNO = 3,
    DB_QUEUE = 4,
    DB_UNKNOWN = 5
};
typedef enum __anonenum_DBTYPE_61 DBTYPE;
struct __anonstruct_dblistlinks_62 {
   struct __db *le_next ;
   struct __db **le_prev ;
};
struct __cq_fq {
   struct __dbc *tqh_first ;
   struct __dbc **tqh_last ;
};
struct __cq_aq {
   struct __dbc *tqh_first ;
   struct __dbc **tqh_last ;
};
struct __cq_jq {
   struct __dbc *tqh_first ;
   struct __dbc **tqh_last ;
};
struct __anonstruct_s_secondaries_63 {
   struct __db *lh_first ;
};
struct __anonstruct_s_links_64 {
   struct __db *le_next ;
   struct __db **le_prev ;
};
struct __db {
   u_int32_t pgsize ;
   int (*db_append_recno)(DB * , DBT * , db_recno_t  ) ;
   void (*db_feedback)(DB * , int  , int  ) ;
   int (*dup_compare)(DB * , DBT const   * , DBT const   * ) ;
   void *app_private ;
   DB_ENV *dbenv ;
   DBTYPE type ;
   DB_MPOOLFILE *mpf ;
   DB_MUTEX *mutexp ;
   char *fname ;
   char *dname ;
   u_int32_t open_flags ;
   u_int8_t fileid[20] ;
   u_int32_t adj_fileid ;
   FNAME *log_filename ;
   db_pgno_t meta_pgno ;
   u_int32_t lid ;
   u_int32_t cur_lid ;
   u_int32_t associate_lid ;
   DB_LOCK handle_lock ;
   long cl_id ;
   time_t timestamp ;
   DBT my_rskey ;
   DBT my_rkey ;
   DBT my_rdata ;
   DB_FH *saved_open_fhp ;
   struct __anonstruct_dblistlinks_62 dblistlinks ;
   struct __cq_fq free_queue ;
   struct __cq_aq active_queue ;
   struct __cq_jq join_queue ;
   struct __anonstruct_s_secondaries_63 s_secondaries ;
   struct __anonstruct_s_links_64 s_links ;
   u_int32_t s_refcnt ;
   int (*s_callback)(DB * , DBT const   * , DBT const   * , DBT * ) ;
   DB *s_primary ;
   void *api_internal ;
   void *bt_internal ;
   void *h_internal ;
   void *q_internal ;
   void *xa_internal ;
   int (*associate)(DB * , DB_TXN * , DB * , int (*)(DB * , DBT const   * , DBT const   * ,
                                                     DBT * ) , u_int32_t  ) ;
   int (*close)(DB * , u_int32_t  ) ;
   int (*cursor)(DB * , DB_TXN * , DBC ** , u_int32_t  ) ;
   int (*del)(DB * , DB_TXN * , DBT * , u_int32_t  ) ;
   void (*err)(DB * , int  , char const   *  , ...) ;
   void (*errx)(DB * , char const   *  , ...) ;
   int (*fd)(DB * , int * ) ;
   int (*get)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
   int (*pget)(DB * , DB_TXN * , DBT * , DBT * , DBT * , u_int32_t  ) ;
   int (*get_byteswapped)(DB * , int * ) ;
   int (*get_cachesize)(DB * , u_int32_t * , u_int32_t * , int * ) ;
   int (*get_dbname)(DB * , char const   ** , char const   ** ) ;
   int (*get_encrypt_flags)(DB * , u_int32_t * ) ;
   int (*get_env)(DB * , DB_ENV ** ) ;
   void (*get_errfile)(DB * , FILE ** ) ;
   void (*get_errpfx)(DB * , char const   ** ) ;
   int (*get_flags)(DB * , u_int32_t * ) ;
   int (*get_lorder)(DB * , int * ) ;
   int (*get_open_flags)(DB * , u_int32_t * ) ;
   int (*get_pagesize)(DB * , u_int32_t * ) ;
   int (*get_transactional)(DB * , int * ) ;
   int (*get_type)(DB * , DBTYPE * ) ;
   int (*join)(DB * , DBC ** , DBC ** , u_int32_t  ) ;
   int (*key_range)(DB * , DB_TXN * , DBT * , DB_KEY_RANGE * , u_int32_t  ) ;
   int (*open)(DB * , DB_TXN * , char const   * , char const   * , DBTYPE  , u_int32_t  ,
               int  ) ;
   int (*put)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
   int (*remove)(DB * , char const   * , char const   * , u_int32_t  ) ;
   int (*rename)(DB * , char const   * , char const   * , char const   * , u_int32_t  ) ;
   int (*truncate)(DB * , DB_TXN * , u_int32_t * , u_int32_t  ) ;
   int (*set_append_recno)(DB * , int (*)(DB * , DBT * , db_recno_t  ) ) ;
   int (*set_alloc)(DB * , void *(*)(size_t  ) , void *(*)(void * , size_t  ) , void (*)(void * ) ) ;
   int (*set_cachesize)(DB * , u_int32_t  , u_int32_t  , int  ) ;
   int (*set_dup_compare)(DB * , int (*)(DB * , DBT const   * , DBT const   * ) ) ;
   int (*set_encrypt)(DB * , char const   * , u_int32_t  ) ;
   void (*set_errcall)(DB * , void (*)(char const   * , char * ) ) ;
   void (*set_errfile)(DB * , FILE * ) ;
   void (*set_errpfx)(DB * , char const   * ) ;
   int (*set_feedback)(DB * , void (*)(DB * , int  , int  ) ) ;
   int (*set_flags)(DB * , u_int32_t  ) ;
   int (*set_lorder)(DB * , int  ) ;
   int (*set_pagesize)(DB * , u_int32_t  ) ;
   int (*set_paniccall)(DB * , void (*)(DB_ENV * , int  ) ) ;
   int (*stat)(DB * , void * , u_int32_t  ) ;
   int (*sync)(DB * , u_int32_t  ) ;
   int (*upgrade)(DB * , char const   * , u_int32_t  ) ;
   int (*verify)(DB * , char const   * , char const   * , FILE * , u_int32_t  ) ;
   int (*get_bt_minkey)(DB * , u_int32_t * ) ;
   int (*set_bt_compare)(DB * , int (*)(DB * , DBT const   * , DBT const   * ) ) ;
   int (*set_bt_maxkey)(DB * , u_int32_t  ) ;
   int (*set_bt_minkey)(DB * , u_int32_t  ) ;
   int (*set_bt_prefix)(DB * , size_t (*)(DB * , DBT const   * , DBT const   * ) ) ;
   int (*get_h_ffactor)(DB * , u_int32_t * ) ;
   int (*get_h_nelem)(DB * , u_int32_t * ) ;
   int (*set_h_ffactor)(DB * , u_int32_t  ) ;
   int (*set_h_hash)(DB * , u_int32_t (*)(DB * , void const   * , u_int32_t  ) ) ;
   int (*set_h_nelem)(DB * , u_int32_t  ) ;
   int (*get_re_delim)(DB * , int * ) ;
   int (*get_re_len)(DB * , u_int32_t * ) ;
   int (*get_re_pad)(DB * , int * ) ;
   int (*get_re_source)(DB * , char const   ** ) ;
   int (*set_re_delim)(DB * , int  ) ;
   int (*set_re_len)(DB * , u_int32_t  ) ;
   int (*set_re_pad)(DB * , int  ) ;
   int (*set_re_source)(DB * , char const   * ) ;
   int (*get_q_extentsize)(DB * , u_int32_t * ) ;
   int (*set_q_extentsize)(DB * , u_int32_t  ) ;
   int (*db_am_remove)(DB * , DB_TXN * , char const   * , char const   * , DB_LSN * ) ;
   int (*db_am_rename)(DB * , DB_TXN * , char const   * , char const   * , char const   * ) ;
   int (*stored_get)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
   int (*stored_close)(DB * , u_int32_t  ) ;
   u_int32_t am_ok ;
   u_int32_t orig_flags ;
   u_int32_t flags ;
};
struct __anonstruct_links_65 {
   DBC *tqe_next ;
   DBC **tqe_prev ;
};
struct __dbc {
   DB *dbp ;
   DB_TXN *txn ;
   struct __anonstruct_links_65 links ;
   DBT *rskey ;
   DBT *rkey ;
   DBT *rdata ;
   DBT my_rskey ;
   DBT my_rkey ;
   DBT my_rdata ;
   u_int32_t lid ;
   u_int32_t locker ;
   DBT lock_dbt ;
   DB_LOCK_ILOCK lock ;
   DB_LOCK mylock ;
   long cl_id ;
   DBTYPE dbtype ;
   DBC_INTERNAL *internal ;
   int (*c_close)(DBC * ) ;
   int (*c_count)(DBC * , db_recno_t * , u_int32_t  ) ;
   int (*c_del)(DBC * , u_int32_t  ) ;
   int (*c_dup)(DBC * , DBC ** , u_int32_t  ) ;
   int (*c_get)(DBC * , DBT * , DBT * , u_int32_t  ) ;
   int (*c_pget)(DBC * , DBT * , DBT * , DBT * , u_int32_t  ) ;
   int (*c_put)(DBC * , DBT * , DBT * , u_int32_t  ) ;
   int (*c_am_bulk)(DBC * , DBT * , u_int32_t  ) ;
   int (*c_am_close)(DBC * , db_pgno_t  , int * ) ;
   int (*c_am_del)(DBC * ) ;
   int (*c_am_destroy)(DBC * ) ;
   int (*c_am_get)(DBC * , DBT * , DBT * , u_int32_t  , db_pgno_t * ) ;
   int (*c_am_put)(DBC * , DBT * , DBT * , u_int32_t  , db_pgno_t * ) ;
   int (*c_am_writelock)(DBC * ) ;
   u_int32_t flags ;
};
struct __key_range {
   double less ;
   double equal ;
   double greater ;
};
struct __anonstruct_dblist_66 {
   struct __db *lh_first ;
};
struct __anonstruct_links_67 {
   struct __db_env *tqe_next ;
   struct __db_env **tqe_prev ;
};
struct __xa_txn {
   struct __db_txn *tqh_first ;
   struct __db_txn **tqh_last ;
};
struct __db_env {
   FILE *db_errfile ;
   char const   *db_errpfx ;
   void (*db_errcall)(char const   * , char * ) ;
   void (*db_feedback)(DB_ENV * , int  , int  ) ;
   void (*db_paniccall)(DB_ENV * , int  ) ;
   void *(*db_malloc)(size_t  ) ;
   void *(*db_realloc)(void * , size_t  ) ;
   void (*db_free)(void * ) ;
   u_int32_t verbose ;
   void *app_private ;
   int (*app_dispatch)(DB_ENV * , DBT * , DB_LSN * , db_recops  ) ;
   u_int8_t *lk_conflicts ;
   u_int32_t lk_modes ;
   u_int32_t lk_max ;
   u_int32_t lk_max_lockers ;
   u_int32_t lk_max_objects ;
   u_int32_t lk_detect ;
   db_timeout_t lk_timeout ;
   u_int32_t lg_bsize ;
   u_int32_t lg_size ;
   u_int32_t lg_regionmax ;
   u_int32_t mp_gbytes ;
   u_int32_t mp_bytes ;
   size_t mp_size ;
   int mp_ncache ;
   size_t mp_mmapsize ;
   int mp_maxwrite ;
   int mp_maxwrite_sleep ;
   int rep_eid ;
   int (*rep_send)(DB_ENV * , DBT const   * , DBT const   * , DB_LSN const   * , int  ,
                   u_int32_t  ) ;
   u_int32_t tx_max ;
   time_t tx_timestamp ;
   db_timeout_t tx_timeout ;
   char *db_home ;
   char *db_log_dir ;
   char *db_tmp_dir ;
   char **db_data_dir ;
   int data_cnt ;
   int data_next ;
   int db_mode ;
   u_int32_t open_flags ;
   void *reginfo ;
   DB_FH *lockfhp ;
   int (**recover_dtab)(DB_ENV * , DBT * , DB_LSN * , db_recops  , void * ) ;
   size_t recover_dtab_size ;
   void *cl_handle ;
   long cl_id ;
   int db_ref ;
   long shm_key ;
   u_int32_t tas_spins ;
   DB_MUTEX *dblist_mutexp ;
   struct __anonstruct_dblist_66 dblist ;
   struct __anonstruct_links_67 links ;
   struct __xa_txn xa_txn ;
   int xa_rmid ;
   void *api1_internal ;
   void *api2_internal ;
   char *passwd ;
   size_t passwd_len ;
   void *crypto_handle ;
   DB_MUTEX *mt_mutexp ;
   int mti ;
   u_long *mt ;
   int (*close)(DB_ENV * , u_int32_t  ) ;
   int (*dbremove)(DB_ENV * , DB_TXN * , char const   * , char const   * , u_int32_t  ) ;
   int (*dbrename)(DB_ENV * , DB_TXN * , char const   * , char const   * , char const   * ,
                   u_int32_t  ) ;
   void (*err)(DB_ENV const   * , int  , char const   *  , ...) ;
   void (*errx)(DB_ENV const   * , char const   *  , ...) ;
   int (*get_home)(DB_ENV * , char const   ** ) ;
   int (*get_open_flags)(DB_ENV * , u_int32_t * ) ;
   int (*open)(DB_ENV * , char const   * , u_int32_t  , int  ) ;
   int (*remove)(DB_ENV * , char const   * , u_int32_t  ) ;
   int (*set_alloc)(DB_ENV * , void *(*)(size_t  ) , void *(*)(void * , size_t  ) ,
                    void (*)(void * ) ) ;
   int (*set_app_dispatch)(DB_ENV * , int (*)(DB_ENV * , DBT * , DB_LSN * , db_recops  ) ) ;
   int (*get_data_dirs)(DB_ENV * , char const   *** ) ;
   int (*set_data_dir)(DB_ENV * , char const   * ) ;
   int (*get_encrypt_flags)(DB_ENV * , u_int32_t * ) ;
   int (*set_encrypt)(DB_ENV * , char const   * , u_int32_t  ) ;
   void (*set_errcall)(DB_ENV * , void (*)(char const   * , char * ) ) ;
   void (*get_errfile)(DB_ENV * , FILE ** ) ;
   void (*set_errfile)(DB_ENV * , FILE * ) ;
   void (*get_errpfx)(DB_ENV * , char const   ** ) ;
   void (*set_errpfx)(DB_ENV * , char const   * ) ;
   int (*set_feedback)(DB_ENV * , void (*)(DB_ENV * , int  , int  ) ) ;
   int (*get_flags)(DB_ENV * , u_int32_t * ) ;
   int (*set_flags)(DB_ENV * , u_int32_t  , int  ) ;
   int (*set_paniccall)(DB_ENV * , void (*)(DB_ENV * , int  ) ) ;
   int (*set_rpc_server)(DB_ENV * , void * , char const   * , long  , long  , u_int32_t  ) ;
   int (*get_shm_key)(DB_ENV * , long * ) ;
   int (*set_shm_key)(DB_ENV * , long  ) ;
   int (*get_tas_spins)(DB_ENV * , u_int32_t * ) ;
   int (*set_tas_spins)(DB_ENV * , u_int32_t  ) ;
   int (*get_tmp_dir)(DB_ENV * , char const   ** ) ;
   int (*set_tmp_dir)(DB_ENV * , char const   * ) ;
   int (*get_verbose)(DB_ENV * , u_int32_t  , int * ) ;
   int (*set_verbose)(DB_ENV * , u_int32_t  , int  ) ;
   void *lg_handle ;
   int (*get_lg_bsize)(DB_ENV * , u_int32_t * ) ;
   int (*set_lg_bsize)(DB_ENV * , u_int32_t  ) ;
   int (*get_lg_dir)(DB_ENV * , char const   ** ) ;
   int (*set_lg_dir)(DB_ENV * , char const   * ) ;
   int (*get_lg_max)(DB_ENV * , u_int32_t * ) ;
   int (*set_lg_max)(DB_ENV * , u_int32_t  ) ;
   int (*get_lg_regionmax)(DB_ENV * , u_int32_t * ) ;
   int (*set_lg_regionmax)(DB_ENV * , u_int32_t  ) ;
   int (*log_archive)(DB_ENV * , char *** , u_int32_t  ) ;
   int (*log_cursor)(DB_ENV * , DB_LOGC ** , u_int32_t  ) ;
   int (*log_file)(DB_ENV * , DB_LSN const   * , char * , size_t  ) ;
   int (*log_flush)(DB_ENV * , DB_LSN const   * ) ;
   int (*log_put)(DB_ENV * , DB_LSN * , DBT const   * , u_int32_t  ) ;
   int (*log_stat)(DB_ENV * , DB_LOG_STAT ** , u_int32_t  ) ;
   void *lk_handle ;
   int (*get_lk_conflicts)(DB_ENV * , u_int8_t const   ** , int * ) ;
   int (*set_lk_conflicts)(DB_ENV * , u_int8_t * , int  ) ;
   int (*get_lk_detect)(DB_ENV * , u_int32_t * ) ;
   int (*set_lk_detect)(DB_ENV * , u_int32_t  ) ;
   int (*set_lk_max)(DB_ENV * , u_int32_t  ) ;
   int (*get_lk_max_locks)(DB_ENV * , u_int32_t * ) ;
   int (*set_lk_max_locks)(DB_ENV * , u_int32_t  ) ;
   int (*get_lk_max_lockers)(DB_ENV * , u_int32_t * ) ;
   int (*set_lk_max_lockers)(DB_ENV * , u_int32_t  ) ;
   int (*get_lk_max_objects)(DB_ENV * , u_int32_t * ) ;
   int (*set_lk_max_objects)(DB_ENV * , u_int32_t  ) ;
   int (*lock_detect)(DB_ENV * , u_int32_t  , u_int32_t  , int * ) ;
   int (*lock_dump_region)(DB_ENV * , char const   * , FILE * ) ;
   int (*lock_get)(DB_ENV * , u_int32_t  , u_int32_t  , DBT const   * , db_lockmode_t  ,
                   DB_LOCK * ) ;
   int (*lock_put)(DB_ENV * , DB_LOCK * ) ;
   int (*lock_id)(DB_ENV * , u_int32_t * ) ;
   int (*lock_id_free)(DB_ENV * , u_int32_t  ) ;
   int (*lock_stat)(DB_ENV * , DB_LOCK_STAT ** , u_int32_t  ) ;
   int (*lock_vec)(DB_ENV * , u_int32_t  , u_int32_t  , DB_LOCKREQ * , int  , DB_LOCKREQ ** ) ;
   void *mp_handle ;
   int (*get_cachesize)(DB_ENV * , u_int32_t * , u_int32_t * , int * ) ;
   int (*set_cachesize)(DB_ENV * , u_int32_t  , u_int32_t  , int  ) ;
   int (*get_mp_mmapsize)(DB_ENV * , size_t * ) ;
   int (*set_mp_mmapsize)(DB_ENV * , size_t  ) ;
   int (*get_mp_maxwrite)(DB_ENV * , int * , int * ) ;
   int (*set_mp_maxwrite)(DB_ENV * , int  , int  ) ;
   int (*memp_dump_region)(DB_ENV * , char const   * , FILE * ) ;
   int (*memp_fcreate)(DB_ENV * , DB_MPOOLFILE ** , u_int32_t  ) ;
   int (*memp_register)(DB_ENV * , int  , int (*)(DB_ENV * , db_pgno_t  , void * ,
                                                  DBT * ) , int (*)(DB_ENV * , db_pgno_t  ,
                                                                    void * , DBT * ) ) ;
   int (*memp_stat)(DB_ENV * , DB_MPOOL_STAT ** , DB_MPOOL_FSTAT *** , u_int32_t  ) ;
   int (*memp_sync)(DB_ENV * , DB_LSN * ) ;
   int (*memp_trickle)(DB_ENV * , int  , int * ) ;
   void *rep_handle ;
   int (*rep_elect)(DB_ENV * , int  , int  , u_int32_t  , int * ) ;
   int (*rep_flush)(DB_ENV * ) ;
   int (*rep_process_message)(DB_ENV * , DBT * , DBT * , int * , DB_LSN * ) ;
   int (*rep_start)(DB_ENV * , DBT * , u_int32_t  ) ;
   int (*rep_stat)(DB_ENV * , DB_REP_STAT ** , u_int32_t  ) ;
   int (*get_rep_limit)(DB_ENV * , u_int32_t * , u_int32_t * ) ;
   int (*set_rep_limit)(DB_ENV * , u_int32_t  , u_int32_t  ) ;
   int (*set_rep_request)(DB_ENV * , u_int32_t  , u_int32_t  ) ;
   int (*set_rep_transport)(DB_ENV * , int  , int (*)(DB_ENV * , DBT const   * , DBT const   * ,
                                                      DB_LSN const   * , int  , u_int32_t  ) ) ;
   void *tx_handle ;
   int (*get_tx_max)(DB_ENV * , u_int32_t * ) ;
   int (*set_tx_max)(DB_ENV * , u_int32_t  ) ;
   int (*get_tx_timestamp)(DB_ENV * , time_t * ) ;
   int (*set_tx_timestamp)(DB_ENV * , time_t * ) ;
   int (*txn_begin)(DB_ENV * , DB_TXN * , DB_TXN ** , u_int32_t  ) ;
   int (*txn_checkpoint)(DB_ENV * , u_int32_t  , u_int32_t  , u_int32_t  ) ;
   int (*txn_recover)(DB_ENV * , DB_PREPLIST * , long  , long * , u_int32_t  ) ;
   int (*txn_stat)(DB_ENV * , DB_TXN_STAT ** , u_int32_t  ) ;
   int (*get_timeout)(DB_ENV * , db_timeout_t * , u_int32_t  ) ;
   int (*set_timeout)(DB_ENV * , db_timeout_t  , u_int32_t  ) ;
   int test_abort ;
   int test_copy ;
   u_int32_t flags ;
};
struct _statistics {
   int aborted ;
   int aborts ;
   int adds ;
   int deletes ;
   int txns ;
   int found ;
   int notfound ;
};
typedef unsigned short u_int16_t;
struct _pthread_fastlock {
   long __status ;
   int __spinlock ;
};
struct _pthread_descr_struct;
typedef struct _pthread_descr_struct *_pthread_descr;
typedef long long __pthread_cond_align_t;
struct __anonstruct_pthread_cond_t_4 {
   struct _pthread_fastlock __c_lock ;
   _pthread_descr __c_waiting ;
   char __padding[(int )(((48U - sizeof(struct _pthread_fastlock )) - sizeof(_pthread_descr )) -
                         sizeof(__pthread_cond_align_t ))] ;
   __pthread_cond_align_t __align ;
};
typedef struct __anonstruct_pthread_cond_t_4 pthread_cond_t;
struct __anonstruct_pthread_condattr_t_5 {
   int __dummy ;
};
typedef struct __anonstruct_pthread_condattr_t_5 pthread_condattr_t;
struct __anonstruct_pthread_mutex_t_6 {
   int __m_reserved ;
   int __m_count ;
   _pthread_descr __m_owner ;
   int __m_kind ;
   struct _pthread_fastlock __m_lock ;
};
typedef struct __anonstruct_pthread_mutex_t_6 pthread_mutex_t;
struct __anonstruct_pthread_mutexattr_t_7 {
   int __mutexkind ;
};
typedef struct __anonstruct_pthread_mutexattr_t_7 pthread_mutexattr_t;
typedef u_int16_t db_indx_t;
struct __dbc_internal {
   DBC *opd ;
   void *page ;
   db_pgno_t root ;
   db_pgno_t pgno ;
   db_indx_t indx ;
   DB_LOCK lock ;
   db_lockmode_t lock_mode ;
};
struct __mutex_t {
   pthread_mutex_t mutex ;
   pthread_cond_t cond ;
   u_int32_t locked ;
   u_int32_t mutex_set_wait ;
   u_int32_t mutex_set_nowait ;
   u_int32_t mutex_set_spin ;
   u_int32_t mutex_set_spins ;
   u_int32_t flags ;
};
struct __fh_t {
   DB_MUTEX *mutexp ;
   int ref ;
   int fd ;
   char *name ;
   u_int32_t pgno ;
   u_int32_t pgsize ;
   u_int32_t offset ;
   u_int8_t flags ;
};
struct __pg_chksum {
   u_int8_t unused[2] ;
   u_int8_t chksum[4] ;
};
typedef struct __pg_chksum PG_CHKSUM;
struct __pg_crypto {
   u_int8_t unused[2] ;
   u_int8_t chksum[20] ;
   u_int8_t iv[16] ;
};
typedef struct __pg_crypto PG_CRYPTO;
struct _db_page {
   DB_LSN lsn ;
   db_pgno_t pgno ;
   db_pgno_t prev_pgno ;
   db_pgno_t next_pgno ;
   db_indx_t entries ;
   db_indx_t hf_offset ;
   u_int8_t level ;
   u_int8_t type ;
};
typedef struct _db_page PAGE;
struct _bkeydata {
   db_indx_t len ;
   u_int8_t type ;
   u_int8_t data[1] ;
};
typedef struct _bkeydata BKEYDATA;
struct _boverflow {
   db_indx_t unused1 ;
   u_int8_t type ;
   u_int8_t unused2 ;
   db_pgno_t pgno ;
   u_int32_t tlen ;
};
typedef struct _boverflow BOVERFLOW;
struct _binternal {
   db_indx_t len ;
   u_int8_t type ;
   u_int8_t unused ;
   db_pgno_t pgno ;
   db_recno_t nrecs ;
   u_int8_t data[1] ;
};
typedef struct _binternal BINTERNAL;
struct __dbpginfo {
   size_t db_pagesize ;
   u_int32_t flags ;
   DBTYPE type ;
};
typedef struct __dbpginfo DB_PGINFO;
struct _dbmeta33 {
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t magic ;
   u_int32_t version ;
   u_int32_t pagesize ;
   u_int8_t encrypt_alg ;
   u_int8_t type ;
   u_int8_t metaflags ;
   u_int8_t unused1 ;
   u_int32_t free ;
   db_pgno_t last_pgno ;
   u_int32_t unused3 ;
   u_int32_t key_count ;
   u_int32_t record_count ;
   u_int32_t flags ;
   u_int8_t uid[20] ;
};
typedef struct _dbmeta33 DBMETA;
struct __db_rep;
typedef struct __db_rep DB_REP;
struct __rep {
   DB_MUTEX mutex ;
   roff_t db_mutex_off ;
   u_int32_t tally_off ;
   u_int32_t v2tally_off ;
   int eid ;
   int master_id ;
   u_int32_t egen ;
   u_int32_t gen ;
   u_int32_t recover_gen ;
   int asites ;
   int nsites ;
   int priority ;
   u_int32_t gbytes ;
   u_int32_t bytes ;
   u_int32_t request_gap ;
   u_int32_t max_gap ;
   u_int32_t msg_th ;
   int start_th ;
   u_int32_t handle_cnt ;
   u_int32_t op_cnt ;
   int in_recovery ;
   time_t timestamp ;
   int sites ;
   int winner ;
   int w_priority ;
   u_int32_t w_gen ;
   DB_LSN w_lsn ;
   int w_tiebreaker ;
   int votes ;
   DB_REP_STAT stat ;
   u_int32_t flags ;
};
typedef struct __rep REP;
struct __db_rep {
   DB_MUTEX *rep_mutexp ;
   DB_MUTEX *db_mutexp ;
   DB *rep_db ;
   REP *region ;
};
struct __cursor;
typedef struct __cursor BTREE_CURSOR;
struct __epg;
typedef struct __epg EPG;
struct __epg {
   PAGE *page ;
   db_indx_t indx ;
   db_indx_t entries ;
   DB_LOCK lock ;
   db_lockmode_t lock_mode ;
};
struct __cursor {
   DBC *opd ;
   void *page ;
   db_pgno_t root ;
   db_pgno_t pgno ;
   db_indx_t indx ;
   DB_LOCK lock ;
   db_lockmode_t lock_mode ;
   EPG *sp ;
   EPG *csp ;
   EPG *esp ;
   EPG stack[5] ;
   db_indx_t ovflsize ;
   db_recno_t recno ;
   u_int32_t order ;
   u_int32_t flags ;
};
enum __anonenum_db_ca_mode_54 {
    DB_CA_DI = 1,
    DB_CA_DUP = 2,
    DB_CA_RSPLIT = 3,
    DB_CA_SPLIT = 4
};
typedef enum __anonenum_db_ca_mode_54 db_ca_mode;
struct _rinternal {
   db_pgno_t pgno ;
   db_recno_t nrecs ;
};
typedef struct _rinternal RINTERNAL;
struct __btree;
typedef struct __btree BTREE;
struct __btree {
   db_pgno_t bt_meta ;
   db_pgno_t bt_root ;
   u_int32_t bt_maxkey ;
   u_int32_t bt_minkey ;
   int (*bt_compare)(DB * , DBT const   * , DBT const   * ) ;
   size_t (*bt_prefix)(DB * , DBT const   * , DBT const   * ) ;
   int re_pad ;
   int re_delim ;
   u_int32_t re_len ;
   char *re_source ;
   db_pgno_t bt_lpgno ;
   int re_modified ;
   FILE *re_fp ;
   int re_eof ;
   db_recno_t re_last ;
};
struct __anonstruct_q_74 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __mpoolfile {
   DB_MUTEX mutex ;
   u_int32_t mpf_cnt ;
   u_int32_t block_cnt ;
   roff_t path_off ;
   int32_t deadfile ;
   struct __anonstruct_q_74 q ;
   db_pgno_t last_pgno ;
   db_pgno_t orig_last_pgno ;
   db_pgno_t maxpgno ;
   int32_t ftype ;
   int32_t priority ;
   int32_t file_written ;
   int32_t no_backing_file ;
   int32_t unlink_on_close ;
   DB_MPOOL_FSTAT stat ;
   int32_t lsn_off ;
   u_int32_t clear_len ;
   roff_t fileid_off ;
   roff_t pgcookie_len ;
   roff_t pgcookie_off ;
   u_int32_t flags ;
};
struct __queue;
typedef struct __queue QUEUE;
struct __qmpf {
   int pinref ;
   DB_MPOOLFILE *mpf ;
};
struct __mpfarray {
   u_int32_t n_extent ;
   u_int32_t low_extent ;
   u_int32_t hi_extent ;
   struct __qmpf *mpfarray ;
};
typedef struct __mpfarray MPFARRAY;
struct __queue {
   db_pgno_t q_meta ;
   db_pgno_t q_root ;
   int re_pad ;
   u_int32_t re_len ;
   u_int32_t rec_page ;
   u_int32_t page_ext ;
   MPFARRAY array1 ;
   MPFARRAY array2 ;
   DBT pgcookie ;
   DB_PGINFO pginfo ;
   char *path ;
   char *name ;
   char *dir ;
   int mode ;
};
struct __db_cipher;
typedef struct __db_cipher DB_CIPHER;
enum __anonenum_APPNAME_39 {
    DB_APP_NONE = 0,
    DB_APP_DATA = 1,
    DB_APP_LOG = 2,
    DB_APP_TMP = 3
};
typedef enum __anonenum_APPNAME_39 APPNAME;
struct __db_reginfo_t;
typedef struct __db_reginfo_t REGINFO;
enum __anonenum_reg_type_50 {
    INVALID_REGION_TYPE = 0,
    REGION_TYPE_ENV = 1,
    REGION_TYPE_LOCK = 2,
    REGION_TYPE_LOG = 3,
    REGION_TYPE_MPOOL = 4,
    REGION_TYPE_MUTEX = 5,
    REGION_TYPE_TXN = 6
};
typedef enum __anonenum_reg_type_50 reg_type;
struct __anonstruct_q_51 {
   ssize_t sle_next ;
   ssize_t sle_prev ;
};
struct __db_region {
   DB_MUTEX mutex ;
   struct __anonstruct_q_51 q ;
   reg_type type ;
   u_int32_t id ;
   roff_t size ;
   roff_t primary ;
   long segid ;
};
typedef struct __db_region REGION;
struct __db_reginfo_t {
   reg_type type ;
   u_int32_t id ;
   int mode ;
   REGION *rp ;
   char *name ;
   void *addr ;
   void *primary ;
   u_int32_t flags ;
};
struct __db_cipher {
   u_int (*adj_size)(size_t  ) ;
   int (*close)(DB_ENV * , void * ) ;
   int (*decrypt)(DB_ENV * , void * , void * , u_int8_t * , size_t  ) ;
   int (*encrypt)(DB_ENV * , void * , void * , u_int8_t * , size_t  ) ;
   int (*init)(DB_ENV * , DB_CIPHER * ) ;
   u_int8_t mac_key[20] ;
   void *data ;
   u_int8_t alg ;
   u_int8_t spare[3] ;
   u_int32_t flags ;
};
struct _btmeta33 {
   DBMETA dbmeta ;
   u_int32_t maxkey ;
   u_int32_t minkey ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int32_t root ;
   u_int32_t unused[92] ;
   u_int32_t crypto_magic ;
   u_int32_t trash[3] ;
   u_int8_t iv[16] ;
   u_int8_t chksum[20] ;
};
typedef struct _btmeta33 BTMETA;
struct __db_entry {
   DB *dbp ;
   int deleted ;
};
typedef struct __db_entry DB_ENTRY;
struct __anonstruct_q_74___0 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __fname {
   struct __anonstruct_q_74___0 q ;
   int32_t id ;
   DBTYPE s_type ;
   roff_t name_off ;
   db_pgno_t meta_pgno ;
   u_int8_t ufid[20] ;
   u_int32_t create_txnid ;
   int is_durable ;
};
struct __db_log;
typedef struct __db_log DB_LOG;
struct __db_log {
   DB_MUTEX *mutexp ;
   DB_ENTRY *dbentry ;
   int32_t dbentry_cnt ;
   u_int32_t lfname ;
   DB_FH *lfhp ;
   u_int8_t *bufp ;
   DB_ENV *dbenv ;
   REGINFO reginfo ;
   u_int32_t flags ;
};
enum __anonenum_ca_recno_arg_53 {
    CA_DELETE = 0,
    CA_IAFTER = 1,
    CA_IBEFORE = 2,
    CA_ICURRENT = 3
};
typedef enum __anonenum_ca_recno_arg_53 ca_recno_arg;
struct ___bam_split_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t left ;
   DB_LSN llsn ;
   db_pgno_t right ;
   DB_LSN rlsn ;
   u_int32_t indx ;
   db_pgno_t npgno ;
   DB_LSN nlsn ;
   db_pgno_t root_pgno ;
   DBT pg ;
   u_int32_t opflags ;
};
typedef struct ___bam_split_args __bam_split_args;
struct ___bam_rsplit_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DBT pgdbt ;
   db_pgno_t root_pgno ;
   db_pgno_t nrec ;
   DBT rootent ;
   DB_LSN rootlsn ;
};
typedef struct ___bam_rsplit_args __bam_rsplit_args;
struct ___bam_adj_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN lsn ;
   u_int32_t indx ;
   u_int32_t indx_copy ;
   u_int32_t is_insert ;
};
typedef struct ___bam_adj_args __bam_adj_args;
struct ___bam_cadjust_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN lsn ;
   u_int32_t indx ;
   int32_t adjust ;
   u_int32_t opflags ;
};
typedef struct ___bam_cadjust_args __bam_cadjust_args;
struct ___bam_cdel_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN lsn ;
   u_int32_t indx ;
};
typedef struct ___bam_cdel_args __bam_cdel_args;
struct ___bam_repl_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN lsn ;
   u_int32_t indx ;
   u_int32_t isdeleted ;
   DBT orig ;
   DBT repl ;
   u_int32_t prefix ;
   u_int32_t suffix ;
};
typedef struct ___bam_repl_args __bam_repl_args;
struct ___bam_root_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t meta_pgno ;
   db_pgno_t root_pgno ;
   DB_LSN meta_lsn ;
};
typedef struct ___bam_root_args __bam_root_args;
struct ___bam_curadj_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_ca_mode mode ;
   db_pgno_t from_pgno ;
   db_pgno_t to_pgno ;
   db_pgno_t left_pgno ;
   u_int32_t first_indx ;
   u_int32_t from_indx ;
   u_int32_t to_indx ;
};
typedef struct ___bam_curadj_args __bam_curadj_args;
struct ___bam_rcuradj_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   ca_recno_arg mode ;
   db_pgno_t root ;
   db_recno_t recno ;
   u_int32_t order ;
};
typedef struct ___bam_rcuradj_args __bam_rcuradj_args;
struct __anonstruct_db_trunc_param_55 {
   DBC *dbc ;
   u_int32_t count ;
};
typedef struct __anonstruct_db_trunc_param_55 db_trunc_param;
typedef struct _IO_FILE _IO_FILE;
enum __anonenum_dir_76 {
    UP = 0,
    DOWN = 1
};
struct __db_bt_stat;
typedef struct __db_bt_stat DB_BTREE_STAT;
struct __db_bt_stat {
   u_int32_t bt_magic ;
   u_int32_t bt_version ;
   u_int32_t bt_metaflags ;
   u_int32_t bt_nkeys ;
   u_int32_t bt_ndata ;
   u_int32_t bt_pagesize ;
   u_int32_t bt_maxkey ;
   u_int32_t bt_minkey ;
   u_int32_t bt_re_len ;
   u_int32_t bt_re_pad ;
   u_int32_t bt_levels ;
   u_int32_t bt_int_pg ;
   u_int32_t bt_leaf_pg ;
   u_int32_t bt_dup_pg ;
   u_int32_t bt_over_pg ;
   u_int32_t bt_free ;
   u_int32_t bt_int_pgfree ;
   u_int32_t bt_leaf_pgfree ;
   u_int32_t bt_dup_pgfree ;
   u_int32_t bt_over_pgfree ;
};
struct _dbmeta31 {
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t magic ;
   u_int32_t version ;
   u_int32_t pagesize ;
   u_int8_t unused1[1] ;
   u_int8_t type ;
   u_int8_t unused2[2] ;
   u_int32_t free ;
   DB_LSN unused3 ;
   u_int32_t key_count ;
   u_int32_t record_count ;
   u_int32_t flags ;
   u_int8_t uid[20] ;
};
typedef struct _dbmeta31 DBMETA31;
struct _btmeta31 {
   DBMETA31 dbmeta ;
   u_int32_t maxkey ;
   u_int32_t minkey ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int32_t root ;
};
typedef struct _btmeta31 BTMETA31;
struct _dbmeta30 {
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t magic ;
   u_int32_t version ;
   u_int32_t pagesize ;
   u_int8_t unused1[1] ;
   u_int8_t type ;
   u_int8_t unused2[2] ;
   u_int32_t free ;
   u_int32_t flags ;
   u_int8_t uid[20] ;
};
typedef struct _dbmeta30 DBMETA30;
struct _btmeta30 {
   DBMETA30 dbmeta ;
   u_int32_t maxkey ;
   u_int32_t minkey ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int32_t root ;
};
typedef struct _btmeta30 BTMETA30;
struct _btmeta2X {
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t magic ;
   u_int32_t version ;
   u_int32_t pagesize ;
   u_int32_t maxkey ;
   u_int32_t minkey ;
   u_int32_t free ;
   u_int32_t flags ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int8_t uid[20] ;
};
typedef struct _btmeta2X BTMETA2X;
struct __lsn_page {
   DB_LSN lsn ;
   int32_t fid ;
   DB_LOCK_ILOCK pgdesc ;
   u_int32_t flags ;
};
typedef struct __lsn_page LSN_PAGE;
struct __txn_recs {
   int npages ;
   int nalloc ;
   LSN_PAGE *array ;
   u_int32_t txnid ;
   u_int32_t lockid ;
};
typedef struct __txn_recs TXN_RECS;
typedef struct __txn_logrec DB_TXNLOGREC;
struct _chain {
   struct __db_txn *tqh_first ;
   struct __db_txn **tqh_last ;
};
struct __db_txnmgr {
   DB_MUTEX *mutexp ;
   struct _chain txn_chain ;
   u_int32_t n_discards ;
   DB_ENV *dbenv ;
   REGINFO reginfo ;
};
struct __anonstruct_links_72 {
   struct __txn_logrec *stqe_next ;
};
struct __txn_logrec {
   struct __anonstruct_links_72 links ;
   u_int8_t data[1] ;
};
struct _hashmeta33 {
   DBMETA dbmeta ;
   u_int32_t max_bucket ;
   u_int32_t high_mask ;
   u_int32_t low_mask ;
   u_int32_t ffactor ;
   u_int32_t nelem ;
   u_int32_t h_charkey ;
   u_int32_t spares[32] ;
   u_int32_t unused[59] ;
   u_int32_t crypto_magic ;
   u_int32_t trash[3] ;
   u_int8_t iv[16] ;
   u_int8_t chksum[20] ;
};
typedef struct _hashmeta33 HMETA;
struct _hkeydata {
   u_int8_t type ;
   u_int8_t data[1] ;
};
typedef struct _hkeydata HKEYDATA;
struct _hoffpage {
   u_int8_t type ;
   u_int8_t unused[3] ;
   db_pgno_t pgno ;
   u_int32_t tlen ;
};
typedef struct _hoffpage HOFFPAGE;
struct _hoffdup {
   u_int8_t type ;
   u_int8_t unused[3] ;
   db_pgno_t pgno ;
};
typedef struct _hoffdup HOFFDUP;
struct hash_t {
   db_pgno_t meta_pgno ;
   u_int32_t h_ffactor ;
   u_int32_t h_nelem ;
   u_int32_t (*h_hash)(DB * , void const   * , u_int32_t  ) ;
};
typedef struct hash_t HASH;
struct cursor_t {
   DBC *opd ;
   void *page ;
   db_pgno_t root ;
   db_pgno_t pgno ;
   db_indx_t indx ;
   DB_LOCK lock ;
   db_lockmode_t lock_mode ;
   DB_LOCK hlock ;
   HMETA *hdr ;
   PAGE *split_buf ;
   db_pgno_t bucket ;
   db_pgno_t lbucket ;
   db_indx_t dup_off ;
   db_indx_t dup_len ;
   db_indx_t dup_tlen ;
   u_int32_t seek_size ;
   db_pgno_t seek_found_page ;
   u_int32_t order ;
   u_int32_t flags ;
};
typedef struct cursor_t HASH_CURSOR;
enum __anonenum_db_ham_mode_65 {
    DB_HAM_CHGPG = 1,
    DB_HAM_DELFIRSTPG = 2,
    DB_HAM_DELMIDPG = 3,
    DB_HAM_DELLASTPG = 4,
    DB_HAM_DUP = 5,
    DB_HAM_SPLIT = 6
};
typedef enum __anonenum_db_ham_mode_65 db_ham_mode;
struct ___ham_insdel_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   int32_t fileid ;
   db_pgno_t pgno ;
   u_int32_t ndx ;
   DB_LSN pagelsn ;
   DBT key ;
   DBT data ;
};
typedef struct ___ham_insdel_args __ham_insdel_args;
struct ___ham_newpage_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   int32_t fileid ;
   db_pgno_t prev_pgno ;
   DB_LSN prevlsn ;
   db_pgno_t new_pgno ;
   DB_LSN pagelsn ;
   db_pgno_t next_pgno ;
   DB_LSN nextlsn ;
};
typedef struct ___ham_newpage_args __ham_newpage_args;
struct ___ham_splitdata_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   u_int32_t opcode ;
   db_pgno_t pgno ;
   DBT pageimage ;
   DB_LSN pagelsn ;
};
typedef struct ___ham_splitdata_args __ham_splitdata_args;
struct ___ham_replace_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   u_int32_t ndx ;
   DB_LSN pagelsn ;
   int32_t off ;
   DBT olditem ;
   DBT newitem ;
   u_int32_t makedup ;
};
typedef struct ___ham_replace_args __ham_replace_args;
struct ___ham_copypage_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN pagelsn ;
   db_pgno_t next_pgno ;
   DB_LSN nextlsn ;
   db_pgno_t nnext_pgno ;
   DB_LSN nnextlsn ;
   DBT page ;
};
typedef struct ___ham_copypage_args __ham_copypage_args;
struct ___ham_metagroup_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   u_int32_t bucket ;
   db_pgno_t mmpgno ;
   DB_LSN mmetalsn ;
   db_pgno_t mpgno ;
   DB_LSN metalsn ;
   db_pgno_t pgno ;
   DB_LSN pagelsn ;
   u_int32_t newalloc ;
};
typedef struct ___ham_metagroup_args __ham_metagroup_args;
struct ___ham_groupalloc_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   DB_LSN meta_lsn ;
   db_pgno_t start_pgno ;
   u_int32_t num ;
   db_pgno_t free ;
};
typedef struct ___ham_groupalloc_args __ham_groupalloc_args;
struct ___ham_curadj_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   u_int32_t indx ;
   u_int32_t len ;
   u_int32_t dup_off ;
   int add ;
   int is_dup ;
   u_int32_t order ;
};
typedef struct ___ham_curadj_args __ham_curadj_args;
struct ___ham_chgpg_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_ham_mode mode ;
   db_pgno_t old_pgno ;
   db_pgno_t new_pgno ;
   u_int32_t old_indx ;
   u_int32_t new_indx ;
};
typedef struct ___ham_chgpg_args __ham_chgpg_args;
struct __db_h_stat;
typedef struct __db_h_stat DB_HASH_STAT;
struct __db_h_stat {
   u_int32_t hash_magic ;
   u_int32_t hash_version ;
   u_int32_t hash_metaflags ;
   u_int32_t hash_nkeys ;
   u_int32_t hash_ndata ;
   u_int32_t hash_pagesize ;
   u_int32_t hash_ffactor ;
   u_int32_t hash_buckets ;
   u_int32_t hash_free ;
   u_int32_t hash_bfree ;
   u_int32_t hash_bigpages ;
   u_int32_t hash_big_bfree ;
   u_int32_t hash_overflows ;
   u_int32_t hash_ovfl_free ;
   u_int32_t hash_dup ;
   u_int32_t hash_dup_free ;
};
enum __anonenum_DB_OS_SEEK_52 {
    DB_OS_SEEK_CUR = 0,
    DB_OS_SEEK_END = 1,
    DB_OS_SEEK_SET = 2
};
typedef enum __anonenum_DB_OS_SEEK_52 DB_OS_SEEK;
struct _hashmeta31 {
   DBMETA31 dbmeta ;
   u_int32_t max_bucket ;
   u_int32_t high_mask ;
   u_int32_t low_mask ;
   u_int32_t ffactor ;
   u_int32_t nelem ;
   u_int32_t h_charkey ;
   u_int32_t spares[32] ;
};
typedef struct _hashmeta31 HMETA31;
struct _hashmeta30 {
   DBMETA30 dbmeta ;
   u_int32_t max_bucket ;
   u_int32_t high_mask ;
   u_int32_t low_mask ;
   u_int32_t ffactor ;
   u_int32_t nelem ;
   u_int32_t h_charkey ;
   u_int32_t spares[32] ;
};
typedef struct _hashmeta30 HMETA30;
struct hashhdr {
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t magic ;
   u_int32_t version ;
   u_int32_t pagesize ;
   u_int32_t ovfl_point ;
   u_int32_t last_freed ;
   u_int32_t max_bucket ;
   u_int32_t high_mask ;
   u_int32_t low_mask ;
   u_int32_t ffactor ;
   u_int32_t nelem ;
   u_int32_t h_charkey ;
   u_int32_t flags ;
   u_int32_t spares[32] ;
   u_int8_t uid[20] ;
};
typedef struct hashhdr HASHHDR;
struct __vrfy_childinfo;
typedef struct __vrfy_childinfo VRFY_CHILDINFO;
struct __vrfy_dbinfo;
typedef struct __vrfy_dbinfo VRFY_DBINFO;
struct __vrfy_pageinfo;
typedef struct __vrfy_pageinfo VRFY_PAGEINFO;
struct __subdbs {
   struct __vrfy_childinfo *lh_first ;
};
struct __activepips {
   struct __vrfy_pageinfo *lh_first ;
};
struct __vrfy_dbinfo {
   DBTYPE type ;
   struct __subdbs subdbs ;
   DB *pgdbp ;
   DB *cdbp ;
   struct __activepips activepips ;
   DB *pgset ;
   DB *salvage_pages ;
   db_pgno_t last_pgno ;
   db_pgno_t pgs_remaining ;
   db_pgno_t prev_pgno ;
   db_pgno_t next_pgno ;
   u_int8_t leaf_type ;
   u_int32_t re_len ;
   u_int32_t rec_page ;
   u_int32_t page_ext ;
   u_int32_t first_recno ;
   u_int32_t last_recno ;
   int nextents ;
   db_pgno_t *extents ;
   u_int32_t flags ;
};
struct __anonstruct_links_53 {
   struct __vrfy_pageinfo *le_next ;
   struct __vrfy_pageinfo **le_prev ;
};
struct __vrfy_pageinfo {
   u_int8_t type ;
   u_int8_t bt_level ;
   u_int8_t unused1 ;
   u_int8_t unused2 ;
   db_pgno_t pgno ;
   db_pgno_t prev_pgno ;
   db_pgno_t next_pgno ;
   db_pgno_t root ;
   db_pgno_t free ;
   db_indx_t entries ;
   u_int16_t unused ;
   db_recno_t rec_cnt ;
   u_int32_t re_len ;
   u_int32_t bt_minkey ;
   u_int32_t bt_maxkey ;
   u_int32_t h_ffactor ;
   u_int32_t h_nelem ;
   u_int32_t refcount ;
   u_int32_t olen ;
   u_int32_t flags ;
   struct __anonstruct_links_53 links ;
   u_int32_t pi_refcount ;
};
struct __anonstruct_links_54 {
   struct __vrfy_childinfo *le_next ;
   struct __vrfy_childinfo **le_prev ;
};
struct __vrfy_childinfo {
   db_pgno_t pgno ;
   u_int32_t type ;
   db_recno_t nrecs ;
   u_int32_t tlen ;
   u_int32_t refcnt ;
   struct __anonstruct_links_54 links ;
};
struct __db_regionh {
   ssize_t slh_first ;
};
struct __db_reg_env {
   DB_MUTEX mutex ;
   u_int32_t magic ;
   int envpanic ;
   int majver ;
   int minver ;
   int patch ;
   u_int32_t init_flags ;
   roff_t cipher_off ;
   struct __db_regionh regionq ;
   u_int32_t refcnt ;
   roff_t rep_off ;
   size_t pad ;
};
typedef struct __db_reg_env REGENV;
struct _qmeta33 {
   DBMETA dbmeta ;
   u_int32_t first_recno ;
   u_int32_t cur_recno ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int32_t rec_page ;
   u_int32_t page_ext ;
   u_int32_t unused[91] ;
   u_int32_t crypto_magic ;
   u_int32_t trash[3] ;
   u_int8_t iv[16] ;
   u_int8_t chksum[20] ;
};
typedef struct _qmeta33 QMETA;
struct _qpage {
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t unused0[3] ;
   u_int8_t unused1[1] ;
   u_int8_t type ;
   u_int8_t unused2[2] ;
   u_int8_t chksum[20] ;
   u_int8_t iv[16] ;
};
typedef struct _qpage QPAGE;
struct _qamdata {
   u_int8_t flags ;
   u_int8_t data[1] ;
};
typedef struct _qamdata QAMDATA;
struct __qcursor;
typedef struct __qcursor QUEUE_CURSOR;
struct __qcursor {
   DBC *opd ;
   void *page ;
   db_pgno_t root ;
   db_pgno_t pgno ;
   db_indx_t indx ;
   DB_LOCK lock ;
   db_lockmode_t lock_mode ;
   db_recno_t recno ;
   u_int32_t flags ;
};
enum __anonenum_qam_position_mode_79 {
    QAM_READ = 0,
    QAM_WRITE = 1,
    QAM_CONSUME = 2
};
typedef enum __anonenum_qam_position_mode_79 qam_position_mode;
enum __anonenum_qam_probe_mode_80 {
    QAM_PROBE_GET = 0,
    QAM_PROBE_PUT = 1,
    QAM_PROBE_MPF = 2
};
typedef enum __anonenum_qam_probe_mode_80 qam_probe_mode;
struct ___qam_incfirst_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_recno_t recno ;
   db_pgno_t meta_pgno ;
};
typedef struct ___qam_incfirst_args __qam_incfirst_args;
struct ___qam_mvptr_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   int32_t fileid ;
   db_recno_t old_first ;
   db_recno_t new_first ;
   db_recno_t old_cur ;
   db_recno_t new_cur ;
   DB_LSN metalsn ;
   db_pgno_t meta_pgno ;
};
typedef struct ___qam_mvptr_args __qam_mvptr_args;
struct ___qam_del_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t indx ;
   db_recno_t recno ;
};
typedef struct ___qam_del_args __qam_del_args;
struct ___qam_add_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t indx ;
   db_recno_t recno ;
   DBT data ;
   u_int32_t vflag ;
   DBT olddata ;
};
typedef struct ___qam_add_args __qam_add_args;
struct ___qam_delext_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   DB_LSN lsn ;
   db_pgno_t pgno ;
   u_int32_t indx ;
   db_recno_t recno ;
   DBT data ;
};
typedef struct ___qam_delext_args __qam_delext_args;
struct __qam_filelist {
   DB_MPOOLFILE *mpf ;
   u_int32_t id ;
};
typedef struct __qam_filelist QUEUE_FILELIST;
enum __anonenum_qam_name_op_78 {
    QAM_NAME_DISCARD = 0,
    QAM_NAME_RENAME = 1,
    QAM_NAME_REMOVE = 2
};
typedef enum __anonenum_qam_name_op_78 qam_name_op;
struct __db_txnhead;
typedef struct __db_txnhead DB_TXNHEAD;
struct __db_txnlist;
enum __anonenum_db_txnlist_type_54 {
    TXNLIST_DELETE = 0,
    TXNLIST_LSN = 1,
    TXNLIST_PGNO = 2,
    TXNLIST_TXNID = 3
};
typedef enum __anonenum_db_txnlist_type_54 db_txnlist_type;
struct __anonstruct_gen_array_55 {
   u_int32_t generation ;
   u_int32_t txn_min ;
   u_int32_t txn_max ;
};
struct __db_headlink {
   struct __db_txnlist *lh_first ;
};
struct __db_txnhead {
   u_int32_t maxid ;
   DB_LSN maxlsn ;
   DB_LSN ckplsn ;
   DB_LSN trunc_lsn ;
   u_int32_t generation ;
   u_int32_t gen_alloc ;
   struct __anonstruct_gen_array_55 *gen_array ;
   u_int nslots ;
   struct __db_headlink head[1] ;
};
struct __anonstruct_links_56___0 {
   struct __db_txnlist *le_next ;
   struct __db_txnlist **le_prev ;
};
struct __anonstruct_t_58 {
   u_int32_t txnid ;
   u_int32_t generation ;
   int32_t status ;
};
struct __anonstruct_l_59 {
   u_int32_t ntxns ;
   u_int32_t maxn ;
   DB_LSN *lsn_array ;
};
struct __anonstruct_p_60 {
   u_int32_t nentries ;
   u_int32_t maxentry ;
   int32_t locked ;
   char *fname ;
   int32_t fileid ;
   db_pgno_t *pgno_array ;
   u_int8_t uid[20] ;
};
union __anonunion_u_57 {
   struct __anonstruct_t_58 t ;
   struct __anonstruct_l_59 l ;
   struct __anonstruct_p_60 p ;
};
struct __db_txnlist {
   db_txnlist_type type ;
   struct __anonstruct_links_56___0 links ;
   union __anonunion_u_57 u ;
};
struct __db_qam_stat;
typedef struct __db_qam_stat DB_QUEUE_STAT;
struct __db_qam_stat {
   u_int32_t qs_magic ;
   u_int32_t qs_version ;
   u_int32_t qs_metaflags ;
   u_int32_t qs_nkeys ;
   u_int32_t qs_ndata ;
   u_int32_t qs_pagesize ;
   u_int32_t qs_extentsize ;
   u_int32_t qs_pages ;
   u_int32_t qs_re_len ;
   u_int32_t qs_re_pad ;
   u_int32_t qs_pgfree ;
   u_int32_t qs_first_recno ;
   u_int32_t qs_cur_recno ;
};
struct _qmeta31 {
   DBMETA31 dbmeta ;
   u_int32_t start ;
   u_int32_t first_recno ;
   u_int32_t cur_recno ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int32_t rec_page ;
};
typedef struct _qmeta31 QMETA31;
struct _qmeta32 {
   DBMETA31 dbmeta ;
   u_int32_t first_recno ;
   u_int32_t cur_recno ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int32_t rec_page ;
   u_int32_t page_ext ;
};
typedef struct _qmeta32 QMETA32;
struct _qmeta30 {
   DBMETA30 dbmeta ;
   u_int32_t start ;
   u_int32_t first_recno ;
   u_int32_t cur_recno ;
   u_int32_t re_len ;
   u_int32_t re_pad ;
   u_int32_t rec_page ;
};
typedef struct _qmeta30 QMETA30;
struct __rep_control {
   u_int32_t rep_version ;
   u_int32_t log_version ;
   DB_LSN lsn ;
   u_int32_t rectype ;
   u_int32_t gen ;
   u_int32_t flags ;
};
typedef struct __rep_control REP_CONTROL;
struct __log;
typedef struct __log LOG;
struct __log_persist;
typedef struct __log_persist LOGP;
struct __log_persist {
   u_int32_t magic ;
   u_int32_t version ;
   u_int32_t log_size ;
   u_int32_t mode ;
};
struct __fq1 {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __commit {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __free {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __log {
   DB_MUTEX fq_mutex ;
   LOGP persist ;
   struct __fq1 fq ;
   int32_t fid_max ;
   roff_t free_fid_stack ;
   int free_fids ;
   int free_fids_alloced ;
   DB_LSN lsn ;
   DB_LSN f_lsn ;
   size_t b_off ;
   u_int32_t w_off ;
   u_int32_t len ;
   int in_flush ;
   roff_t flush_mutex_off ;
   DB_LSN s_lsn ;
   DB_LOG_STAT stat ;
   DB_LSN waiting_lsn ;
   DB_LSN verify_lsn ;
   DB_LSN max_wait_lsn ;
   u_int32_t wait_recs ;
   u_int32_t rcvd_recs ;
   DB_LSN ready_lsn ;
   DB_LSN cached_ckp_lsn ;
   roff_t buffer_off ;
   u_int32_t buffer_size ;
   u_int32_t log_size ;
   u_int32_t log_nsize ;
   u_int32_t ncommit ;
   DB_LSN t_lsn ;
   struct __commit commits ;
   struct __free free_commits ;
};
struct __db_txnregion;
typedef struct __db_txnregion DB_TXNREGION;
struct __active {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __db_txnregion {
   u_int32_t maxtxns ;
   u_int32_t last_txnid ;
   u_int32_t cur_maxid ;
   DB_LSN last_ckp ;
   time_t time_ckp ;
   DB_TXN_STAT stat ;
   u_int32_t flags ;
   struct __active active_txn ;
};
struct ___txn_regop_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   int32_t timestamp ;
   DBT locks ;
};
typedef struct ___txn_regop_args __txn_regop_args;
struct ___txn_ckp_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   DB_LSN ckp_lsn ;
   DB_LSN last_ckp ;
   int32_t timestamp ;
   u_int32_t rep_gen ;
};
typedef struct ___txn_ckp_args __txn_ckp_args;
struct ___txn_xa_regop_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   DBT xid ;
   int32_t formatID ;
   u_int32_t gtrid ;
   u_int32_t bqual ;
   DB_LSN begin_lsn ;
   DBT locks ;
};
typedef struct ___txn_xa_regop_args __txn_xa_regop_args;
struct __rep_vote {
   u_int32_t egen ;
   int nsites ;
   int priority ;
   int tiebreaker ;
};
typedef struct __rep_vote REP_VOTE_INFO;
struct __rep_vtally {
   u_int32_t egen ;
   int eid ;
};
typedef struct __rep_vtally REP_VTALLY;
struct __lsn_collection {
   int nlsns ;
   int nalloc ;
   DB_LSN *array ;
};
typedef struct __lsn_collection LSN_COLLECTION;
struct ___dbreg_register_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   DBT name ;
   DBT uid ;
   int32_t fileid ;
   DBTYPE ftype ;
   db_pgno_t meta_pgno ;
   u_int32_t id ;
};
typedef struct ___dbreg_register_args __dbreg_register_args;
struct ___txn_child_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t child ;
   DB_LSN c_lsn ;
};
typedef struct ___txn_child_args __txn_child_args;
typedef u_int32_t u32;
struct __anonstruct_keyInstance_53 {
   u_int8_t direction ;
   int keyLen ;
   char keyMaterial[65] ;
   int Nr ;
   u32 rk[60] ;
   u32 ek[60] ;
};
typedef struct __anonstruct_keyInstance_53 keyInstance;
struct __anonstruct_cipherInstance_54 {
   u_int8_t mode ;
   u_int8_t IV[16] ;
};
typedef struct __anonstruct_cipherInstance_54 cipherInstance;
struct __aes_cipher {
   keyInstance decrypt_ki ;
   keyInstance encrypt_ki ;
   u_int32_t flags ;
};
typedef struct __aes_cipher AES_CIPHER;
struct __anonstruct_SHA1_CTX_55 {
   u_int32_t state[5] ;
   u_int32_t count[2] ;
   unsigned char buffer[64] ;
};
typedef struct __anonstruct_SHA1_CTX_55 SHA1_CTX;
struct __cipher {
   roff_t passwd ;
   size_t passwd_len ;
   u_int32_t flags ;
};
typedef struct __cipher CIPHER;
typedef u_int8_t u8;
struct ___crdel_metasub_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DBT page ;
   DB_LSN lsn ;
};
typedef struct ___crdel_metasub_args __crdel_metasub_args;
struct __db_mpool;
typedef struct __db_mpool DB_MPOOL;
enum __anonenum_mu_action_40 {
    MU_REMOVE = 0,
    MU_RENAME = 1,
    MU_OPEN = 2
};
typedef enum __anonenum_mu_action_40 mu_action;
struct __db_mpreg;
struct __db_mpregh {
   struct __db_mpreg *lh_first ;
};
struct __db_mpoolfileh {
   struct __db_mpoolfile *tqh_first ;
   struct __db_mpoolfile **tqh_last ;
};
struct __db_mpool {
   DB_MUTEX *mutexp ;
   struct __db_mpregh dbregq ;
   struct __db_mpoolfileh dbmfq ;
   DB_ENV *dbenv ;
   u_int32_t nreg ;
   REGINFO *reginfo ;
};
struct __anonstruct_q_77 {
   struct __db_mpreg *le_next ;
   struct __db_mpreg **le_prev ;
};
struct __db_mpreg {
   struct __anonstruct_q_77 q ;
   int32_t ftype ;
   int (*pgin)(DB_ENV * , db_pgno_t  , void * , DBT * ) ;
   int (*pgout)(DB_ENV * , db_pgno_t  , void * , DBT * ) ;
};
struct ___db_addrem_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   int32_t fileid ;
   db_pgno_t pgno ;
   u_int32_t indx ;
   u_int32_t nbytes ;
   DBT hdr ;
   DBT dbt ;
   DB_LSN pagelsn ;
};
typedef struct ___db_addrem_args __db_addrem_args;
struct ___db_big_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   int32_t fileid ;
   db_pgno_t pgno ;
   db_pgno_t prev_pgno ;
   db_pgno_t next_pgno ;
   DBT dbt ;
   DB_LSN pagelsn ;
   DB_LSN prevlsn ;
   DB_LSN nextlsn ;
};
typedef struct ___db_big_args __db_big_args;
struct ___db_ovref_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   int32_t adjust ;
   DB_LSN lsn ;
};
typedef struct ___db_ovref_args __db_ovref_args;
struct ___db_relink_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t opcode ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN lsn ;
   db_pgno_t prev ;
   DB_LSN lsn_prev ;
   db_pgno_t next ;
   DB_LSN lsn_next ;
};
typedef struct ___db_relink_args __db_relink_args;
struct ___db_debug_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   DBT op ;
   int32_t fileid ;
   DBT key ;
   DBT data ;
   u_int32_t arg_flags ;
};
typedef struct ___db_debug_args __db_debug_args;
struct ___db_noop_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN prevlsn ;
};
typedef struct ___db_noop_args __db_noop_args;
struct ___db_pg_alloc_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   DB_LSN meta_lsn ;
   db_pgno_t meta_pgno ;
   DB_LSN page_lsn ;
   db_pgno_t pgno ;
   u_int32_t ptype ;
   db_pgno_t next ;
};
typedef struct ___db_pg_alloc_args __db_pg_alloc_args;
struct ___db_pg_free_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN meta_lsn ;
   db_pgno_t meta_pgno ;
   DBT header ;
   db_pgno_t next ;
};
typedef struct ___db_pg_free_args __db_pg_free_args;
struct ___db_cksum_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
};
typedef struct ___db_cksum_args __db_cksum_args;
struct ___db_pg_freedata_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN meta_lsn ;
   db_pgno_t meta_pgno ;
   DBT header ;
   db_pgno_t next ;
   DBT data ;
};
typedef struct ___db_pg_freedata_args __db_pg_freedata_args;
struct ___db_pg_prepare_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
};
typedef struct ___db_pg_prepare_args __db_pg_prepare_args;
struct ___db_pg_new_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   int32_t fileid ;
   db_pgno_t pgno ;
   DB_LSN meta_lsn ;
   db_pgno_t meta_pgno ;
   DBT header ;
   db_pgno_t next ;
};
typedef struct ___db_pg_new_args __db_pg_new_args;
union __anonunion_u_53 {
   long l ;
   char c[(int )sizeof(long )] ;
};
typedef struct __db_txnlist DB_TXNLIST;
enum __anonenum_db_limbo_state_68 {
    LIMBO_NORMAL = 0,
    LIMBO_PREPARE = 1,
    LIMBO_RECOVER = 2,
    LIMBO_TIMESTAMP = 3,
    LIMBO_COMPENSATE = 4
};
typedef enum __anonenum_db_limbo_state_68 db_limbo_state;
typedef __builtin_va_list __gnuc_va_list;
typedef __gnuc_va_list va_list;
struct __join_cursor {
   u_int8_t *j_exhausted ;
   DBC **j_curslist ;
   DBC **j_fdupcurs ;
   DBC **j_workcurs ;
   DB *j_primary ;
   DBT j_key ;
   DBT j_rdata ;
   u_int32_t j_ncurs ;
   u_int32_t flags ;
};
typedef struct __join_cursor JOIN_CURSOR;
struct __db_envq {
   struct __db_env *tqh_first ;
   struct __db_env **tqh_last ;
};
struct __db_globals {
   struct __db_envq db_envq ;
   int (*j_close)(int  ) ;
   void (*j_dirfree)(char ** , int  ) ;
   int (*j_dirlist)(char const   * , char *** , int * ) ;
   int (*j_exists)(char const   * , int * ) ;
   void (*j_free)(void * ) ;
   int (*j_fsync)(int  ) ;
   int (*j_ioinfo)(char const   * , int  , u_int32_t * , u_int32_t * , u_int32_t * ) ;
   void *(*j_malloc)(size_t  ) ;
   int (*j_map)(char * , size_t  , int  , int  , void ** ) ;
   int (*j_open)(char const   * , int   , ...) ;
   ssize_t (*j_read)(int  , void * , size_t  ) ;
   void *(*j_realloc)(void * , size_t  ) ;
   int (*j_rename)(char const   * , char const   * ) ;
   int (*j_seek)(int  , size_t  , db_pgno_t  , u_int32_t  , int  , int  ) ;
   int (*j_sleep)(u_long  , u_long  ) ;
   int (*j_unlink)(char const   * ) ;
   int (*j_unmap)(void * , size_t  ) ;
   ssize_t (*j_write)(int  , void const   * , size_t  ) ;
   int (*j_yield)(void) ;
};
typedef unsigned char __u_char;
typedef __u_char u_char;
struct __fn {
   u_int32_t mask ;
   char const   *name ;
};
typedef struct __fn FN;
typedef unsigned long long db_align_t;
struct __head {
   ssize_t slh_first ;
};
struct __anonstruct_links_59 {
   ssize_t sle_next ;
   ssize_t sle_prev ;
};
struct __data {
   size_t len ;
   struct __anonstruct_links_59 links ;
};
struct __anonstruct_list_53 {
   u_int32_t power ;
   u_int32_t prime ;
};
struct hash_head {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
typedef struct __db DBM;
struct __anonstruct_datum_39 {
   char *dptr ;
   int dsize ;
};
typedef struct __anonstruct_datum_39 datum;
struct __db_reg_env_ref {
   roff_t size ;
   long segid ;
};
typedef struct __db_reg_env_ref REGENV_REF;
struct ___fop_create_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   DBT name ;
   u_int32_t appname ;
   u_int32_t mode ;
};
typedef struct ___fop_create_args __fop_create_args;
struct ___fop_remove_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   DBT name ;
   DBT fid ;
   u_int32_t appname ;
};
typedef struct ___fop_remove_args __fop_remove_args;
struct ___fop_write_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   DBT name ;
   u_int32_t appname ;
   u_int32_t pgsize ;
   db_pgno_t pageno ;
   u_int32_t offset ;
   DBT page ;
   u_int32_t flag ;
};
typedef struct ___fop_write_args __fop_write_args;
struct ___fop_rename_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   DBT oldname ;
   DBT newname ;
   DBT fileid ;
   u_int32_t appname ;
};
typedef struct ___fop_rename_args __fop_rename_args;
struct ___fop_file_remove_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   DBT real_fid ;
   DBT tmp_fid ;
   DBT name ;
   u_int32_t appname ;
   u_int32_t child ;
};
typedef struct ___fop_file_remove_args __fop_file_remove_args;
enum __anonenum_ACTION_40 {
    FIND = 0,
    ENTER = 1
};
typedef enum __anonenum_ACTION_40 ACTION;
struct entry {
   char *key ;
   char *data ;
};
typedef struct entry ENTRY;
enum __anonenum_db_status_t_29 {
    DB_LSTAT_ABORTED = 1,
    DB_LSTAT_ERR = 2,
    DB_LSTAT_EXPIRED = 3,
    DB_LSTAT_FREE = 4,
    DB_LSTAT_HELD = 5,
    DB_LSTAT_NOTEXIST = 6,
    DB_LSTAT_PENDING = 7,
    DB_LSTAT_WAITING = 8
};
typedef enum __anonenum_db_status_t_29 db_status_t;
struct __hash_head {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
typedef struct __hash_head DB_HASHTAB;
struct __anonstruct_db_timeval_t_59 {
   u_int32_t tv_sec ;
   u_int32_t tv_usec ;
};
typedef struct __anonstruct_db_timeval_t_59 db_timeval_t;
struct __flock {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __fobj {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __flocker {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __dobj {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __lkrs {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __db_lockregion {
   u_int32_t need_dd ;
   u_int32_t detect ;
   db_timeval_t next_timeout ;
   struct __flock free_locks ;
   struct __fobj free_objs ;
   struct __flocker free_lockers ;
   struct __dobj dd_objs ;
   struct __lkrs lockers ;
   db_timeout_t lk_timeout ;
   db_timeout_t tx_timeout ;
   u_int32_t locker_t_size ;
   u_int32_t object_t_size ;
   roff_t conf_off ;
   roff_t obj_off ;
   roff_t osynch_off ;
   roff_t locker_off ;
   roff_t lsynch_off ;
   DB_LOCK_STAT stat ;
};
typedef struct __db_lockregion DB_LOCKREGION;
struct __sh_dbt {
   u_int32_t size ;
   ssize_t off ;
};
typedef struct __sh_dbt SH_DBT;
struct __anonstruct_links_60 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __anonstruct_dd_links_61 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __waitl {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __holdl {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __db_lockobj {
   SH_DBT lockobj ;
   struct __anonstruct_links_60 links ;
   struct __anonstruct_dd_links_61 dd_links ;
   struct __waitl waiters ;
   struct __holdl holders ;
   u_int8_t objdata[(int )sizeof(struct __db_ilock )] ;
};
typedef struct __db_lockobj DB_LOCKOBJ;
struct _child {
   ssize_t slh_first ;
};
struct __anonstruct_child_link_62 {
   ssize_t sle_next ;
   ssize_t sle_prev ;
};
struct __anonstruct_links_63 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __anonstruct_ulinks_64 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct _held {
   ssize_t slh_first ;
};
struct __db_locker {
   u_int32_t id ;
   u_int32_t dd_id ;
   u_int32_t nlocks ;
   u_int32_t nwrites ;
   size_t master_locker ;
   size_t parent_locker ;
   struct _child child_locker ;
   struct __anonstruct_child_link_62 child_link ;
   struct __anonstruct_links_63 links ;
   struct __anonstruct_ulinks_64 ulinks ;
   struct _held heldby ;
   db_timeval_t lk_expire ;
   db_timeval_t tx_expire ;
   db_timeout_t lk_timeout ;
   u_int32_t flags ;
};
typedef struct __db_locker DB_LOCKER;
struct __db_locktab {
   DB_ENV *dbenv ;
   REGINFO reginfo ;
   u_int8_t *conflicts ;
   DB_HASHTAB *obj_tab ;
   DB_HASHTAB *locker_tab ;
};
typedef struct __db_locktab DB_LOCKTAB;
struct __anonstruct_links_65___0 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __anonstruct_locker_links_66 {
   ssize_t sle_next ;
   ssize_t sle_prev ;
};
struct __db_lock {
   DB_MUTEX mutex ;
   u_int32_t holder ;
   u_int32_t gen ;
   struct __anonstruct_links_65___0 links ;
   struct __anonstruct_locker_links_66 locker_links ;
   u_int32_t refcount ;
   db_lockmode_t mode ;
   ssize_t obj ;
   db_status_t status ;
};
enum __anonenum_action_70 {
    GRANT = 0,
    UPGRADE = 1,
    HEAD = 2,
    SECOND = 3,
    TAIL = 4
};
struct __anonstruct_locker_info_67 {
   int valid ;
   int self_wait ;
   int in_abort ;
   u_int32_t count ;
   u_int32_t id ;
   u_int32_t last_lock ;
   ssize_t last_obj ;
   u_int32_t last_locker_id ;
   db_pgno_t pgno ;
};
typedef struct __anonstruct_locker_info_67 locker_info;
struct tm {
   int tm_sec ;
   int tm_min ;
   int tm_hour ;
   int tm_mday ;
   int tm_mon ;
   int tm_year ;
   int tm_wday ;
   int tm_yday ;
   int tm_isdst ;
   long tm_gmtoff ;
   char const   *tm_zone ;
};
struct __hdr;
typedef struct __hdr HDR;
struct __hdr {
   u_int32_t prev ;
   u_int32_t len ;
   u_int8_t chksum[20] ;
   u_int8_t iv[16] ;
   u_int32_t orig_size ;
   size_t size ;
};
enum __anonenum_logfile_validity_65 {
    DB_LV_INCOMPLETE = 0,
    DB_LV_NONEXISTENT = 1,
    DB_LV_NORMAL = 2,
    DB_LV_OLD_READABLE = 3,
    DB_LV_OLD_UNREADABLE = 4
};
typedef enum __anonenum_logfile_validity_65 logfile_validity;
enum __anonenum_RLOCK_69 {
    L_ALREADY = 0,
    L_ACQUIRED = 1,
    L_NONE = 2
};
typedef enum __anonenum_RLOCK_69 RLOCK;
struct __anonstruct_links_57 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __db_commit {
   DB_MUTEX mutex ;
   DB_LSN lsn ;
   struct __anonstruct_links_57 links ;
   u_int32_t flags ;
};
struct __bh;
typedef struct __bh BH;
struct __db_mpool_hash;
typedef struct __db_mpool_hash DB_MPOOL_HASH;
struct __mpool;
typedef struct __mpool MPOOL;
enum __anonenum_db_sync_op_53 {
    DB_SYNC_ALLOC = 0,
    DB_SYNC_CACHE = 1,
    DB_SYNC_FILE = 2,
    DB_SYNC_TRICKLE = 3
};
typedef enum __anonenum_db_sync_op_53 db_sync_op;
struct __mpfq {
   ssize_t stqh_first ;
   ssize_t stqh_last ;
};
struct __mpool {
   DB_LSN lsn ;
   struct __mpfq mpfq ;
   u_int32_t nreg ;
   roff_t regids ;
   int htab_buckets ;
   roff_t htab ;
   u_int32_t last_checked ;
   u_int32_t lru_count ;
   DB_MPOOL_STAT stat ;
   u_int32_t put_counter ;
};
struct __db_mpool_hash {
   DB_MUTEX hash_mutex ;
   DB_HASHTAB hash_bucket ;
   u_int32_t hash_page_dirty ;
   u_int32_t hash_priority ;
};
struct __anonstruct_hq_56 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __bh {
   DB_MUTEX mutex ;
   u_int16_t ref ;
   u_int16_t ref_sync ;
   u_int16_t flags ;
   u_int32_t priority ;
   struct __anonstruct_hq_56 hq ;
   db_pgno_t pgno ;
   roff_t mf_offset ;
   u_int8_t buf[1] ;
};
typedef struct __db_mpreg DB_MPREG;
enum __anonenum_state_60 {
    FIRST_FOUND = 0,
    FIRST_MISS = 1,
    SECOND_FOUND = 2,
    SECOND_MISS = 3
};
struct __anonstruct_BH_TRACK_66 {
   DB_MPOOL_HASH *track_hp ;
   roff_t track_off ;
   db_pgno_t track_pgno ;
};
typedef struct __anonstruct_BH_TRACK_66 BH_TRACK;
struct __db_regmaint_stat_t {
   u_int32_t st_hint_hit ;
   u_int32_t st_hint_miss ;
   u_int32_t st_records ;
   u_int32_t st_clears ;
   u_int32_t st_destroys ;
   u_int32_t st_max_locks ;
};
typedef struct __db_regmaint_stat_t REGMAINT_STAT;
struct __db_regmaint_t {
   u_int32_t reglocks ;
   u_int32_t regmutex_hint ;
   REGMAINT_STAT stat ;
   roff_t regmutexes[1] ;
};
typedef struct __db_regmaint_t REGMAINT;
struct timezone {
   int tz_minuteswest ;
   int tz_dsttime ;
};
typedef struct timezone * __restrict  __timezone_ptr_t;
typedef unsigned long __ino_t;
struct dirent {
   __ino_t d_ino ;
   __off_t d_off ;
   unsigned short d_reclen ;
   unsigned char d_type ;
   char d_name[256] ;
};
struct __dirstream;
typedef struct __dirstream DIR;
typedef unsigned long long __u_quad_t;
typedef __u_quad_t __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned int __mode_t;
typedef unsigned int __nlink_t;
typedef long __blksize_t;
typedef long __blkcnt_t;
struct timespec {
   __time_t tv_sec ;
   long tv_nsec ;
};
struct stat {
   __dev_t st_dev ;
   unsigned short __pad1 ;
   __ino_t st_ino ;
   __mode_t st_mode ;
   __nlink_t st_nlink ;
   __uid_t st_uid ;
   __gid_t st_gid ;
   __dev_t st_rdev ;
   unsigned short __pad2 ;
   __off_t st_size ;
   __blksize_t st_blksize ;
   __blkcnt_t st_blocks ;
   struct timespec st_atim ;
   struct timespec st_mtim ;
   struct timespec st_ctim ;
   unsigned long __unused4 ;
   unsigned long __unused5 ;
};
typedef int __key_t;
typedef __key_t key_t;
struct ipc_perm {
   __key_t __key ;
   __uid_t uid ;
   __gid_t gid ;
   __uid_t cuid ;
   __gid_t cgid ;
   unsigned short mode ;
   unsigned short __pad1 ;
   unsigned short __seq ;
   unsigned short __pad2 ;
   unsigned long __unused1 ;
   unsigned long __unused2 ;
};
typedef unsigned long shmatt_t;
struct shmid_ds {
   struct ipc_perm shm_perm ;
   size_t shm_segsz ;
   __time_t shm_atime ;
   unsigned long __unused1 ;
   __time_t shm_dtime ;
   unsigned long __unused2 ;
   __time_t shm_ctime ;
   unsigned long __unused3 ;
   __pid_t shm_cpid ;
   __pid_t shm_lpid ;
   shmatt_t shm_nattch ;
   unsigned long __unused4 ;
   unsigned long __unused5 ;
};
typedef __off_t off_t;
union __anonunion_CHAR64LONG16_54 {
   unsigned char c[64] ;
   u_int32_t l[16] ;
};
typedef union __anonunion_CHAR64LONG16_54 CHAR64LONG16;
struct __anonstruct_links_88 {
   ssize_t stqe_next ;
   ssize_t stqe_prev ;
};
struct __txn_detail {
   u_int32_t txnid ;
   DB_LSN last_lsn ;
   DB_LSN begin_lsn ;
   roff_t parent ;
   u_int32_t status ;
   u_int32_t flags ;
   struct __anonstruct_links_88 links ;
   u_int32_t xa_status ;
   u_int8_t xid[128] ;
   u_int32_t bqual ;
   u_int32_t gtrid ;
   int32_t format ;
};
typedef struct __txn_detail TXN_DETAIL;
enum __anonenum_txnop_t_90 {
    TXN_OP_ABORT = 0,
    TXN_OP_COMMIT = 1,
    TXN_OP_DISCARD = 2,
    TXN_OP_PREPARE = 3
};
typedef enum __anonenum_txnop_t_90 txnop_t;
struct ___txn_recycle_args {
   u_int32_t type ;
   DB_TXN *txnid ;
   DB_LSN prev_lsn ;
   u_int32_t min ;
   u_int32_t max ;
};
typedef struct ___txn_recycle_args __txn_recycle_args;
struct xid_t {
   long formatID ;
   long gtrid_length ;
   long bqual_length ;
   char data[128] ;
};
typedef struct xid_t XID;
enum __anonenum_TXN_EVENT_T_65 {
    TXN_CLOSE = 0,
    TXN_REMOVE = 1,
    TXN_TRADE = 2,
    TXN_TRADED = 3
};
typedef enum __anonenum_TXN_EVENT_T_65 TXN_EVENT_T;
typedef struct __txn_event TXN_EVENT;
struct __anonstruct_links_77 {
   struct __txn_event *tqe_next ;
   struct __txn_event **tqe_prev ;
};
struct __anonstruct_c_79 {
   DB *dbp ;
};
struct __anonstruct_r_80 {
   char *name ;
   u_int8_t *fileid ;
};
struct __anonstruct_t_81 {
   DB_LOCK lock ;
   u_int32_t locker ;
   DB *dbp ;
};
union __anonunion_u_78 {
   struct __anonstruct_c_79 c ;
   struct __anonstruct_r_80 r ;
   struct __anonstruct_t_81 t ;
};
struct __txn_event {
   TXN_EVENT_T op ;
   struct __anonstruct_links_77 links ;
   union __anonunion_u_78 u ;
};
struct xa_switch_t {
   char name[32] ;
   long flags ;
   long version ;
   int (*xa_open_entry)(char * , int  , long  ) ;
   int (*xa_close_entry)(char * , int  , long  ) ;
   int (*xa_start_entry)(XID * , int  , long  ) ;
   int (*xa_end_entry)(XID * , int  , long  ) ;
   int (*xa_rollback_entry)(XID * , int  , long  ) ;
   int (*xa_prepare_entry)(XID * , int  , long  ) ;
   int (*xa_commit_entry)(XID * , int  , long  ) ;
   int (*xa_recover_entry)(XID * , long  , int  , long  ) ;
   int (*xa_forget_entry)(XID * , int  , long  ) ;
   int (*xa_complete_entry)(int * , int * , int  , long  ) ;
};
struct __xa_methods {
   int (*close)(DB * , u_int32_t  ) ;
   int (*cursor)(DB * , DB_TXN * , DBC ** , u_int32_t  ) ;
   int (*del)(DB * , DB_TXN * , DBT * , u_int32_t  ) ;
   int (*get)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
   int (*open)(DB * , DB_TXN * , char const   * , char const   * , DBTYPE  , u_int32_t  ,
               int  ) ;
   int (*put)(DB * , DB_TXN * , DBT * , DBT * , u_int32_t  ) ;
};
typedef struct __xa_methods XA_METHODS;
#pragma merger(0,"/tmp/cil-eA2jBBN3.i","")
extern int select(int __nfds , fd_set * __restrict  __readfds , fd_set * __restrict  __writefds ,
                  fd_set * __restrict  __exceptfds , struct timeval * __restrict  __timeout ) ;
extern  __attribute__((__nothrow__)) int *__errno_location(void)  __attribute__((__const__)) ;
extern  __attribute__((__nothrow__)) int sched_yield(void) ;
extern  __attribute__((__nothrow__)) time_t time(time_t *__timer ) ;
extern  __attribute__((__nothrow__)) int pthread_create(pthread_t * __restrict  __threadp ,
                                                        pthread_attr_t const   * __restrict  __attr ,
                                                        void *(*__start_routine)(void * ) ,
                                                        void * __restrict  __arg ) ;
extern  __attribute__((__nothrow__)) pthread_t pthread_self(void) ;
extern int pthread_join(pthread_t __th , void **__thread_return ) ;
extern  __attribute__((__nothrow__)) __sighandler_t signal(int __sig , void (*__handler)(int  ) ) ;
extern struct _IO_FILE *stdout ;
extern struct _IO_FILE *stderr ;
extern  __attribute__((__nothrow__)) int remove(char const   *__filename ) ;
extern int fflush(FILE *__stream ) ;
extern FILE *fopen(char const   * __restrict  __filename , char const   * __restrict  __modes ) ;
extern int fprintf(FILE * __restrict  __stream , char const   * __restrict  __format 
                   , ...) ;
extern int printf(char const   * __restrict  __format  , ...) ;
extern  __attribute__((__nothrow__)) int sprintf(char * __restrict  __s , char const   * __restrict  __format 
                                                 , ...) ;
extern char *fgets(char * __restrict  __s , int __n , FILE * __restrict  __stream ) ;
extern  __attribute__((__nothrow__)) int atoi(char const   *__nptr )  __attribute__((__pure__,
__nonnull__(1))) ;
extern  __attribute__((__nothrow__)) int rand(void) ;
extern  __attribute__((__nothrow__)) void srand(unsigned int __seed ) ;
extern  __attribute__((__nothrow__)) void *malloc(size_t __size )  __attribute__((__malloc__)) ;
extern  __attribute__((__nothrow__)) void *calloc(size_t __nmemb , size_t __size )  __attribute__((__malloc__)) ;
extern  __attribute__((__nothrow__)) void free(void *__ptr ) ;
extern  __attribute__((__nothrow__, __noreturn__)) void exit(int __status ) ;
extern  __attribute__((__nothrow__)) void *memset(void *__s , int __c , size_t __n )  __attribute__((__nonnull__(1))) ;
extern  __attribute__((__nothrow__)) char *strdup(char const   *__s )  __attribute__((__nonnull__(1),
__malloc__)) ;
extern  __attribute__((__nothrow__)) size_t strlen(char const   *__s )  __attribute__((__pure__,
__nonnull__(1))) ;
extern  __attribute__((__nothrow__)) char *strerror(int __errnum ) ;
extern ssize_t write(int __fd , void const   *__buf , size_t __n ) ;
extern unsigned int sleep(unsigned int __seconds ) ;
extern  __attribute__((__nothrow__)) __pid_t getpid(void) ;
extern char *optarg ;
extern int optind ;
extern  __attribute__((__nothrow__)) int getopt(int ___argc , char * const  *___argv ,
                                                char const   *__shortopts ) ;
int db_create(DB **dbpp , DB_ENV *dbenv___0 , u_int32_t flags ) ;
char *db_strerror(int error ) ;
int db_env_create(DB_ENV **dbenvpp , u_int32_t flags ) ;
int db_env_set_func_yield(int (*func_yield)(void) ) ;
int db_init(char const   *home ) ;
void *deadlock(void *arg ) ;
void fatal(char const   *msg , int err , int syserr ) ;
void onint(int signo ) ;
int main(int argc , char **argv ) ;
int reader(int id ) ;
void stats(void) ;
void *trickle(void *arg ) ;
void *tstart(void *arg ) ;
int usage(void) ;
void word(void) ;
int writer(int id ) ;
int quit  ;
struct _statistics *perf  ;
char const   *progname  =    "ex_thread";
int punish  ;
int nlist  ;
int nreaders  ;
int verbose  ;
int nwriters  ;
DB *dbp  ;
DB_ENV *dbenv  ;
int nthreads  ;
char **list  ;


static char const   __db_lock_invalid[28]  = 
  {      (char const   )'%',      (char const   )'s',      (char const   )':',      (char const   )' ', 
        (char const   )'L',      (char const   )'o',      (char const   )'c',      (char const   )'k', 
        (char const   )' ',      (char const   )'i',      (char const   )'s',      (char const   )' ', 
        (char const   )'n',      (char const   )'o',      (char const   )' ',      (char const   )'l', 
        (char const   )'o',      (char const   )'n',      (char const   )'g',      (char const   )'e', 
        (char const   )'r',      (char const   )' ',      (char const   )'v',      (char const   )'a', 
        (char const   )'l',      (char const   )'i',      (char const   )'d',      (char const   )'\000'};



int __db_panic(DB_ENV *dbenv___0 , int errval ) 
{ 

  return (-30978);
  
}


void __db_err(DB_ENV const   *dbenv___0 , char const   *fmt  , ...) 
{ va_list ap ;
  return;
}

int __memp_fput(DB_MPOOLFILE *dbmfp , void *pgaddr , u_int32_t flags ) 
{ 
  return 0;
}

int __memp_fget(DB_MPOOLFILE *dbmfp , db_pgno_t *pgnoaddr , 
                u_int32_t flags , void *addrp ) 
{ 
  (*((void **)addrp)) = (void *)0;

  return 0;
}

int __db_pgerr(DB *dbp___1 , db_pgno_t pgno , int errval ) 
{ int tmp ;

  {
  __db_err((DB_ENV const   *)dbp___1->dbenv, 
           "unable to create/retrieve page %lu",
           (unsigned long )pgno);
  tmp = __db_panic(dbp___1->dbenv, errval);
  return (tmp);
}
}


int log_compare(DB_LSN const   *lsn0 , DB_LSN const   *lsn1 ) 
{ int tmp ;
  int tmp___0 ;

  if (lsn0->file != lsn1->file) {
    if (lsn0->file < lsn1->file) {
      tmp = -1;
    } else {
      tmp = 1;
    }
    return (tmp);
  }
  if (lsn0->offset != lsn1->offset) {
    if (lsn0->offset < lsn1->offset) {
      tmp___0 = -1;
    } else {
      tmp___0 = 1;
    }
    return (tmp___0);
  }
  return (0);

}

void *__ua_memcpy(void *dst , void const   *src , size_t len ) 
{ void *tmp ;

  
  tmp = memcpy((void * __restrict  )dst, 
               (void const   * __restrict  )src, 
               len);
  return (tmp);

}


static int __db_pg_free_recover_int(DB_ENV *dbenv___0 , 
                                    __db_pg_freedata_args *argp ,
                                    DB *file_dbp , DB_LSN *lsnp , 
                                    DB_MPOOLFILE *mpf ,
                                    db_recops op , int data ) 
{ DBMETA *meta ;
  DB_LSN copy_lsn ;
  PAGE *pagep ;
  db_pgno_t pgno ;
  int cmp_n ;
  int cmp_p ;
  int modified ;
  int ret ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;


  meta = (DBMETA *)((void *)0);
  pagep = (PAGE *)((void *)0);
  ret = __memp_fget(mpf, & argp->pgno, 1U, (void *)(& pagep));

  if (ret != 0) {
    goto out;
  }

  modified = 0;
  __ua_memcpy((void *)(& copy_lsn), 
              (void const   *)(& ((PAGE *)argp->header.data)->lsn),
              sizeof(DB_LSN ));
  cmp_n = log_compare((DB_LSN const   *)lsnp, 
                      (DB_LSN const   *)(& pagep->lsn));
  cmp_p = log_compare((DB_LSN const   *)(& pagep->lsn), 
                      (DB_LSN const   *)(& copy_lsn));
  if ((int )op == 4) {
    goto _L;
  } else {
    if ((int )op == 1) {
      _L: 
      if (cmp_p < 0) {
        if (pagep->lsn.file == 0U) {
          if (! (pagep->lsn.offset == 1U)) {
            __db_err((DB_ENV const *)dbenv___0, 
                     "Log sequence err: page LSN %lu %lu; prev LSN %lu %lu",
                     (unsigned long )pagep->lsn.file, 
                     (unsigned long )pagep->lsn.offset,
                     (unsigned long )copy_lsn.file, 
                     (unsigned long )copy_lsn.offset);
            ret = 22;
            goto out;
          }
        } else {
          __db_err((DB_ENV const *)dbenv___0, 
                   "Log sequence err: page LSN %lu %lu; prev LSN %lu %lu",
                   (unsigned long )pagep->lsn.file, 
                   (unsigned long )pagep->lsn.offset,
                   (unsigned long )copy_lsn.file, 
                   (unsigned long )copy_lsn.offset);
          ret = 22;
          goto out;
        }
      }
    }
  }

  if ((int )op == 4) {
    goto _L___3;
  } else {
    if ((int )op == 1) {
      _L___3: 
      if (cmp_p == 0) {
        goto _L___2;
      } else {
        if (copy_lsn.file == 0U) {
          tmp = log_compare((DB_LSN const   *)(& pagep->lsn), 
                            (DB_LSN const   *)(& argp->meta_lsn));
          if (tmp <= 0) {
            _L___2: 
            while (1) {
              pagep->pgno = argp->pgno;
              pagep->prev_pgno = 0U;
              pagep->next_pgno = argp->next;
              pagep->entries = (unsigned short)0;
              pagep->hf_offset = (unsigned short )file_dbp->pgsize;
              pagep->level = (unsigned char)0;
              pagep->type = (unsigned char)0;
              break;
            }
            pagep->lsn = (*lsnp);
            modified = 1;
          } else {
            goto _L___1;
          }
        } else {
          goto _L___1;
        }
      }
    } else {
      _L___1: 
      if (cmp_n == 0) {
        if ((int )op == 0) {
          goto _L___0;
        } else {
          if ((int )op == 3) {
            goto _L___0;
          } else {
            if ((int )op == 2) {
              _L___0: 
              memcpy((void * __restrict  )pagep, 
                     (void const   * __restrict  )argp->header.data,
                     argp->header.size);
              if (data) {
                memcpy((void * __restrict  )
                       ((u_int8_t *)pagep + (int )pagep->hf_offset),
                       (void const * __restrict  )argp->data.data, 
                       argp->data.size);
              }
              modified = 1;
            }
          }
        }
      }
    }
  }


  if (modified) {
    tmp___0 = 2;
  } else {
    tmp___0 = 0;
  }


  ret = __memp_fput(mpf, (void *)pagep, (unsigned int )tmp___0);

  if (ret != 0) {
    goto out;
  }

  pagep = (PAGE *)((void *)0);
  pgno = 0U;
  ret = __memp_fget(mpf, & pgno, 0U, (void *)(& meta));

  if (ret != 0) {
    ret = __db_pgerr(file_dbp, pgno, ret);
    goto out;
  }

  modified = 0;
  cmp_n = log_compare((DB_LSN const   *)lsnp, 
                      (DB_LSN const   *)(& ((PAGE *)meta)->lsn));
  cmp_p = log_compare((DB_LSN const   *)(& ((PAGE *)meta)->lsn), 
                      (DB_LSN const   *)(& argp->meta_lsn));

  if ((int )op == 4) {
    goto _L___4;
  } else {
    if ((int )op == 1) {
      _L___4: 
      if (cmp_p < 0) {
        if (((PAGE *)meta)->lsn.file == 0U) {
          if (! (((PAGE *)meta)->lsn.offset == 1U)) {
            __db_err((DB_ENV const   *)dbenv___0, 
                     "Log sequence err: page LSN %lu %lu; prev LSN %lu %lu",
                     (unsigned long )((PAGE *)meta)->lsn.file, 
                     (unsigned long )((PAGE *)meta)->lsn.offset,
                     (unsigned long )argp->meta_lsn.file, 
                     (unsigned long )argp->meta_lsn.offset);
            ret = 22;
            goto out;
          }
        } else {
          __db_err((DB_ENV const   *)dbenv___0, 
                   "Log sequence err: page LSN %lu %lu; prev LSN %lu %lu",
                   (unsigned long )((PAGE *)meta)->lsn.file, 
                   (unsigned long )((PAGE *)meta)->lsn.offset,
                   (unsigned long )argp->meta_lsn.file, 
                   (unsigned long )argp->meta_lsn.offset);
          ret = 22;
          goto out;
        }
      }
    }
  }

  if (cmp_p == 0) {
    if ((int )op == 4) {
      goto _L___6;
    } else {
      if ((int )op == 1) {
        _L___6: 
        meta->free = argp->pgno;
        if (meta->last_pgno < meta->free) {
          meta->last_pgno = meta->free;
        }
        ((PAGE *)meta)->lsn = (*lsnp);
        modified = 1;
      } else {
        goto _L___5;
      }
    }
  } else {
    _L___5: 
    if (cmp_n == 0) {
      if ((int )op == 0) {
        meta->free = argp->next;
        ((PAGE *)meta)->lsn = argp->meta_lsn;
        modified = 1;
      } else {
        if ((int )op == 3) {
          meta->free = argp->next;
          ((PAGE *)meta)->lsn = argp->meta_lsn;
          modified = 1;
        } else {
          if ((int )op == 2) {
            meta->free = argp->next;
            ((PAGE *)meta)->lsn = argp->meta_lsn;
            modified = 1;
          }
        }
      }
    }
  }
  if (modified) {
    tmp___1 = 2;
  } else {
    tmp___1 = 0;
  }
  ret = __memp_fput(mpf, (void *)meta, (unsigned int )tmp___1);
  if (ret != 0) {
    goto out;
  }
  meta = (DBMETA *)((void *)0);
  (*lsnp) = argp->prev_lsn;
  ret = 0;
  out: 
  if ((unsigned int )pagep != (unsigned int )((void *)0)) {
    __memp_fput(mpf, (void *)pagep, 0U);
  }
  if ((unsigned int )meta != (unsigned int )((void *)0)) {
    __memp_fput(mpf, (void *)meta, 0U);
  }
  return (ret);

}


////////////////////////////

//make dummy versions of these for now

extern  __attribute__((__nothrow__)) int pthread_mutex_init(pthread_mutex_t * __restrict  __mutex ,
                                                            pthread_mutexattr_t const   * __restrict  __mutex_attr ) ;
extern  __attribute__((__nothrow__)) int pthread_mutex_destroy(pthread_mutex_t *__mutex ) ;
extern  __attribute__((__nothrow__)) int pthread_mutex_trylock(pthread_mutex_t *__mutex ) ;
extern  __attribute__((__nothrow__)) int pthread_mutex_lock(pthread_mutex_t *__mutex ) ;
extern  __attribute__((__nothrow__)) int pthread_mutex_unlock(pthread_mutex_t *__mutex ) ;
extern  __attribute__((__nothrow__)) int pthread_mutexattr_init(pthread_mutexattr_t *__attr ) ;
extern  __attribute__((__nothrow__)) int pthread_mutexattr_destroy(pthread_mutexattr_t *__attr ) ;
extern  __attribute__((__nothrow__)) int pthread_cond_init(pthread_cond_t * __restrict  __cond ,
                                                           pthread_condattr_t const   * __restrict  __cond_attr ) ;
extern  __attribute__((__nothrow__)) int pthread_cond_signal(pthread_cond_t *__cond ) ;
extern int pthread_cond_wait(pthread_cond_t * __restrict  __cond , pthread_mutex_t * __restrict  __mutex ) ;
extern  __attribute__((__nothrow__)) int pthread_condattr_init(pthread_condattr_t *__attr ) ;
extern  __attribute__((__nothrow__)) int pthread_condattr_destroy(pthread_condattr_t *__attr ) ;



int __db_pthread_mutex_init(DB_ENV *dbenv___0 , DB_MUTEX *mutexp , 
                            u_int32_t flags ) ;
int __db_pthread_mutex_lock(DB_ENV *dbenv___0 , DB_MUTEX *mutexp ) ;
int __db_pthread_mutex_unlock(DB_ENV *dbenv___0 , DB_MUTEX *mutexp ) ;
int __db_pthread_mutex_destroy(DB_MUTEX *mutexp ) ;
void __db_err(DB_ENV const   *dbenv___0 , char const   *fmt  , ...) ;


int __db_pthread_mutex_init(DB_ENV *dbenv___0 , DB_MUTEX *mutexp , 
                            u_int32_t flags ) 
{ u_int32_t save ;
  int ret ;
  pthread_condattr_t condattr ;
  pthread_condattr_t *condattrp ;
  pthread_mutexattr_t mutexattr ;
  pthread_mutexattr_t *mutexattrp ;
  char *tmp ;

  {
  ret = 0;
  save = mutexp->flags & 16U;
  memset((void *)mutexp, 0, sizeof((*mutexp)));
  mutexp->flags |= save;
  if (flags & 256U) {
    goto _L;
  } else {
    if (dbenv___0->flags & 32768U) {
      _L: 
      if (! (dbenv___0->flags & 1048576U)) {
        mutexp->flags |= 2U;
        return (0);
      }
    }
  }
  condattrp = (pthread_condattr_t *)((void *)0);
  mutexattrp = (pthread_mutexattr_t *)((void *)0);
  if (! (flags & 256U)) {
    ret = 0;//pthread_mutexattr_init(& mutexattr);
    mutexattrp = & mutexattr;
  }
  if (ret == 0) {
    ret = 0;//pthread_mutex_init((pthread_mutex_t * __restrict  )(& mutexp->mutex), (pthread_mutexattr_t const   * __restrict  )mutexattrp);
  }
  if ((unsigned int )mutexattrp != (unsigned int )((void *)0)) {
    ;//pthread_mutexattr_destroy(mutexattrp);
  }
  if (ret == 0) {
    if (flags & 128U) {
      if (! (flags & 256U)) {
        ret = pthread_condattr_init(& condattr);
      }
      if (ret == 0) {
        ret = pthread_cond_init((pthread_cond_t * __restrict  )(& mutexp->cond), (pthread_condattr_t const   * __restrict  )condattrp);
      }
      mutexp->flags |= 128U;
      if ((unsigned int )condattrp != (unsigned int )((void *)0)) {
        pthread_condattr_destroy(condattrp);
      }
    }
  }
  if (ret == 0) {
    mutexp->flags |= 4U;
  } else {
    tmp = strerror(ret);
    __db_err((DB_ENV const   *)dbenv___0, "unable to initialize mutex: %s", tmp);
  }
  return (ret);
}
}

int __db_pthread_mutex_lock(DB_ENV *dbenv___0 , DB_MUTEX *mutexp ) 
{ u_int32_t nspins ;
  int i ;
  int ret ;
  int waited ;
  int tmp ;
  char *tmp___0 ;

  {
  if (dbenv___0->flags & 1024U) {
    return (0);
  } else {
    if (mutexp->flags & 2U) {
      return (0);
    }
  }
  nspins = dbenv___0->tas_spins;
  while (nspins > 0U) {
    tmp = 0;//pthread_mutex_trylock(& mutexp->mutex);
    if (tmp == 0) {
      break;
    }
    nspins --;
  }
  if (nspins == 0U) {
    ret = pthread_mutex_lock(& mutexp->mutex);
    if (ret != 0) {
      goto err;
    }
  }
  if (mutexp->flags & 128U) {
    waited = 0;
    while (mutexp->locked != 0U) {
      ret = pthread_cond_wait((pthread_cond_t * __restrict  )(& mutexp->cond), (pthread_mutex_t * __restrict  )(& mutexp->mutex));
      if (ret != 0) {
        if (ret != 4) {
          if (ret != 62) {
            if (ret != 110) {
              pthread_mutex_unlock(& mutexp->mutex);
              return (ret);
            }
          }
        }
      }
      waited = 1;
    }
    if (waited) {
      mutexp->mutex_set_wait ++;
    } else {
      mutexp->mutex_set_nowait ++;
    }
    mutexp->locked = 1U;
    i = 5;
    while (1) {
      ret = pthread_mutex_unlock(& mutexp->mutex);
      if (ret == 14) {
        i --;
        if (! (i > 0)) {
          break;
        }
      } else {
        break;
      }
    }
    if (ret != 0) {
      goto err;
    }
  } else {
    if (nspins == dbenv___0->tas_spins) {
      mutexp->mutex_set_nowait ++;
    } else {
      if (nspins > 0U) {
        mutexp->mutex_set_spin ++;
        mutexp->mutex_set_spins += dbenv___0->tas_spins - nspins;
      } else {
        mutexp->mutex_set_wait ++;
      }
    }
    mutexp->locked = 1U;
  }
  return (0);
  err: 
  tmp___0 = strerror(ret);
  __db_err((DB_ENV const   *)dbenv___0, "unable to lock mutex: %s", tmp___0);
  return (ret);
}
}
int __db_pthread_mutex_unlock(DB_ENV *dbenv___0 , DB_MUTEX *mutexp ) 
{ int i ;
  int ret ;
  char *tmp ;

  {
  if (dbenv___0->flags & 1024U) {
    return (0);
  } else {
    if (mutexp->flags & 2U) {
      return (0);
    }
  }
  if (mutexp->flags & 128U) {
    ret = pthread_mutex_lock(& mutexp->mutex);
    if (ret != 0) {
      goto err;
    }
    mutexp->locked = 0U;
    ret = pthread_cond_signal(& mutexp->cond);
    if (ret != 0) {
      return (ret);
    }
  } else {
    mutexp->locked = 0U;
  }
  i = 5;
  while (1) {
    ret = pthread_mutex_unlock(& mutexp->mutex);
    if (ret == 14) {
      i --;
      if (! (i > 0)) {
        break;
      }
    } else {
      break;
    }
  }
  return (ret);
  err: 
  tmp = strerror(ret);
  __db_err((DB_ENV const   *)dbenv___0, "unable to unlock mutex: %s", tmp);
  return (ret);
}
}

int __db_pthread_mutex_destroy(DB_MUTEX *mutexp ) 
{ int ret ;
  char *tmp ;

  {
  if (mutexp->flags & 2U) {
    return (0);
  }
  ret = pthread_mutex_destroy(& mutexp->mutex);
  if (ret != 0) {
    tmp = strerror(ret);
    __db_err((DB_ENV const   *)((void *)0), "unable to destroy mutex: %s", tmp);
  }
  return (ret);
}
}



int __lock_put(DB_ENV *dbenv___0 , DB_LOCK *lock ) 
{ DB_LOCKTAB *lt ;
  int ret ;
  int run_dd ;

  {
  if ((unsigned int )dbenv___0->lg_handle != (unsigned int )((void *)0)) {
    if (((DB_LOG *)dbenv___0->lg_handle)->flags & 1U) {
      return (0);
    }
  }
  lt = (DB_LOCKTAB *)dbenv___0->lk_handle;
  if (! ((lt->reginfo.rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_lock(dbenv___0, & (lt->reginfo.rp)->mutex);
  }
  ret = 10;// __lock_put_nolock(dbenv___0, lock, & run_dd, 0U);
  if (! ((lt->reginfo.rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_unlock(dbenv___0, & (lt->reginfo.rp)->mutex);
  }
  if (ret == 0) {
    if (run_dd) {
      //__lock_detect(dbenv___0, ((DB_LOCKREGION *)lt->reginfo.primary)->detect, (int *)((void *)0));
    }
  }
  return (ret);
  }
}

static int __db_mutex_alloc_int(DB_ENV *dbenv___0 , REGINFO *infop , 
                                DB_MUTEX **storep ) 
{ int ret ;

  dbenv___0 = (DB_ENV *)((void *)0);
  dbenv___0 = dbenv___0;
  infop = (REGINFO *)((void *)0);
  infop = infop;
  ret = 0; //__os_calloc(dbenv___0, 1U, sizeof(DB_MUTEX ), (void *)storep);
  if (ret != 0) {
    //__db_err((DB_ENV const   *)dbenv___0, "Unable to allocate memory for mutex");
  }
  return (ret);
}


void __db_mutex_free(DB_ENV *dbenv___0 , REGINFO *infop , DB_MUTEX *mutexp ) 
{ 

  dbenv___0 = (DB_ENV *)((void *)0);
  dbenv___0 = dbenv___0;
  infop = (REGINFO *)((void *)0);
  infop = infop;
  free((void *)mutexp); //__os_free(dbenv___0, (void *)mutexp);
  return;

}


int __db_mutex_setup(DB_ENV *dbenv___0 , REGINFO *infop , 
                     void *ptr , u_int32_t flags ) 
{ DB_MUTEX *mutex ;
  REGMAINT *maint ;
  u_int32_t iflags ;
  u_int32_t offset ;
  int ret ;

  ret = 0;
  mutex = (DB_MUTEX *)((void *)0);
  if (flags & 1U) {
    ret = __db_mutex_alloc_int(dbenv___0, infop, (DB_MUTEX **)ptr);
    if (ret != 0) {
      goto err;
    }
    mutex = (*((DB_MUTEX **)ptr));
  } else {
    mutex = (DB_MUTEX *)ptr;
  }

  iflags = flags & 392U;
  switch ((int )infop->type) {
  case 2: 
    offset = (unsigned int )mutex + 1U;
    break;
  case 4: 
    offset = (unsigned int )mutex + 2U;
    break;
  default: 
    offset = (unsigned int )mutex;
    break;
  }

  maint = (REGMAINT *)((void *)0);
  ret = __db_pthread_mutex_init(dbenv___0, mutex, iflags);
 err: 
  if (ret != 0) {
    if (flags & 1U) {
      if ((unsigned int )mutex != (unsigned int )((void *)0)) {
        __db_mutex_free(dbenv___0, infop, mutex);
        (*((DB_MUTEX **)ptr)) = (DB_MUTEX *)((void *)0);
      }
    }
  }
  return (ret);
}

int __db_c_close(DBC *dbc ) 
{ DB *dbp___1 ;
  DBC *opd ;
  DBC_INTERNAL *cp ;
  DB_ENV *dbenv___0 ;
  int ret ;
  int t_ret ;

  {
  dbp___1 = dbc->dbp;
  dbenv___0 = dbp___1->dbenv;
  cp = dbc->internal;
  opd = cp->opd;
  ret = 0;
  if ((unsigned int )dbp___1->mutexp != (unsigned int )((void *)0)) {
    if (! ((dbp___1->mutexp)->flags & 2U)) {
      __db_pthread_mutex_lock(dbenv___0, dbp___1->mutexp);
    }
  }
  if ((unsigned int )opd != (unsigned int )((void *)0)) {
    opd->flags = opd->flags & 4294967294U;
    while (1) {
      if ((unsigned int )opd->links.tqe_next != (unsigned int )((void *)0)) {
        (opd->links.tqe_next)->links.tqe_prev = opd->links.tqe_prev;
      } else {
        dbp___1->active_queue.tqh_last = opd->links.tqe_prev;
      }
      (*(opd->links.tqe_prev)) = opd->links.tqe_next;
      break;
    }
  }
  dbc->flags = dbc->flags & 4294967294U;
  while (1) {
    if ((unsigned int )dbc->links.tqe_next != (unsigned int )((void *)0)) {
      (dbc->links.tqe_next)->links.tqe_prev = dbc->links.tqe_prev;
    } else {
      dbp___1->active_queue.tqh_last = dbc->links.tqe_prev;
    }
    (*(dbc->links.tqe_prev)) = dbc->links.tqe_next;
    break;
  }
  if ((unsigned int )dbp___1->mutexp != (unsigned int )((void *)0)) {
    if (! ((dbp___1->mutexp)->flags & 2U)) {
      __db_pthread_mutex_unlock(dbenv___0, dbp___1->mutexp);
    }
  }
  t_ret = ((*(dbc->c_am_close)))(dbc, 0U, (int *)((void *)0));
  if (t_ret != 0) {
    if (ret == 0) {
      ret = t_ret;
    }
  }
  if (dbenv___0->flags & 2U) {
    if (dbc->mylock.off != 0U) {
      t_ret = __lock_put(dbenv___0, & dbc->mylock);
      if (t_ret != 0) {
        if (ret == 0) {
          ret = t_ret;
        }
      }
    }
    memset((void *)(& dbc->mylock), 0, sizeof(dbc->mylock));
    if ((unsigned int )opd != (unsigned int )((void *)0)) {
      memset((void *)(& opd->mylock), 0, sizeof(opd->mylock));
    }
  }
  if ((unsigned int )dbc->txn != (unsigned int )((void *)0)) {
    (dbc->txn)->cursors = (dbc->txn)->cursors - 1U;
  }
  if ((unsigned int )dbp___1->mutexp != (unsigned int )((void *)0)) {
    if (! ((dbp___1->mutexp)->flags & 2U)) {
      __db_pthread_mutex_lock(dbenv___0, dbp___1->mutexp);
    }
  }
  if ((unsigned int )opd != (unsigned int )((void *)0)) {
    if ((unsigned int )dbc->txn != (unsigned int )((void *)0)) {
      (dbc->txn)->cursors = (dbc->txn)->cursors - 1U;
    }
    while (1) {
      opd->links.tqe_next = (DBC *)((void *)0);
      opd->links.tqe_prev = dbp___1->free_queue.tqh_last;
      (*(dbp___1->free_queue.tqh_last)) = opd;
      dbp___1->free_queue.tqh_last = & opd->links.tqe_next;
      break;
    }
    opd = (DBC *)((void *)0);
  }
  while (1) {
    dbc->links.tqe_next = (DBC *)((void *)0);
    dbc->links.tqe_prev = dbp___1->free_queue.tqh_last;
    (*(dbp___1->free_queue.tqh_last)) = dbc;
    dbp___1->free_queue.tqh_last = & dbc->links.tqe_next;
    break;
  }
  if ((unsigned int )dbp___1->mutexp != (unsigned int )((void *)0)) {
    if (! ((dbp___1->mutexp)->flags & 2U)) {
      __db_pthread_mutex_unlock(dbenv___0, dbp___1->mutexp);
    }
  }
  return (ret);
}
}



int __dbreg_id_to_db(DB_ENV *dbenv___0 , DB_TXN *txn , DB **dbpp , int32_t ndx , int inc ) 
{ int tmp ;

  {
    tmp = 0; //replaced
    *dbpp = 0;
    return (tmp);
  }
}

int __dbreg_id_to_fname(DB_LOG *dblp , int32_t lid , int have_lock , FNAME **fnamep ) 
{ 
  //simplified
  (*fnamep) = (FNAME *)((void *)0);
  return 0;

}

int __db_cursor(DB *dbp___1 , 
                DB_TXN *txn , DBC **dbcp , u_int32_t flags ) 
{ 
  (*dbcp) = 10;
  return (0);
}

static int __db_txnlist_pgnoadd(DB_ENV *dbenv___0 , DB_TXNHEAD *hp , int32_t fileid ,
                                u_int8_t *uid , char *fname , db_pgno_t pgno ) 
{ 
  // just making sure it mods enough things
  hp->head[0].lh_first = (struct __db *) malloc (sizeof(struct __db));
  (hp->head[0].lh_first)->links.le_prev = 
    (struct __db**) malloc(sizeof(struct __db *));

  return 0;
}


int __db_add_limbo(DB_ENV *dbenv___0 , void *info , 
                   int32_t fileid , db_pgno_t pgno ,
                   int32_t count ) 
{ DB_LOG *dblp ;
  FNAME *fnp ;
  int ret ;

  
  dblp = (DB_LOG *)dbenv___0->lg_handle;
  ret = __dbreg_id_to_fname(dblp, fileid, 0, & fnp);
  if (ret != 0) {
    return (ret);
  }
  while (1) {
    ret = __db_txnlist_pgnoadd(dbenv___0, (DB_TXNHEAD *)info, fileid, fnp->ufid, (char *)((void *)((u_int8_t *)dblp->reginfo.addr +
                                                                                                   fnp->name_off)),
                               pgno);
    if (ret != 0) {
      return (ret);
    }
    pgno ++;
    count --;
    if (! (count != 0)) {
      break;
    }
  }
  return (0);

}

int __db_pg_alloc_read(DB_ENV *dbenv___0 , void *recbuf , __db_pg_alloc_args **argpp ) 
{ __db_pg_alloc_args *argp ;
  u_int32_t uinttmp ;
  u_int8_t *bp ;
  int ret ;

  {
    ret = 10;//__os_malloc(dbenv___0, sizeof(__db_pg_alloc_args ) + sizeof(DB_TXN ), (void *)(& argp));
  if (ret != 0) {
    return (ret);
  }
  argp->txnid = (DB_TXN *)(argp + 1);
  bp = (u_int8_t *)recbuf;
  memcpy((void * __restrict  )(& argp->type), (void const   * __restrict  )bp, sizeof(argp->type));
  bp += sizeof(argp->type);
  memcpy((void * __restrict  )(& (argp->txnid)->txnid), (void const   * __restrict  )bp,
         sizeof((argp->txnid)->txnid));
  bp += sizeof((argp->txnid)->txnid);
  memcpy((void * __restrict  )(& argp->prev_lsn), (void const   * __restrict  )bp,
         sizeof(DB_LSN ));
  bp += sizeof(DB_LSN );
  memcpy((void * __restrict  )(& uinttmp), (void const   * __restrict  )bp, sizeof(uinttmp));
  argp->fileid = (int )uinttmp;
  bp += sizeof(uinttmp);
  memcpy((void * __restrict  )(& argp->meta_lsn), (void const   * __restrict  )bp,
         sizeof(argp->meta_lsn));
  bp += sizeof(argp->meta_lsn);
  memcpy((void * __restrict  )(& uinttmp), (void const   * __restrict  )bp, sizeof(uinttmp));
  argp->meta_pgno = uinttmp;
  bp += sizeof(uinttmp);
  memcpy((void * __restrict  )(& argp->page_lsn), (void const   * __restrict  )bp,
         sizeof(argp->page_lsn));
  bp += sizeof(argp->page_lsn);
  memcpy((void * __restrict  )(& uinttmp), (void const   * __restrict  )bp, sizeof(uinttmp));
  argp->pgno = uinttmp;
  bp += sizeof(uinttmp);
  memcpy((void * __restrict  )(& uinttmp), (void const   * __restrict  )bp, sizeof(uinttmp));
  argp->ptype = uinttmp;
  bp += sizeof(uinttmp);
  memcpy((void * __restrict  )(& uinttmp), (void const   * __restrict  )bp, sizeof(uinttmp));
  argp->next = uinttmp;
  bp += sizeof(uinttmp);
  (*argpp) = argp;
  return (0);
}
}


int __db_pg_alloc_recover(DB_ENV *dbenv___0 , 
                          DBT *dbtp , DB_LSN *lsnp , db_recops op ,
                          void *info ) 
{ __db_pg_alloc_args *argp ;
  DB *file_dbp ;
  DBC *dbc ;
  DBMETA *meta ;
  DB_MPOOLFILE *mpf ;
  PAGE *pagep ;
  db_pgno_t pgno ;
  int cmp_n ;
  int cmp_p ;
  int created ;
  int level ;
  int modified ;
  int ret ;
  int tmp ;
  int tmp___0 ;
  int __t_ret ;

  meta = (DBMETA *)((void *)0);
  pagep = (PAGE *)((void *)0);
  while (1) {
    argp = (__db_pg_alloc_args *)((void *)0);
    dbc = (DBC *)((void *)0);
    file_dbp = (DB *)((void *)0);
    mpf = (DB_MPOOLFILE *)((void *)0);
    ret = __db_pg_alloc_read(dbenv___0, dbtp->data, & argp);
    if (ret != 0) {
      goto out;
    }
    ret = __dbreg_id_to_db(dbenv___0, argp->txnid, & file_dbp, argp->fileid, 0);
    if (ret != 0) {
      if (ret == -30898) {
        ret = 0;
        goto done;
      }
      goto out;
    }
    ret = __db_cursor(file_dbp, (DB_TXN *)((void *)0), & dbc, 0U);
    if (ret != 0) {
      goto out;
    }
    dbc->flags = dbc->flags | 16U;
    mpf = file_dbp->mpf;
    break;
  }
  pgno = 0U;
  ret = __memp_fget(mpf, & pgno, 0U, (void *)(& meta));
  if (ret != 0) {
    if ((int )op == 4) {
      ret = __db_pgerr(file_dbp, pgno, ret);
      goto out;
    } else {
      if ((int )op == 1) {
        ret = __db_pgerr(file_dbp, pgno, ret);
        goto out;
      } else {
        goto done;
      }
    }
  }
  modified = 0;
  created = modified;
  ret = __memp_fget(mpf, & argp->pgno, 0U, (void *)(& pagep));
  if (ret != 0) {
    ret = __memp_fget(mpf, & argp->pgno, 1U, (void *)(& pagep));
    if (ret != 0) {
      if (ret == 28) {
        goto do_meta;
      }
      ret = __db_pgerr(file_dbp, argp->pgno, ret);
      goto out;
    }
    modified = 1;
    created = modified;
  }
  cmp_n = log_compare((DB_LSN const   *)lsnp, (DB_LSN const   *)(& pagep->lsn));
  cmp_p = log_compare((DB_LSN const   *)(& pagep->lsn), (DB_LSN const   *)(& argp->page_lsn));
  if (pagep->lsn.file == 0U) {
    cmp_p = 0;
  }
  if ((int )op == 4) {
    goto _L;
  } else {
    if ((int )op == 1) {
      _L: 
      if (cmp_p < 0) {
        if (pagep->lsn.file == 0U) {
          if (! (pagep->lsn.offset == 1U)) {
            __db_err((DB_ENV const   *)dbenv___0, "Log sequence error: page LSN %lu %lu; previous LSN %lu %lu",
                     (unsigned long )pagep->lsn.file, (unsigned long )pagep->lsn.offset,
                     (unsigned long )argp->page_lsn.file, (unsigned long )argp->page_lsn.offset);
            ret = 22;
            goto out;
          }
        } else {
          __db_err((DB_ENV const   *)dbenv___0, "Log sequence error: page LSN %lu %lu; previous LSN %lu %lu",
                   (unsigned long )pagep->lsn.file, (unsigned long )pagep->lsn.offset,
                   (unsigned long )argp->page_lsn.file, (unsigned long )argp->page_lsn.offset);
          ret = 22;
          goto out;
        }
      }
    }
  }
  if ((int )op == 4) {
    goto _L___4;
  } else {
    if ((int )op == 1) {
      _L___4: 
      if (cmp_p == 0) {
        goto _L___3;
      } else {
        if (argp->page_lsn.file == 0U) {
          if (pagep->lsn.file == 1U) {
            if (pagep->lsn.offset == 0U) {
              _L___3: 
              switch ((int )argp->ptype) {
              case 5: ;
              case 6: ;
              case 12: 
              level = 1;
              break;
              default: 
              level = 0;
              break;
              }
              while (1) {
                pagep->pgno = argp->pgno;
                pagep->prev_pgno = 0U;
                pagep->next_pgno = 0U;
                pagep->entries = (unsigned short)0;
                pagep->hf_offset = (unsigned short )file_dbp->pgsize;
                pagep->level = (unsigned char )level;
                pagep->type = (unsigned char )argp->ptype;
                break;
              }
              pagep->lsn = (*lsnp);
              modified = 1;
            } else {
              goto _L___2;
            }
          } else {
            goto _L___2;
          }
        } else {
          goto _L___2;
        }
      }
    } else {
      _L___2: 
      if ((int )op == 0) {
        goto _L___1;
      } else {
        if ((int )op == 3) {
          goto _L___1;
        } else {
          if ((int )op == 2) {
            _L___1: 
            if (cmp_n == 0) {
              goto _L___0;
            } else {
              if (created) {
                _L___0: 
                while (1) {
                  pagep->pgno = argp->pgno;
                  pagep->prev_pgno = 0U;
                  pagep->next_pgno = argp->next;
                  pagep->entries = (unsigned short)0;
                  pagep->hf_offset = (unsigned short )file_dbp->pgsize;
                  pagep->level = (unsigned char)0;
                  pagep->type = (unsigned char)0;
                  break;
                }
                pagep->lsn = argp->page_lsn;
                modified = 1;
              }
            }
          }
        }
      }
    }
  }
  if (pagep->lsn.file == 0U) {
    if (argp->page_lsn.file == 0U) {
      if ((int )op == 0) {
        goto _L___5;
      } else {
        if ((int )op == 3) {
          goto _L___5;
        } else {
          if ((int )op == 2) {
            _L___5: 
            ret = __db_add_limbo(dbenv___0, info, argp->fileid, argp->pgno, 1);
            if (ret != 0) {
              goto out;
            }
          }
        }
      }
    }
  }
  if (modified) {
    tmp = 2;
  } else {
    tmp = 0;
  }
  ret = __memp_fput(mpf, (void *)pagep, (unsigned int )tmp);
  if (ret != 0) {
    goto out;
  }
  pagep = (PAGE *)((void *)0);
  do_meta: 
  modified = 0;
  cmp_n = log_compare((DB_LSN const   *)lsnp, (DB_LSN const   *)(& ((PAGE *)meta)->lsn));
  cmp_p = log_compare((DB_LSN const   *)(& ((PAGE *)meta)->lsn), (DB_LSN const   *)(& argp->meta_lsn));
  if ((int )op == 4) {
    goto _L___6;
  } else {
    if ((int )op == 1) {
      _L___6: 
      if (cmp_p < 0) {
        if (((PAGE *)meta)->lsn.file == 0U) {
          if (! (((PAGE *)meta)->lsn.offset == 1U)) {
            __db_err((DB_ENV const   *)dbenv___0, "Log sequence error: page LSN %lu %lu; previous LSN %lu %lu",
                     (unsigned long )((PAGE *)meta)->lsn.file, (unsigned long )((PAGE *)meta)->lsn.offset,
                     (unsigned long )argp->meta_lsn.file, (unsigned long )argp->meta_lsn.offset);
            ret = 22;
            goto out;
          }
        } else {
          __db_err((DB_ENV const   *)dbenv___0, "Log sequence error: page LSN %lu %lu; previous LSN %lu %lu",
                   (unsigned long )((PAGE *)meta)->lsn.file, (unsigned long )((PAGE *)meta)->lsn.offset,
                   (unsigned long )argp->meta_lsn.file, (unsigned long )argp->meta_lsn.offset);
          ret = 22;
          goto out;
        }
      }
    }
  }
  if (cmp_p == 0) {
    if ((int )op == 4) {
      ((PAGE *)meta)->lsn = (*lsnp);
      meta->free = argp->next;
      modified = 1;
    } else {
      if ((int )op == 1) {
        ((PAGE *)meta)->lsn = (*lsnp);
        meta->free = argp->next;
        modified = 1;
      } else {
        goto _L___8;
      }
    }
  } else {
    _L___8: 
    if (cmp_n == 0) {
      if ((int )op == 0) {
        goto _L___7;
      } else {
        if ((int )op == 3) {
          goto _L___7;
        } else {
          if ((int )op == 2) {
            _L___7: 
            ((PAGE *)meta)->lsn = argp->meta_lsn;
            if (! (argp->page_lsn.file == 0U)) {
              meta->free = argp->pgno;
            }
            modified = 1;
          }
        }
      }
    }
  }
  if (argp->pgno > meta->last_pgno) {
    meta->last_pgno = argp->pgno;
    modified = 1;
  }
  if (modified) {
    tmp___0 = 2;
  } else {
    tmp___0 = 0;
  }
  ret = __memp_fput(mpf, (void *)meta, (unsigned int )tmp___0);
  if (ret != 0) {
    goto out;
  }
  meta = (DBMETA *)((void *)0);
  done: 
  (*lsnp) = argp->prev_lsn;
  ret = 0;
  out: 
  if ((unsigned int )pagep != (unsigned int )((void *)0)) {
    __memp_fput(mpf, (void *)pagep, 0U);
  }
  if ((unsigned int )meta != (unsigned int )((void *)0)) {
    __memp_fput(mpf, (void *)meta, 0U);
  }
  if (ret == 2) {
    if ((int )op == 2) {
      ret = 0;
    }
  }
  if ((unsigned int )argp != (unsigned int )((void *)0)) {
    
  }
  if ((unsigned int )dbc != (unsigned int )((void *)0)) {
    __t_ret = __db_c_close(dbc);
    if (__t_ret != 0) {
      if (ret == 0) {
        ret = __t_ret;
      }
    }
  }
  return (ret);

}

/////////////////// PRE-REQ

int __os_get_errno () {
  return 0;
}

static char ebuf[40]  ;
char *db_strerror(int error ) 
{ char *p ;

  {
  if (error == 0) {
    return ((char *)"Successful return: 0");
  }
  if (error > 0) {
    p = strerror(error);
    if ((unsigned int )p != (unsigned int )((void *)0)) {
      return (p);
    }
    goto unknown_err;
  }
  switch (error) {
  case -30999: ;
  return ((char *)"DB_DONOTINDEX: Secondary index callback returns null");
  case -30998: ;
  return ((char *)"DB_FILEOPEN: Rename or remove while file is open.");
  case -30997: ;
  return ((char *)"DB_KEYEMPTY: Non-existent key/data pair");
  case -30996: ;
  return ((char *)"DB_KEYEXIST: Key/data pair already exists");
  case -30995: ;
  return ((char *)"DB_LOCK_DEADLOCK: Locker killed to resolve a deadlock");
  case -30994: ;
  return ((char *)"DB_LOCK_NOTGRANTED: Lock not granted");
  case -30993: ;
  return ((char *)"DB_NOSERVER: Fatal error, no RPC server");
  case -30992: ;
  return ((char *)"DB_NOSERVER_HOME: Home unrecognized at server");
  case -30991: ;
  return ((char *)"DB_NOSERVER_ID: Identifier unrecognized at server");
  case -30990: ;
  return ((char *)"DB_NOTFOUND: No matching key/data pair found");
  case -30989: ;
  return ((char *)"DB_OLDVERSION: Database requires a version upgrade");
  case -30988: ;
  return ((char *)"DB_PAGE_NOTFOUND: Requested page not found");
  case -30987: ;
  return ((char *)"DB_REP_DUPMASTER: A second master site appeared");
  case -30986: ;
  return ((char *)"DB_REP_HANDLE_DEAD: Handle is no longer valid.");
  case -30985: ;
  return ((char *)"DB_REP_HOLDELECTION: Need to hold an election");
  case -30984: ;
  return ((char *)"DB_REP_ISPERM: Permanent record written");
  case -30983: ;
  return ((char *)"DB_REP_NEWMASTER: A new master has declared itself");
  case -30982: ;
  return ((char *)"DB_REP_NEWSITE: A new site has entered the system");
  case -30981: ;
  return ((char *)"DB_REP_NOTPERM: Permanent log record not written.");
  case -30980: ;
  return ((char *)"DB_REP_OUTDATED: Insufficient logs on master to recover");
  case -30979: ;
  return ((char *)"DB_REP_UNAVAIL: Unable to elect a master");
  case -30978: ;
  return ((char *)"DB_RUNRECOVERY: Fatal error, run database recovery");
  case -30977: ;
  return ((char *)"DB_SECONDARY_BAD: Secondary index inconsistent with primary");
  case -30976: ;
  return ((char *)"DB_VERIFY_BAD: Database verification failed");
  default: ;
  break;
  }
  unknown_err: 
  snprintf((char * __restrict  )(ebuf), sizeof(ebuf), (char const   * __restrict  )"Unknown error: %d",
           error);
  return (ebuf);
}
}


struct __db_globals __db_global_values ;
struct __db_globals __db_global_values  = 
     {{(struct __db_env *)((void *)0), & __db_global_values.db_envq.tqh_first}, (int (*)(int  ))((void *)0),
    (void (*)(char ** , int  ))((void *)0), (int (*)(char const   * , char *** , int * ))((void *)0),
    (int (*)(char const   * , int * ))((void *)0), (void (*)(void * ))((void *)0),
    (int (*)(int  ))((void *)0), (int (*)(char const   * , int  , u_int32_t * , u_int32_t * ,
                                          u_int32_t * ))((void *)0), (void *(*)(size_t  ))((void *)0),
    (int (*)(char * , size_t  , int  , int  , void ** ))((void *)0), (int (*)(char const   * ,
                                                                              int  
                                                                              , ...))((void *)0),
    (ssize_t (*)(int  , void * , size_t  ))((void *)0), (void *(*)(void * , size_t  ))((void *)0),
    (int (*)(char const   * , char const   * ))((void *)0), (int (*)(int  , size_t  ,
                                                                     db_pgno_t  ,
                                                                     u_int32_t  ,
                                                                     int  , int  ))((void *)0),
    (int (*)(u_long  , u_long  ))((void *)0), (int (*)(char const   * ))((void *)0),
    (int (*)(void * , size_t  ))((void *)0), (ssize_t (*)(int  , void const   * ,
                                                          size_t  ))((void *)0), (int (*)(void))((void *)0)};


static char const   __db_locker_invalid[20]  = 
  {      (char const   )'L',      (char const   )'o',      (char const   )'c',      (char const   )'k', 
        (char const   )'e',      (char const   )'r',      (char const   )' ',      (char const   )'i', 
        (char const   )'s',      (char const   )' ',      (char const   )'n',      (char const   )'o', 
        (char const   )'t',      (char const   )' ',      (char const   )'v',      (char const   )'a', 
        (char const   )'l',      (char const   )'i',      (char const   )'d',      (char const   )'\000'};


int __db_close(DB *dbp___1 , DB_TXN *txn , u_int32_t flags ) ;

int __os_unlink(DB_ENV *dbenv___0 , char const   *path ) 
{ int ret ;
  int retries ;
  char *tmp___1 ;

  {
  retries = 0;
  retry: 
  if ((unsigned int )__db_global_values.j_unlink != (unsigned int )((void *)0)) {
    ret = ((*(__db_global_values.j_unlink)))(path);
  } else {
    ret = unlink(path);
  }
  if (ret == -1) {
    ret = __os_get_errno();
    if (ret == 4) {
      goto _L;
    } else {
      if (ret == 16) {
        _L: 
        retries ++;
        if (retries < 100) {
          goto retry;
        }
      }
    }
    if (ret != 2) {
      tmp___1 = strerror(ret);
      //__db_err((DB_ENV const   *)dbenv___0, "unlink: %s: %s", path, tmp___1);
    }
  }
  return (ret);
}
}


void __os_free(DB_ENV *dbenv___0 , void *ptr ) 
{ 

  {
  dbenv___0 = (DB_ENV *)((void *)0);
  dbenv___0 = dbenv___0;
  if ((unsigned int )__db_global_values.j_free != (unsigned int )((void *)0)) {
    ((*(__db_global_values.j_free)))(ptr);
  } else {
    free(ptr);
  }
  return;
}
}


int __bam_db_close(DB *dbp___1 ) 
{ BTREE *t ;

  {
  t = (BTREE *)dbp___1->bt_internal;
  if ((unsigned int )t == (unsigned int )((void *)0)) {
    return (0);
  }
  if ((unsigned int )t->re_fp != (unsigned int )((void *)0)) {
    fclose(t->re_fp);
  }
  if ((unsigned int )t->re_source != (unsigned int )((void *)0)) {
    __os_free(dbp___1->dbenv, (void *)t->re_source);
  }
  __os_free(dbp___1->dbenv, (void *)t);
  dbp___1->bt_internal = (void *)0;
  return (0);
}
}

int __db_cursor_int(DB *dbp___1 , DB_TXN *txn , DBTYPE dbtype , db_pgno_t root , int is_opd ,
                    u_int32_t lockerid , DBC **dbcp ) 
{ DBC *dbc ;
  DBC *adbc ;
  DBC_INTERNAL *cp ;
  DB_ENV *dbenv___0 ;
  int allocated ;
  int ret ;

  
  return 0;
}


int __db_not_txn_env(DB_ENV *dbenv___0 ) 
{ 
    if (dbenv___0) {
      
      return (22);
    } else return 0;
}


int __db_check_txn(DB *dbp___1 , DB_TXN *txn , u_int32_t assoc_lid , int read_op ) 
{ DB_ENV *dbenv___0 ;
  int tmp ;

  {
  dbenv___0 = dbp___1->dbenv;
  if ((unsigned int )dbenv___0->lg_handle != (unsigned int )((void *)0)) {
    if (((DB_LOG *)dbenv___0->lg_handle)->flags & 1U) {
      return (0);
    } else {
      goto _L;
    }
  } else {
    _L: 
    if (dbp___1->flags & 2097152U) {
      return (0);
    }
  }
  if ((unsigned int )txn == (unsigned int )((void *)0)) {
    if (! read_op) {
      if (dbp___1->flags & 536870912U) {
        __db_err((DB_ENV const   *)dbenv___0, "DB handle previously used in transaction, missing transaction handle");
        return (22);
      }
    }
    if (dbp___1->cur_lid >= 2147483648U) {
      goto open_err;
    }
  } else {
    if (dbp___1->cur_lid >= 2147483648U) {
      if (dbp___1->cur_lid != txn->txnid) {
        goto open_err;
      }
    }
    if (! ((unsigned int )dbenv___0->tx_handle != (unsigned int )((void *)0))) {
      tmp = __db_not_txn_env(dbenv___0);
      return (tmp);
    }
    if (! (dbp___1->flags & 536870912U)) {
      __db_err((DB_ENV const   *)dbenv___0, "Transaction specified for a DB handle opened outside a transaction");
      return (22);
    }
  }
  if (! read_op) {
    if (dbp___1->associate_lid != 0U) {
      if ((unsigned int )txn != (unsigned int )((void *)0)) {
        if (dbp___1->associate_lid != assoc_lid) {
          __db_err((DB_ENV const   *)dbenv___0, "Operation forbidden while secondary index is being created");
          return (22);
        }
      }
    }
  }
  return (0);
  open_err: 
  __db_err((DB_ENV const   *)dbenv___0, "Transaction that opened the DB handle is still active");
  return (22);
}
}




static int __db_wrlock_err(DB_ENV *dbenv___0 ) 
{ 

    if (dbenv___0) {//__db_err((DB_ENV const   *)dbenv___0, "Write attempted on read-only cursor");
      return (1);
    } else {
      return 0;
    }
}

int __lock_get(DB_ENV *dbenv___0 , u_int32_t locker , u_int32_t flags , DBT const   *obj ,
               db_lockmode_t lock_mode , DB_LOCK *lock ) 
{ int ret ;

  {
  if ((unsigned int )dbenv___0->lg_handle != (unsigned int )((void *)0)) {
    if (((DB_LOG *)dbenv___0->lg_handle)->flags & 1U) {
      lock->off = 0U;
      return (0);
    }
  }
  if (! ((((DB_LOCKTAB *)dbenv___0->lk_handle)->reginfo.rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_lock(dbenv___0, & (((DB_LOCKTAB *)dbenv___0->lk_handle)->reginfo.rp)->mutex);
  }
  ret = 0;//__lock_get_internal((DB_LOCKTAB *)dbenv___0->lk_handle, locker, flags, obj, lock_mode, 0U, lock);
  if (! ((((DB_LOCKTAB *)dbenv___0->lk_handle)->reginfo.rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_unlock(dbenv___0, & (((DB_LOCKTAB *)dbenv___0->lk_handle)->reginfo.rp)->mutex);
  }
  return (ret);
}
}

int __lock_downgrade(DB_ENV *dbenv___0 , DB_LOCK *lock , db_lockmode_t new_mode ,
                     u_int32_t flags ) 
{ struct __db_lock *lockp ;
  DB_LOCKER *sh_locker ;
  DB_LOCKOBJ *obj ;
  DB_LOCKREGION *region ;
  DB_LOCKTAB *lt ;
  u_int32_t indx ;
  int ret ;
  int tmp ;
  u_int32_t tmp___0 ;

  {
  flags = 0U;
  flags = flags;
  if (! (dbenv___0->flags & 4096U)) {
    if ((unsigned int )dbenv___0->reginfo != (unsigned int )((void *)0)) {
      if (((REGENV *)((REGINFO *)dbenv___0->reginfo)->primary)->envpanic != 0) {
        tmp = 0;//__db_panic_msg(dbenv___0);
        return (tmp);
      }
    }
  }
  ret = 0;
  if (dbenv___0->flags & 1024U) {
    return (0);
  }
  lt = (DB_LOCKTAB *)dbenv___0->lk_handle;
  region = (DB_LOCKREGION *)lt->reginfo.primary;
  if (! ((lt->reginfo.rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_lock(dbenv___0, & (lt->reginfo.rp)->mutex);
  }
  lockp = (struct __db_lock *)((void *)((u_int8_t *)lt->reginfo.addr + lock->off));
  if (lock->gen != lockp->gen) {
    __db_err((DB_ENV const   *)dbenv___0, __db_lock_invalid, "lock_downgrade");
    ret = 22;
    goto out;
  }
  tmp___0 = 0;//__lock_locker_hash(lockp->holder);
  indx = tmp___0 % region->locker_t_size;
  ret = 0;//__lock_getlocker(lt, lockp->holder, indx, 0, & sh_locker);
  if (ret != 0) {
    goto _L;
  } else {
    if ((unsigned int )sh_locker == (unsigned int )((void *)0)) {
      _L: 
      if (ret == 0) {
        ret = 22;
      }
      __db_err((DB_ENV const   *)dbenv___0, __db_locker_invalid);
      goto out;
    }
  }
  if ((int )lockp->mode == 2) {
    goto _L___0;
  } else {
    if ((int )lockp->mode == 8) {
      goto _L___0;
    } else {
      if ((int )lockp->mode == 4) {
        goto _L___0;
      } else {
        if ((int )lockp->mode == 6) {
          _L___0: 
          if (! ((int )new_mode == 2)) {
            if (! ((int )new_mode == 8)) {
              if (! ((int )new_mode == 4)) {
                if (! ((int )new_mode == 6)) {
                  sh_locker->nwrites --;
                }
              }
            }
          }
        }
      }
    }
  }
  if ((int )new_mode == 8) {
    sh_locker->flags |= 2U;
  }
  lockp->mode = new_mode;
  obj = (DB_LOCKOBJ *)((u_int8_t *)lockp + lockp->obj);
  //__lock_promote(lt, obj, flags & 1048576U);
  out: 
  if (! ((lt->reginfo.rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_unlock(dbenv___0, & (lt->reginfo.rp)->mutex);
  }
  return (ret);
}
}


DB *__db_s_first(DB *pdbp ) 
{ DB *sdbp ;

  {
  if ((unsigned int )pdbp->mutexp != (unsigned int )((void *)0)) {
    if (! ((pdbp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_lock(pdbp->dbenv, pdbp->mutexp);
    }
  }
  sdbp = pdbp->s_secondaries.lh_first;
  if ((unsigned int )sdbp != (unsigned int )((void *)0)) {
    sdbp->s_refcnt = sdbp->s_refcnt + 1U;
  }
  if ((unsigned int )pdbp->mutexp != (unsigned int )((void *)0)) {
    if (! ((pdbp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_unlock(pdbp->dbenv, pdbp->mutexp);
    }
  }
  return (sdbp);
}
}

int __db_secondary_corrupt(DB *dbp___1 ) 
{ 
    if(dbp___1->dbenv) { //__db_err((DB_ENV const   *)dbp___1->dbenv, "Secondary index corrupt: not consistent with primary");
      return (-30977);
    } else {
      return 0;
    }
}



void __os_ufree(DB_ENV *dbenv___0 , void *ptr ) 
{ 

  {
  if ((unsigned int )dbenv___0 != (unsigned int )((void *)0)) {
    if ((unsigned int )dbenv___0->db_free != (unsigned int )((void *)0)) {
      ((*(dbenv___0->db_free)))(ptr);
    } else {
      goto _L;
    }
  } else {
    _L: 
    if ((unsigned int )__db_global_values.j_free != (unsigned int )((void *)0)) {
      ((*(__db_global_values.j_free)))(ptr);
    } else {
      free(ptr);
    }
  }
  return;
}
}



int __db_s_next(DB **sdbpp ) 
{ DB *sdbp ;
  DB *pdbp ;
  DB *closeme ;
  int ret ;

  {
  sdbp = (*sdbpp);
  pdbp = sdbp->s_primary;
  closeme = (DB *)((void *)0);
  if ((unsigned int )pdbp->mutexp != (unsigned int )((void *)0)) {
    if (! ((pdbp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_lock(pdbp->dbenv, pdbp->mutexp);
    }
  }
  sdbp->s_refcnt = sdbp->s_refcnt - 1U;
  if (sdbp->s_refcnt == 0U) {
    while (1) {
      if ((unsigned int )sdbp->s_links.le_next != (unsigned int )((void *)0)) {
        (sdbp->s_links.le_next)->s_links.le_prev = sdbp->s_links.le_prev;
      }
      (*(sdbp->s_links.le_prev)) = sdbp->s_links.le_next;
      break;
    }
    closeme = sdbp;
  }
  sdbp = sdbp->s_links.le_next;
  if ((unsigned int )sdbp != (unsigned int )((void *)0)) {
    sdbp->s_refcnt = sdbp->s_refcnt + 1U;
  }
  if ((unsigned int )pdbp->mutexp != (unsigned int )((void *)0)) {
    if (! ((pdbp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_unlock(pdbp->dbenv, pdbp->mutexp);
    }
  }
  (*sdbpp) = sdbp;
  if ((unsigned int )closeme != (unsigned int )((void *)0)) {
    ret = __db_close(closeme, (DB_TXN *)((void *)0), 0U);
  } else {
    ret = 0;
  }
  return (ret);
}
}

extern  __attribute__((__nothrow__)) void *mmap(void *__addr , size_t __len , int __prot ,
                                                int __flags , int __fd , __off_t __offset ) ;
extern  __attribute__((__nothrow__)) int munmap(void *__addr , size_t __len ) ;
extern  __attribute__((__nothrow__)) int mlock(void const   *__addr , size_t __len ) ;
extern  __attribute__((__nothrow__)) int munlock(void const   *__addr , size_t __len ) ;
extern  __attribute__((__nothrow__)) int shmctl(int __shmid , int __cmd , struct shmid_ds *__buf ) ;
extern  __attribute__((__nothrow__)) int shmget(key_t __key , size_t __size , int __shmflg ) ;
extern  __attribute__((__nothrow__)) void *shmat(int __shmid , void const   *__shmaddr ,
                                                 int __shmflg ) ;
extern  __attribute__((__nothrow__)) int shmdt(void const   *__shmaddr ) ;



int __os_unmapfile(DB_ENV *dbenv___0 , void *addr , size_t len ) 
{ int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int err ;
  int ret ;
  int retries ;
  int tmp___2 ;

  {
  if ((unsigned int )__db_global_values.j_unmap != (unsigned int )((void *)0)) {
    tmp = ((*(__db_global_values.j_unmap)))(addr, len);
    return (tmp);
  }
  if (dbenv___0->flags & 256U) {
    while (1) {
      tmp___0 = munlock((void const   *)addr, len);
      if (tmp___0 != 0) {
        tmp___1 = 0;//__os_get_errno();
        if (! (tmp___1 == 4)) {
          break;
        }
      } else {
        break;
      }
    }
  }
  retries = 0;
  err = retries;
  while (1) {
    ret = munmap(addr, len);
    if (ret != 0) {
      err = 0;//__os_get_errno();
      if (err == 4) {
        goto _L;
      } else {
        if (err == 16) {
          _L: 
          retries ++;
          if (! (retries < 100)) {
            break;
          }
        } else {
          break;
        }
      }
    } else {
      break;
    }
  }
  if (ret) {
    tmp___2 = err;
  } else {
    tmp___2 = 0;
  }
  return (tmp___2);
}
}


char *__memp_fns(DB_MPOOL *dbmp , MPOOLFILE *mfp ) 
{ 

  {
  if (mfp->path_off == 0U) {
    return ((char *)"temporary");
  }
  return ((char *)((void *)((u_int8_t *)(dbmp->reginfo)->addr + mfp->path_off)));
}
}

char *__memp_fn(DB_MPOOLFILE *dbmfp ) 
{ char *tmp ;

  {
  tmp = __memp_fns((DB_MPOOL *)(dbmfp->dbenv)->mp_handle, dbmfp->mfp);
  return (tmp);
}
}

int __memp_mf_sync(DB_MPOOL *dbmp , MPOOLFILE *mfp ) 
{ DB_ENV *dbenv___0 ;

  return 0;
}


void __db_shalloc_free(void *regionp , void *ptr ) 
{ struct __data *elp ;
  struct __data *lastp ;
  struct __data *newp ;
  struct __head *hp ;
  size_t free_size ;
  size_t *sp ;
  int merged ;
  int tmp ;
  int tmp___0 ;
  int tmp___1 ;
  int tmp___2 ;
  int tmp___3 ;
  int tmp___4 ;

  {
  sp = (size_t *)ptr;
  while ((*(sp + -1)) == 1U) {
    sp --;
  }
  ptr = (void *)sp;
  newp = (struct __data *)((u_int8_t *)ptr - sizeof(size_t ));
  free_size = newp->len;
  hp = (struct __head *)regionp;
  if (hp->slh_first == -1) {
    elp = (struct __data *)((void *)0);
  } else {
    elp = (struct __data *)((u_int8_t *)hp + hp->slh_first);
  }
  lastp = (struct __data *)((void *)0);
  while (1) {
    if ((unsigned int )elp != (unsigned int )((void *)0)) {
      if (! ((unsigned int )((void *)elp) < (unsigned int )ptr)) {
        break;
      }
    } else {
      break;
    }
    lastp = elp;
    if (elp->links.sle_next == -1) {
      elp = (struct __data *)((void *)0);
    } else {
      elp = (struct __data *)((u_int8_t *)elp + elp->links.sle_next);
    }
  }
  merged = 0;
  if ((unsigned int )((u_int8_t *)ptr + free_size) == (unsigned int )((u_int8_t *)elp)) {
    newp->len += elp->len + sizeof(size_t );
    while (1) {
      if (elp->links.sle_next != -1) {
        ((struct __data *)((u_int8_t *)elp + elp->links.sle_next))->links.sle_prev = elp->links.sle_prev -
                                                                                     elp->links.sle_next;
        (*((ssize_t *)((u_int8_t *)elp + elp->links.sle_prev))) = (*((ssize_t *)((u_int8_t *)elp +
                                                                                 elp->links.sle_prev))) +
                                                                  elp->links.sle_next;
      } else {
        (*((ssize_t *)((u_int8_t *)elp + elp->links.sle_prev))) = -1;
      }
      break;
    }
    if ((unsigned int )lastp != (unsigned int )((void *)0)) {
      while (1) {
        if (lastp->links.sle_next != -1) {
          newp->links.sle_next = (u_int8_t *)((struct __data *)((u_int8_t *)lastp +
                                                                lastp->links.sle_next)) -
                                 (u_int8_t *)newp;
          if (newp->links.sle_next == -1) {
            tmp = 0;
          } else {
            tmp = - newp->links.sle_next;
          }
          ((struct __data *)((u_int8_t *)lastp + lastp->links.sle_next))->links.sle_prev = tmp +
                                                                                           ((u_int8_t *)(& newp->links.sle_next) -
                                                                                            (u_int8_t *)newp);
        } else {
          newp->links.sle_next = -1;
        }
        lastp->links.sle_next = (u_int8_t *)newp - (u_int8_t *)lastp;
        if (lastp->links.sle_next == -1) {
          tmp___0 = 0;
        } else {
          tmp___0 = - lastp->links.sle_next;
        }
        newp->links.sle_prev = tmp___0 + ((u_int8_t *)(& lastp->links.sle_next) -
                                          (u_int8_t *)lastp);
        break;
      }
    } else {
      while (1) {
        if (hp->slh_first != -1) {
          newp->links.sle_next = hp->slh_first - ((u_int8_t *)newp - (u_int8_t *)hp);
          if (newp->links.sle_next == -1) {
            tmp___1 = 0;
          } else {
            tmp___1 = - newp->links.sle_next;
          }
          ((struct __data *)((u_int8_t *)hp + hp->slh_first))->links.sle_prev = tmp___1 +
                                                                                ((u_int8_t *)(& newp->links.sle_next) -
                                                                                 (u_int8_t *)newp);
        } else {
          newp->links.sle_next = -1;
        }
        hp->slh_first = (u_int8_t *)newp - (u_int8_t *)hp;
        newp->links.sle_prev = (u_int8_t *)(& hp->slh_first) - (u_int8_t *)newp;
        break;
      }
    }
    merged = 1;
  }
  if ((unsigned int )lastp != (unsigned int )((void *)0)) {
    if ((unsigned int )(((u_int8_t *)lastp + lastp->len) + sizeof(size_t )) == (unsigned int )((u_int8_t *)newp)) {
      lastp->len += newp->len + sizeof(size_t );
      if (merged) {
        while (1) {
          if (newp->links.sle_next != -1) {
            ((struct __data *)((u_int8_t *)newp + newp->links.sle_next))->links.sle_prev = newp->links.sle_prev -
                                                                                           newp->links.sle_next;
            (*((ssize_t *)((u_int8_t *)newp + newp->links.sle_prev))) = (*((ssize_t *)((u_int8_t *)newp +
                                                                                       newp->links.sle_prev))) +
                                                                        newp->links.sle_next;
          } else {
            (*((ssize_t *)((u_int8_t *)newp + newp->links.sle_prev))) = -1;
          }
          break;
        }
      }
      merged = 1;
    }
  }
  if (! merged) {
    if ((unsigned int )lastp == (unsigned int )((void *)0)) {
      while (1) {
        if (hp->slh_first != -1) {
          newp->links.sle_next = hp->slh_first - ((u_int8_t *)newp - (u_int8_t *)hp);
          if (newp->links.sle_next == -1) {
            tmp___2 = 0;
          } else {
            tmp___2 = - newp->links.sle_next;
          }
          ((struct __data *)((u_int8_t *)hp + hp->slh_first))->links.sle_prev = tmp___2 +
                                                                                ((u_int8_t *)(& newp->links.sle_next) -
                                                                                 (u_int8_t *)newp);
        } else {
          newp->links.sle_next = -1;
        }
        hp->slh_first = (u_int8_t *)newp - (u_int8_t *)hp;
        newp->links.sle_prev = (u_int8_t *)(& hp->slh_first) - (u_int8_t *)newp;
        break;
      }
    } else {
      while (1) {
        if (lastp->links.sle_next != -1) {
          newp->links.sle_next = (u_int8_t *)((struct __data *)((u_int8_t *)lastp +
                                                                lastp->links.sle_next)) -
                                 (u_int8_t *)newp;
          if (newp->links.sle_next == -1) {
            tmp___3 = 0;
          } else {
            tmp___3 = - newp->links.sle_next;
          }
          ((struct __data *)((u_int8_t *)lastp + lastp->links.sle_next))->links.sle_prev = tmp___3 +
                                                                                           ((u_int8_t *)(& newp->links.sle_next) -
                                                                                            (u_int8_t *)newp);
        } else {
          newp->links.sle_next = -1;
        }
        lastp->links.sle_next = (u_int8_t *)newp - (u_int8_t *)lastp;
        if (lastp->links.sle_next == -1) {
          tmp___4 = 0;
        } else {
          tmp___4 = - lastp->links.sle_next;
        }
        newp->links.sle_prev = tmp___4 + ((u_int8_t *)(& lastp->links.sle_next) -
                                          (u_int8_t *)lastp);
        break;
      }
    }
  }
  return;
}
}



int __memp_mf_discard(DB_MPOOL *dbmp , MPOOLFILE *mfp ) 
{ DB_ENV *dbenv___0 ;
  DB_MPOOL_STAT *sp ;
  MPOOL *mp ;
  int ret ;

  {
  dbenv___0 = dbmp->dbenv;
  mp = (MPOOL *)(dbmp->reginfo + 0)->primary;
  ret = 0;
  if (mfp->file_written) {
    if (! mfp->deadfile) {
      ret = __memp_mf_sync(dbmp, mfp);
    }
  }
  mfp->deadfile = 1;
  if (! (mfp->mutex.flags & 2U)) {
    __db_pthread_mutex_unlock(dbenv___0, & mfp->mutex);
  }
  if (! (((dbmp->reginfo)->rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_lock(dbenv___0, & ((dbmp->reginfo)->rp)->mutex);
  }
  while (1) {
    if (mfp->q.stqe_next != -1) {
      ((struct __mpoolfile *)((u_int8_t *)mfp + mfp->q.stqe_next))->q.stqe_prev = mfp->q.stqe_prev +
                                                                                  ((u_int8_t *)mfp -
                                                                                   (u_int8_t *)((struct __mpoolfile *)((u_int8_t *)mfp +
                                                                                                                       mfp->q.stqe_next)));
      (*((ssize_t *)((u_int8_t *)mfp + mfp->q.stqe_prev))) = (*((ssize_t *)((u_int8_t *)mfp +
                                                                            mfp->q.stqe_prev))) +
                                                             mfp->q.stqe_next;
    } else {
      mp->mpfq.stqh_last = mfp->q.stqe_prev + ((u_int8_t *)mfp - (u_int8_t *)(& mp->mpfq));
      (*((ssize_t *)((u_int8_t *)mfp + mfp->q.stqe_prev))) = -1;
    }
    break;
  }
  sp = & mp->stat;
  sp->st_cache_hit = sp->st_cache_hit + mfp->stat.st_cache_hit;
  sp->st_cache_miss = sp->st_cache_miss + mfp->stat.st_cache_miss;
  sp->st_map = sp->st_map + mfp->stat.st_map;
  sp->st_page_create = sp->st_page_create + mfp->stat.st_page_create;
  sp->st_page_in = sp->st_page_in + mfp->stat.st_page_in;
  sp->st_page_out = sp->st_page_out + mfp->stat.st_page_out;
  if (mfp->path_off != 0U) {
    __db_shalloc_free((dbmp->reginfo + 0)->addr, (void *)((u_int8_t *)(dbmp->reginfo)->addr +
                                                          mfp->path_off));
  }
  if (mfp->fileid_off != 0U) {
    __db_shalloc_free((dbmp->reginfo + 0)->addr, (void *)((u_int8_t *)(dbmp->reginfo)->addr +
                                                          mfp->fileid_off));
  }
  if (mfp->pgcookie_off != 0U) {
    __db_shalloc_free((dbmp->reginfo + 0)->addr, (void *)((u_int8_t *)(dbmp->reginfo)->addr +
                                                          mfp->pgcookie_off));
  }
  __db_shalloc_free((dbmp->reginfo + 0)->addr, (void *)mfp);
  if (! (((dbmp->reginfo)->rp)->mutex.flags & 2U)) {
    __db_pthread_mutex_unlock(dbenv___0, & ((dbmp->reginfo)->rp)->mutex);
  }
  return (ret);
}
}


int __os_closehandle(DB_ENV *dbenv___0 , DB_FH *fhp ) 
{ int ret ;
  int retries ;
  char *tmp___1 ;

  {
  ret = 0;
  if ((int )fhp->flags & 2) {
    retries = 0;
    while (1) {
      if ((unsigned int )__db_global_values.j_close != (unsigned int )((void *)0)) {
        ret = ((*(__db_global_values.j_close)))(fhp->fd);
      } else {
        ret = close(fhp->fd);
      }
      if (ret != 0) {
        ret = __os_get_errno();
        if (ret == 4) {
          goto _L;
        } else {
          if (ret == 16) {
            _L: 
            retries ++;
            if (! (retries < 100)) {
              break;
            }
          } else {
            break;
          }
        }
      } else {
        break;
      }
    }
    if (ret != 0) {
      tmp___1 = strerror(ret);
      //__db_err((DB_ENV const   *)dbenv___0, "close: %s", tmp___1);
    }
    if ((int )fhp->flags & 4) {
      __os_unlink(dbenv___0, (char const   *)fhp->name);
      __os_free(dbenv___0, (void *)fhp->name);
    }
  }
  __os_free(dbenv___0, (void *)fhp);
  return (ret);
}
}

int __db_appname(DB_ENV *dbenv___0 , APPNAME appname , char const   *file , u_int32_t tmp_oflags ,
                 DB_FH **fhpp , char **namep ) 
{ 

  return 0;
}

int __memp_fclose(DB_MPOOLFILE *dbmfp , u_int32_t flags ) 
{ DB_ENV *dbenv___0 ;
  DB_MPOOL *dbmp ;
  MPOOLFILE *mfp ;
  char *rpath ;
  u_int32_t ref ;
  int deleted ;
  int ret ;
  int t_ret ;
  char *tmp ;
  char *tmp___0 ;
  char *tmp___1 ;
  char *tmp___2 ;
  char *tmp___3 ;
  int tmp___4 ;

  {
  dbenv___0 = dbmfp->dbenv;
  dbmp = (DB_MPOOL *)dbenv___0->mp_handle;
  ret = 0;
  if ((unsigned int )dbmp == (unsigned int )((void *)0)) {
    goto done;
  }
  if ((unsigned int )dbmp->mutexp != (unsigned int )((void *)0)) {
    if (! ((dbmp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_lock(dbenv___0, dbmp->mutexp);
    }
  }
  dbmfp->ref = dbmfp->ref - 1U;
  ref = dbmfp->ref;
  if (ref == 0U) {
    if (dbmfp->flags & 4U) {
      while (1) {
        if ((unsigned int )dbmfp->q.tqe_next != (unsigned int )((void *)0)) {
          (dbmfp->q.tqe_next)->q.tqe_prev = dbmfp->q.tqe_prev;
        } else {
          dbmp->dbmfq.tqh_last = dbmfp->q.tqe_prev;
        }
        (*(dbmfp->q.tqe_prev)) = dbmfp->q.tqe_next;
        break;
      }
    }
  }
  if (ref == 0U) {
    if ((unsigned int )dbmfp->fhp != (unsigned int )((void *)0)) {
      (dbmfp->fhp)->ref = (dbmfp->fhp)->ref - 1;
      if ((dbmfp->fhp)->ref > 0) {
        dbmfp->fhp = (DB_FH *)((void *)0);
      }
    }
  }
  if ((unsigned int )dbmp->mutexp != (unsigned int )((void *)0)) {
    if (! ((dbmp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_unlock(dbenv___0, dbmp->mutexp);
    }
  }
  if (ref != 0U) {
    return (0);
  }
  if (dbmfp->pinref != 0U) {
    tmp = __memp_fn(dbmfp);
    __db_err((DB_ENV const   *)dbenv___0, "%s: close: %lu blocks left pinned", tmp,
             (unsigned long )dbmfp->pinref);
    ret = __db_panic(dbenv___0, -30978);
  }
  if ((unsigned int )dbmfp->addr != (unsigned int )((void *)0)) {
    ret = __os_unmapfile(dbenv___0, dbmfp->addr, dbmfp->len);
    if (ret != 0) {
      tmp___0 = db_strerror(ret);
      tmp___1 = __memp_fn(dbmfp);
      __db_err((DB_ENV const   *)dbenv___0, "%s: %s", tmp___1, tmp___0);
    }
  }
  if ((unsigned int )dbmfp->fhp != (unsigned int )((void *)0)) {
    if ((unsigned int )(dbmfp->fhp)->mutexp != (unsigned int )((void *)0)) {
      __db_mutex_free(dbenv___0, dbmp->reginfo, (dbmfp->fhp)->mutexp);
      (dbmfp->fhp)->mutexp = (DB_MUTEX *)((void *)0);
    }
    t_ret = __os_closehandle(dbenv___0, dbmfp->fhp);
    if (t_ret != 0) {
      tmp___2 = db_strerror(t_ret);
      tmp___3 = __memp_fn(dbmfp);
      __db_err((DB_ENV const   *)dbenv___0, "%s: %s", tmp___3, tmp___2);
      if (ret == 0) {
        ret = t_ret;
      }
    }
    dbmfp->fhp = (DB_FH *)((void *)0);
  }
  mfp = dbmfp->mfp;
  if (! (dbmfp->flags & 4U)) {
    goto done;
  }
  deleted = 0;
  if (! (mfp->mutex.flags & 2U)) {
    __db_pthread_mutex_lock(dbenv___0, & mfp->mutex);
  }
  mfp->mpf_cnt = mfp->mpf_cnt - 1U;
  if (mfp->mpf_cnt == 0U) {
    goto _L;
  } else {
    if (flags & 4U) {
      _L: 
      if (flags & 4U) {
        mfp->deadfile = 1;
      } else {
        if (mfp->flags & 256U) {
          mfp->deadfile = 1;
        } else {
          if (mfp->unlink_on_close) {
            mfp->deadfile = 1;
          }
        }
      }
      if (mfp->unlink_on_close) {
        t_ret = __db_appname(dbmp->dbenv, (enum __anonenum_APPNAME_39 )1, (char const   *)((void *)((u_int8_t *)(dbmp->reginfo)->addr +
                                                                                                    mfp->path_off)),
                             0U, (DB_FH **)((void *)0), & rpath);
        if (t_ret != 0) {
          if (ret == 0) {
            ret = t_ret;
          }
        }
        if (t_ret == 0) {
          tmp___4 = __os_unlink(dbmp->dbenv, (char const   *)rpath);
          t_ret = tmp___4 != 0;
          if (t_ret) {
            if (ret == 0) {
              ret = t_ret;
            }
          }
          __os_free(dbenv___0, (void *)rpath);
        }
      }
      if (mfp->block_cnt == 0U) {
        t_ret = __memp_mf_discard(dbmp, mfp);
        if (t_ret != 0) {
          if (ret == 0) {
            ret = t_ret;
          }
        }
        deleted = 1;
      }
    }
  }
  if (deleted == 0) {
    if (! (mfp->mutex.flags & 2U)) {
      __db_pthread_mutex_unlock(dbenv___0, & mfp->mutex);
    }
  }
  done: 
  if ((unsigned int )dbmfp->pgcookie != (unsigned int )((void *)0)) {
    __os_free(dbenv___0, (dbmfp->pgcookie)->data);
    __os_free(dbenv___0, (void *)dbmfp->pgcookie);
  }
  __os_free(dbenv___0, (void *)dbmfp);
  return (ret);
}
}



int __qam_nameop(DB *dbp___1 , DB_TXN *txn , char const   *newname , qam_name_op op ) {

  if (dbp___1->dbenv && dbp___1->q_internal && 
      ((QUEUE*)dbp___1->q_internal)->page_ext)
    return 1;
  else
    return 0;

}

int __ham_db_close(DB *dbp___1 ) 
{ 

  {
  if ((unsigned int )dbp___1->h_internal == (unsigned int )((void *)0)) {
    return (0);
  }
  __os_free(dbp___1->dbenv, dbp___1->h_internal);
  dbp___1->h_internal = (void *)0;
  return (0);
}
}




int __qam_db_close(DB *dbp___1 , u_int32_t flags ) 
{ DB_MPOOLFILE *mpf ;
  MPFARRAY *array ;
  QUEUE *t ;
  struct __qmpf *mpfp ;
  u_int32_t i ;
  int ret ;
  int t_ret ;
  int tmp ;

  {
  ret = 0;
  t = (QUEUE *)dbp___1->q_internal;
  if ((unsigned int )t == (unsigned int )((void *)0)) {
    return (0);
  }
  array = & t->array1;
  again: 
  mpfp = array->mpfarray;
  if ((unsigned int )mpfp != (unsigned int )((void *)0)) {
    i = array->low_extent;
    while (i <= array->hi_extent) {
      mpf = mpfp->mpf;
      mpfp->mpf = (DB_MPOOLFILE *)((void *)0);
      if ((unsigned int )mpf != (unsigned int )((void *)0)) {
        if (flags & 256U) {
          tmp = 4;
        } else {
          tmp = 0;
        }
        t_ret = __memp_fclose(mpf, (unsigned int )tmp);
        if (t_ret != 0) {
          if (ret == 0) {
            ret = t_ret;
          }
        }
      }
      i ++;
      mpfp ++;
    }
    __os_free(dbp___1->dbenv, (void *)array->mpfarray);
  }
  if (t->array2.n_extent != 0U) {
    array = & t->array2;
    array->n_extent = 0U;
    goto again;
  }
  if (flags & 256U) {
    t_ret = __qam_nameop(dbp___1, (DB_TXN *)((void *)0), (char const   *)((void *)0),
                         (enum __anonenum_qam_name_op_78 )0);
    if (t_ret != 0) {
      if (ret == 0) {
        ret = t_ret;
      }
    }
  }
  if ((unsigned int )t->path != (unsigned int )((void *)0)) {
    __os_free(dbp___1->dbenv, (void *)t->path);
  }
  __os_free(dbp___1->dbenv, (void *)t);
  dbp___1->q_internal = (void *)0;
  return (ret);
}
}

int __dbenv_close(DB_ENV *dbenv___0 , int rep_check ) 
{ int ret ;
  int t_ret ;
  char **p ;
  
  if (__db_global_values.j_free &&
      dbenv___0->crypto_handle &&
      dbenv___0->passwd &&
      dbenv___0->passwd_len &&
      ((DB_CIPHER *)dbenv___0->crypto_handle)->data &&
      ((DB_CIPHER *)dbenv___0->crypto_handle)->flags)
    dbenv___0->passwd = 0;
  else
    dbenv___0->passwd = 1;

  return 0;
      
}


int __db_s_done(DB *sdbp ) 
{ DB *pdbp ;
  int doclose ;
  int tmp___0 ;

  {
  pdbp = sdbp->s_primary;
  doclose = 0;
  if ((unsigned int )pdbp->mutexp != (unsigned int )((void *)0)) {
    if (! ((pdbp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_lock(pdbp->dbenv, pdbp->mutexp);
    }
  }
  sdbp->s_refcnt = sdbp->s_refcnt - 1U;
  if (sdbp->s_refcnt == 0U) {
    while (1) {
      if ((unsigned int )sdbp->s_links.le_next != (unsigned int )((void *)0)) {
        (sdbp->s_links.le_next)->s_links.le_prev = sdbp->s_links.le_prev;
      }
      (*(sdbp->s_links.le_prev)) = sdbp->s_links.le_next;
      break;
    }
    doclose = 1;
  }
  if ((unsigned int )pdbp->mutexp != (unsigned int )((void *)0)) {
    if (! ((pdbp->mutexp)->flags & 2U)) {
      __db_pthread_mutex_unlock(pdbp->dbenv, pdbp->mutexp);
    }
  }
  if (doclose) {
    tmp___0 = __db_close(sdbp, (DB_TXN *)((void *)0), 0U);
  } else {
    tmp___0 = 0;
  }
  return (tmp___0);
}
}

int __bam_c_rget(DBC *dbc , DBT *data ) 
{ 

  return 0;
}


/////////////////// IN BIG SCC

int __db_c_get(DBC *dbc_arg , DBT *key , DBT *data , u_int32_t flags ) 
{ DB *dbp___1 ;
  DBC *dbc ;
  DBC *dbc_n ;
  DBC *opd ;
  DBC_INTERNAL *cp ;
  DBC_INTERNAL *cp_n ;
  DB_MPOOLFILE *mpf ;
  db_pgno_t pgno ;
  u_int32_t multi ;
  u_int32_t tmp_dirty ;
  u_int32_t tmp_flags ;
  u_int32_t tmp_rmw ;
  u_int8_t type ;
  int ret ;
  int t_ret ;
  int tmp ;
  int tmp___1 ;

  {
  dbp___1 = dbc_arg->dbp;
  mpf = dbp___1->mpf;
  dbc_n = (DBC *)((void *)0);
  opd = (DBC *)((void *)0);
  tmp_rmw = flags & 268435456U;
  flags &= 4026531839U;
  tmp_dirty = flags & 33554432U;
  flags &= 4261412863U;
  multi = flags & 201326592U;
  flags &= 4093640703U;
  if (flags == 13U) {
    if (tmp_rmw) {
      dbc_arg->flags = dbc_arg->flags | 32U;
    }
    if (tmp_dirty) {
      dbc_arg->flags = dbc_arg->flags | 4U;
    }
    ret = __bam_c_rget(dbc_arg, data);
    if (tmp_rmw) {
      dbc_arg->flags = dbc_arg->flags & 4294967263U;
    }
    if (tmp_dirty) {
      dbc_arg->flags = dbc_arg->flags & 4294967291U;
    }
    return (ret);
  }
  if (flags == 5U) {
    goto _L;
  } else {
    if (flags == 6U) {
      _L: 
      if ((dbp___1->dbenv)->flags & 2U) {
        if (! (dbc_arg->flags & 384U)) {
          tmp = __db_wrlock_err(dbp___1->dbenv);
          return (tmp);
        }
        if (dbc_arg->flags & 128U) {
          ret = __lock_get(dbp___1->dbenv, dbc_arg->locker, 32U, (DBT const   *)(& dbc_arg->lock_dbt),
                           (enum __anonenum_db_lockmode_t_50 )2, & dbc_arg->mylock);
          if (ret != 0) {
            return (ret);
          }
        }
      }
    }
  }
  cp = dbc_arg->internal;
  if ((unsigned int )cp->opd != (unsigned int )((void *)0)) {
    if (flags == 7U) {
      goto _L___1;
    } else {
      if (flags == 11U) {
        goto _L___1;
      } else {
        if (flags == 18U) {
          goto _L___1;
        } else {
          if (flags == 19U) {
            goto _L___1;
          } else {
            if (flags == 25U) {
              _L___1: 
              if (tmp_rmw) {
                ret = ((*(dbc_arg->c_am_writelock)))(dbc_arg);
                if (ret != 0) {
                  return (ret);
                }
              }
              ret = 0;//__db_c_idup(cp->opd, & opd, 24U);
              if (ret != 0) {
                return (ret);
              }
              ret = ((*(opd->c_am_get)))(opd, key, data, flags, (db_pgno_t *)((void *)0));
              switch (ret) {
              case 0: ;
              goto done;
              case -30990: ;
              if (flags == 18U) {
                goto _L___0;
              } else {
                if (flags == 25U) {
                  _L___0: 
                  ret = __db_c_close(opd);
                  if (ret != 0) {
                    goto err;
                  }
                  opd = (DBC *)((void *)0);
                  break;
                }
              }
              goto err;
              default: ;
              goto err;
              }
            }
          }
        }
      }
    }
  }
  switch ((int )flags) {
  case 7: ;
  case 11: ;
  case 18: ;
  case 19: ;
  case 20: ;
  case 25: ;
  case 26: 
  tmp_flags = 24U;
  break;
  default: 
  tmp_flags = 0U;
  break;
  }
  if (tmp_dirty) {
    dbc_arg->flags = dbc_arg->flags | 4U;
  }
  if (dbc_arg->flags & 64U) {
    dbc_n = dbc_arg;
  } else {
    ret = 0;//__db_c_idup(dbc_arg, & dbc_n, tmp_flags);
    if (tmp_dirty) {
      dbc_arg->flags = dbc_arg->flags & 4294967291U;
    }
    if (ret != 0) {
      goto err;
    }
    while (1) {
      dbc_n->rskey = dbc_arg->rskey;
      dbc_n->rkey = dbc_arg->rkey;
      dbc_n->rdata = dbc_arg->rdata;
      break;
    }
  }
  if (tmp_rmw) {
    dbc_n->flags = dbc_n->flags | 32U;
  }
  switch ((int )multi) {
  case 67108864: 
  dbc_n->flags = dbc_n->flags | 512U;
  break;
  case 134217728: 
  dbc_n->flags = dbc_n->flags | 1024U;
  break;
  case 201326592: 
  dbc_n->flags = dbc_n->flags | 1536U;
  break;
  case 0: ;
  default: ;
  break;
  }
  pgno = 0U;
  ret = ((*(dbc_n->c_am_get)))(dbc_n, key, data, flags, & pgno);
  if (tmp_rmw) {
    dbc_n->flags = dbc_n->flags & 4294967263U;
  }
  if (tmp_dirty) {
    dbc_arg->flags = dbc_arg->flags & 4294967291U;
  }
  dbc_n->flags = dbc_n->flags & 4294965759U;
  if (ret != 0) {
    goto err;
  }
  cp_n = dbc_n->internal;
  if (pgno != 0U) {
    ret = 0;//__db_c_newopd(dbc_arg, pgno, cp_n->opd, & cp_n->opd);
    if (ret != 0) {
      goto err;
    }
    switch ((int )flags) {
    case 9: ;
    case 18: ;
    case 20: ;
    case 28: ;
    case 31: ;
    case 30: 
    tmp_flags = 9U;
    break;
    case 17: ;
    case 25: ;
    case 26: 
    tmp_flags = 17U;
    break;
    case 10: ;
    case 11: ;
    case 12: 
    tmp_flags = flags;
    break;
    default: 
      ret = 0;//__db_unknown_flag(dbp___1->dbenv, (char *)"__db_c_get", flags);
      goto err;
    }
    ret = ((*((cp_n->opd)->c_am_get)))(cp_n->opd, key, data, tmp_flags, (db_pgno_t *)((void *)0));
    if (ret != 0) {
      goto err;
    }
  }
  done: 
  if ((unsigned int )dbc_n == (unsigned int )((void *)0)) {
    cp_n = dbc_arg->internal;
  } else {
    cp_n = dbc_n->internal;
  }
  if (! (key->flags & 2U)) {
    if ((unsigned int )cp_n->page == (unsigned int )((void *)0)) {
      ret = __memp_fget(mpf, & cp_n->pgno, 0U, (void *)(& cp_n->page));
      if (ret != 0) {
        goto err;
      }
    }
    ret = 0;//__db_ret(dbp___1, (PAGE *)cp_n->page, (unsigned int )cp_n->indx, key, & (dbc_arg->rkey)->data, & (dbc_arg->rkey)->ulen);
    if (ret != 0) {
      goto err;
    }
  }
  if (multi != 0U) {
    if ((unsigned int )dbc_n == (unsigned int )((void *)0)) {
      if (! (multi & 134217728U)) {
        if ((unsigned int )(dbc_arg->internal)->opd == (unsigned int )((void *)0)) {
          dbc_n = dbc_arg;
        } else {
          goto _L___2;
        }
      } else {
        _L___2: 
        if (dbc_arg->flags & 64U) {
          dbc_n = dbc_arg;
        } else {
          ret = 0;//__db_c_idup(dbc_arg, & dbc_n, 24U);
          if (ret != 0) {
            goto err;
          }
          ret = ((*(dbc_n->c_am_get)))(dbc_n, key, data, 7U, & pgno);
          if (ret != 0) {
            goto err;
          }
        }
      }
      cp_n = dbc_n->internal;
    }
    if ((unsigned int )opd != (unsigned int )((void *)0)) {
      cp_n->opd = opd;
      opd = (DBC *)((void *)0);
    }
    data->size = data->ulen;
    ret = ((*(dbc_n->c_am_bulk)))(dbc_n, data, flags | multi);
  } else {
    if (! (data->flags & 2U)) {
      if ((unsigned int )opd != (unsigned int )((void *)0)) {
        dbc = opd;
      } else {
        if ((unsigned int )cp_n->opd != (unsigned int )((void *)0)) {
          dbc = cp_n->opd;
        } else {
          dbc = dbc_n;
        }
      }
      type = ((PAGE *)(dbc->internal)->page)->type;
      if ((int )type == 5) {
        tmp___1 = 1;
      } else {
        if ((int )type == 2) {
          tmp___1 = 1;
        } else {
          tmp___1 = 0;
        }
      }
      ret = 0;//__db_ret(dbp___1, (PAGE *)(dbc->internal)->page, (unsigned int )((int )(dbc->internal)->indx + tmp___1),  data, & (dbc_arg->rdata)->data, & (dbc_arg->rdata)->ulen);
    }
  }
  err: 
  key->flags = key->flags & 4294967293U;
  data->flags = data->flags & 4294967293U;
  if ((unsigned int )opd != (unsigned int )((void *)0)) {
    t_ret = 0;//__db_c_cleanup((dbc_arg->internal)->opd, opd, ret);
    if (t_ret != 0) {
      if (ret == 0) {
        ret = t_ret;
      }
    }
  }
t_ret = 0;//__db_c_cleanup(dbc_arg, dbc_n, ret);
  if (t_ret != 0) {
    if (ret == 0) {
      ret = t_ret;
    }
  }
  if (flags == 5U) {
    goto _L___3;
  } else {
    if (flags == 6U) {
      _L___3: 
      if (dbc_arg->flags & 128U) {
        __lock_downgrade(dbp___1->dbenv, & dbc_arg->mylock, (enum __anonenum_db_lockmode_t_50 )4,
                         0U);
      }
    }
  }
  return (ret);
}
}


int __db_close(DB *dbp___1 , DB_TXN *txn , u_int32_t flags ) 
{ DB_ENV *dbenv___0 ;
  u_int32_t dbpflags ;
  int db_ref ;
  int deferred_close ;
  int ret ;
  int t_ret ;

  {
  dbenv___0 = dbp___1->dbenv;
  ret = 0;
  deferred_close = ret;
  if ((unsigned int )txn != (unsigned int )((void *)0)) {
    __db_check_txn(dbp___1, txn, 0U, 0);
  }
  dbpflags = dbp___1->flags;
  ret = 0;//__db_refresh(dbp___1, txn, flags, & deferred_close);
  if (deferred_close) {
    return (ret);
  }
  t_ret = __bam_db_close(dbp___1);
  if (t_ret != 0) {
    if (ret == 0) {
      ret = t_ret;
    }
  }
  t_ret = __ham_db_close(dbp___1);
  if (t_ret != 0) {
    if (ret == 0) {
      ret = t_ret;
    }
  }
  t_ret = __qam_db_close(dbp___1, dbpflags);
  if (t_ret != 0) {
    if (ret == 0) {
      ret = t_ret;
    }
  }
  if ((unsigned int )dbenv___0->dblist_mutexp != (unsigned int )((void *)0)) {
    if (! ((dbenv___0->dblist_mutexp)->flags & 2U)) {
      __db_pthread_mutex_lock(dbenv___0, dbenv___0->dblist_mutexp);
    }
  }
  dbenv___0->db_ref = dbenv___0->db_ref - 1;
  db_ref = dbenv___0->db_ref;
  if ((unsigned int )dbenv___0->dblist_mutexp != (unsigned int )((void *)0)) {
    if (! ((dbenv___0->dblist_mutexp)->flags & 2U)) {
      __db_pthread_mutex_unlock(dbenv___0, dbenv___0->dblist_mutexp);
    }
  }
  if (dbenv___0->flags & 16U) {
    if (db_ref == 0) {
      t_ret = __dbenv_close(dbenv___0, 0);
      if (t_ret != 0) {
        if (ret == 0) {
          ret = t_ret;
        }
      }
    }
  }
  memset((void *)dbp___1, 219, sizeof((*dbp___1)));
  __os_free(dbenv___0, (void *)dbp___1);
  return (ret);
}
}

static int __db_c_del_secondary(DBC *dbc ) 
{ DB *pdbp ;
  DBC *pdbc ;
  DBT skey ;
  DBT pkey ;
  int ret ;
  int t_ret ;
  int tmp ;

  {
  memset((void *)(& skey), 0, sizeof(DBT ));
  memset((void *)(& pkey), 0, sizeof(DBT ));
  skey.flags = skey.flags | 40U;
  ret = __db_c_get(dbc, & skey, & pkey, 7U);
  if (ret != 0) {
    return (ret);
  }
  pdbp = (dbc->dbp)->s_primary;
  ret = __db_cursor_int(pdbp, dbc->txn, pdbp->type, 0U, 0, dbc->locker, & pdbc);
  if (ret != 0) {
    return (ret);
  }
  if ((pdbp->dbenv)->flags & 2U) {
    pdbc->flags = pdbc->flags | 256U;
  }
  if (! (dbc->flags & 8U)) {
    if (! (((dbc->dbp)->dbenv)->flags & 2U)) {
      if ((unsigned int )((dbc->dbp)->dbenv)->lk_handle != (unsigned int )((void *)0)) {
        tmp = 268435456;
      } else {
        tmp = 0;
      }
    } else {
      tmp = 0;
    }
  } else {
    tmp = 0;
  }
  ret = __db_c_get(pdbc, & pkey, & skey, (unsigned int )(tmp | 28));
  if (ret == 0) {
    ret = __db_c_del(pdbc, 0U);
  } else {
    if (ret == -30990) {
      ret = __db_secondary_corrupt(pdbp);
    }
  }
  t_ret = __db_c_close(pdbc);
  if (t_ret != 0) {
    if (ret != 0) {
      ret = t_ret;
    }
  }
  return (ret);
}
}

int __db_c_del_primary(DBC *dbc ) 
{ DB *dbp___1 ;
  DB *sdbp ;
  DBC *sdbc ;
  DBT data ;
  DBT pkey ;
  DBT skey ;
  DBT temppkey ;
  DBT tempskey ;
  int ret ;
  int t_ret ;
  int tmp ;

  {
  dbp___1 = dbc->dbp;
  memset((void *)(& pkey), 0, sizeof(DBT ));
  memset((void *)(& data), 0, sizeof(DBT ));
  ret = __db_c_get(dbc, & pkey, & data, 7U);
  if (ret != 0) {
    return (ret);
  }
  sdbp = __db_s_first(dbp___1);
  while (1) {
    if ((unsigned int )sdbp != (unsigned int )((void *)0)) {
      if (! (ret == 0)) {
        break;
      }
    } else {
      break;
    }
    memset((void *)(& skey), 0, sizeof(DBT ));
    ret = ((*(sdbp->s_callback)))(sdbp, (DBT const   *)(& pkey), (DBT const   *)(& data),
                                  & skey);
    if (ret != 0) {
      if (ret == -30999) {
        goto __Cont;
      }
      if (skey.flags & 1U) {
        __os_ufree(sdbp->dbenv, skey.data);
        skey.flags = skey.flags & 4294967294U;
      }
      goto done;
    }
    ret = __db_cursor_int(sdbp, dbc->txn, sdbp->type, 0U, 0, dbc->locker, & sdbc);
    if (ret != 0) {
      goto done;
    }
    if ((sdbp->dbenv)->flags & 2U) {
      sdbc->flags = sdbc->flags | 256U;
    }
    memset((void *)(& tempskey), 0, sizeof(DBT ));
    tempskey.data = skey.data;
    tempskey.size = skey.size;
    memset((void *)(& temppkey), 0, sizeof(DBT ));
    temppkey.data = pkey.data;
    temppkey.size = pkey.size;
    if (! (dbc->flags & 8U)) {
      if (! (((dbc->dbp)->dbenv)->flags & 2U)) {
        if ((unsigned int )((dbc->dbp)->dbenv)->lk_handle != (unsigned int )((void *)0)) {
          tmp = 268435456;
        } else {
          tmp = 0;
        }
      } else {
        tmp = 0;
      }
    } else {
      tmp = 0;
    }
    ret = __db_c_get(sdbc, & tempskey, & temppkey, (unsigned int )(tmp | 10));
    if (ret == 0) {
      ret = __db_c_del(sdbc, 34U);
    } else {
      if (ret == -30990) {
        ret = __db_secondary_corrupt(dbp___1);
      }
    }
    if (skey.flags & 1U) {
      __os_ufree(sdbp->dbenv, skey.data);
      skey.flags = skey.flags & 4294967294U;
    }
    t_ret = __db_c_close(sdbc);
    if (t_ret != 0) {
      if (ret == 0) {
        ret = t_ret;
      }
    }
    if (ret != 0) {
      goto done;
    }
    __Cont: 
    ret = __db_s_next(& sdbp);
  }
  done: 
  if ((unsigned int )sdbp != (unsigned int )((void *)0)) {
    t_ret = __db_s_done(sdbp);
    if (t_ret != 0) {
      if (ret == 0) {
        ret = t_ret;
      }
    }
  }
  return (ret);
}
}


int __db_c_del(DBC *dbc , u_int32_t flags ) 
{ DB *dbp___1 ;
  DBC *opd ;
  int ret ;
  int tmp ;

  {
  dbp___1 = dbc->dbp;
  if ((dbp___1->dbenv)->flags & 2U) {
    if (! (dbc->flags & 384U)) {
      tmp = __db_wrlock_err(dbp___1->dbenv);
      return (tmp);
    }
    if (dbc->flags & 128U) {
      ret = __lock_get(dbp___1->dbenv, dbc->locker, 32U, (DBT const   *)(& dbc->lock_dbt),
                       (enum __anonenum_db_lockmode_t_50 )2, & dbc->mylock);
      if (ret != 0) {
        return (ret);
      }
    }
  }
  if (flags != 34U) {
    if (dbp___1->flags & 33554432U) {
      ret = __db_c_del_secondary(dbc);
      goto done;
    }
  }
  if ((unsigned int )dbp___1->s_secondaries.lh_first != (unsigned int )((void *)0)) {
    ret = __db_c_del_primary(dbc);
    if (ret != 0) {
      goto done;
    }
  }
  opd = (dbc->internal)->opd;
  if ((unsigned int )opd == (unsigned int )((void *)0)) {
    ret = ((*(dbc->c_am_del)))(dbc);
  } else {
    ret = ((*(dbc->c_am_writelock)))(dbc);
    if (ret == 0) {
      ret = ((*(opd->c_am_del)))(opd);
    }
  }
  done: 
  if (dbc->flags & 128U) {
    __lock_downgrade(dbp___1->dbenv, & dbc->mylock, (enum __anonenum_db_lockmode_t_50 )4,
                     0U);
  }
  return (ret);
}
}




/////////////////////////////////////////////////////////////
///// FROM SYNCLINK


struct tx_holding_buffer {
        int buffer_size;
        unsigned char * buffer;
};


struct mgsl_struct {
        int magic;
        int flags;
        int count;
        int line;
        int hw_version;
        unsigned short close_delay;
        unsigned short closing_wait;

        int timeout;
        int x_char;
        int blocked_open;
        unsigned char *xmit_buf;
        int xmit_head;
        int xmit_tail;
        int xmit_cnt;


        int num_tx_holding_buffers;
        int get_tx_holding_index;
        int put_tx_holding_index;
        int tx_holding_count;
        struct tx_holding_buffer tx_holding_buffers[5];

        int rx_enabled;
        int rx_overflow;
        int rx_rcc_underrun;

        int tx_enabled;
        int tx_active;

        char device_name[25];

        unsigned int bus_type;
        unsigned char bus;
        unsigned char function;

        unsigned int io_base;
        unsigned int io_addr_size;
        int io_addr_requested;

        unsigned int irq_level;
        unsigned long irq_flags;
        int irq_requested;

        unsigned int dma_level;
        int dma_requested;
        unsigned char serial_signals;

        int irq_occurred;
        unsigned int init_error;
        int fDiagnosticsmode;

        unsigned char* memory_base;
        int shared_mem_requested;

        unsigned char* lcr_base;
        int lcr_mem_requested;

        char flag_buf[4096];
        char char_buf[4096];
        int drop_rts_on_tx_done;

        int loopmode_insert_requested;
        int loopmode_send_done_requested;


        int netcount;
        int dosyncppp;

};

static void mgsl_free_intermediate_txbuffer_memory(struct mgsl_struct *info)
{
        int i;

        for ( i=0; i<info->num_tx_holding_buffers; ++i ) {
                if ( info->tx_holding_buffers[i].buffer ) {
                                free(info->tx_holding_buffers[i].buffer);
                                info->tx_holding_buffers[i].buffer=((void *)0);
                }
        }

        info->get_tx_holding_index = 0;
        info->put_tx_holding_index = 0;
        info->tx_holding_count = 0;

}

/////////////////////////////////////////////////////////////
// FROM REISERFS/JOURNAL.C


typedef struct { volatile int counter; } atomic_t;
typedef atomic_t mm_counter_t;
#define atomic_read(v)                ((v)->counter)
#define atomic_set(v,i)               (((v)->counter) = (i))

struct semaphore {
	atomic_t count;
	int sleepers;
  //wait_queue_head_t wait;
};

struct list_head {
	struct list_head *next, *prev;
};


/*
** one of these for each trans. The most important part is the j_realblock.
** this list of cnodes is used to hash all the blocks in all the commits, 
** to mark all the real buffer heads dirty once all the commits hit the disk,
** and to make sure every real block in a transaction is on disk 
** before allowing the log area to be overwritten */
struct reiserfs_journal_list {
	unsigned long j_start;
	unsigned long j_state;
	unsigned long j_len;
	atomic_t j_nonzerolen;
	atomic_t j_commit_left;
	atomic_t j_older_commits_done;	/* all commits older than this on disk */
	struct semaphore j_commit_lock;
	unsigned long j_trans_id;
	time_t j_timestamp;
	struct reiserfs_list_bitmap *j_list_bitmap;
	struct buffer_head *j_commit_bh;	/* commit buffer head */
	struct reiserfs_journal_cnode *j_realblock;
	struct reiserfs_journal_cnode *j_freedlist;	/* list of buffers that were freed during this trans.  free each of these on flush */
	/* time ordered list of all active transactions */
	struct list_head j_list;

	/* time ordered list of all transactions we haven't tried to flush yet */
	struct list_head j_working_list;

	/* list of tail conversion targets in need of flush before commit */
	struct list_head j_tail_bh_list;
	/* list of data=ordered buffers in need of flush before commit */
	struct list_head j_bh_list;
	int j_refcount;
};

struct reiserfs_journal {
	struct buffer_head **j_ap_blocks;	/* journal blocks on disk */
  //struct reiserfs_journal_cnode *j_last;	/* newest journal block */
  //struct reiserfs_journal_cnode *j_first;	/*  oldest journal block.  start here for traverse */

  //struct file *j_dev_file;
  //struct block_device *j_dev_bd;
	int j_1st_reserved_block;	/* first block on s_dev of reserved area journal */

	long j_state;
	unsigned long j_trans_id;
	unsigned long j_mount_id;
	unsigned long j_start;	/* start of current waiting commit (index into j_ap_blocks) */
	unsigned long j_len;	/* lenght of current waiting commit */
	unsigned long j_len_alloc;	/* number of buffers requested by journal_begin() */
	atomic_t j_wcount;	/* count of writers for current commit */
	unsigned long j_bcount;	/* batch count. allows turning X transactions into 1 */
	unsigned long j_first_unflushed_offset;	/* first unflushed transactions offset */
	unsigned long j_last_flush_trans_id;	/* last fully flushed journal timestamp */
  //struct buffer_head *j_header_bh;

	time_t j_trans_start_time;	/* time this transaction started */
	struct semaphore j_lock;
	struct semaphore j_flush_sem;
  //wait_queue_head_t j_join_wait;	/* wait for current transaction to finish before starting new one */
	atomic_t j_jlock;	/* lock for j_join_wait */
	int j_list_bitmap_index;	/* number of next list bitmap to use */
	int j_must_wait;	/* no more journal begins allowed. MUST sleep on j_join_wait */
	int j_next_full_flush;	/* next journal_end will flush all journal list */
	int j_next_async_flush;	/* next journal_end will flush all async commits */

	int j_cnode_used;	/* number of cnodes on the used list */
	int j_cnode_free;	/* number of cnodes on the free list */

	unsigned int j_trans_max;	/* max number of blocks in a transaction.  */
	unsigned int j_max_batch;	/* max number of blocks to batch into a trans */
	unsigned int j_max_commit_age;	/* in seconds, how old can an async commit be */
	unsigned int j_max_trans_age;	/* in seconds, how old can a transaction be */
	unsigned int j_default_max_commit_age;	/* the default for the max commit age */

  //struct reiserfs_journal_cnode *j_cnode_free_list;
  //struct reiserfs_journal_cnode *j_cnode_free_orig;	/* orig pointer returned from vmalloc */

    struct reiserfs_journal_list *j_current_jl;
	int j_free_bitmap_nodes;
	int j_used_bitmap_nodes;

	int j_num_lists;	/* total number of active transactions */
	int j_num_work_lists;	/* number that need attention from kreiserfsd */

	/* debugging to make sure things are flushed in order */
	int j_last_flush_id;

	/* debugging to make sure things are committed in order */
	int j_last_commit_id;

	struct list_head j_bitmap_nodes;
	struct list_head j_dirty_buffers;
    //spinlock_t j_dirty_buffers_lock;	/* protects j_dirty_buffers */

	/* list of all active transactions */
	struct list_head j_journal_list;
	/* lists that haven't been touched by writeback attempts */
	struct list_head j_working_list;

  //struct reiserfs_list_bitmap j_list_bitmap[JOURNAL_NUM_BITMAPS];	/* array of bitmaps to record the deleted blocks */
  //struct reiserfs_journal_cnode *j_hash_table[JOURNAL_HASH_SIZE];	/* hash table for real buffer heads in current trans */
  //struct reiserfs_journal_cnode *j_list_hash_table[JOURNAL_HASH_SIZE];	/* hash table for all the real buffer heads in all the transactions */
	struct list_head j_prealloc_list;	/* list of inodes which have preallocated blocks */
	int j_persistent_trans;
	unsigned long j_max_trans_size;
	unsigned long j_max_batch_size;

	int j_errno;

	/* when flushing ordered buffers, throttle new ordered writers */
	//struct work_struct j_work;
	atomic_t j_async_throttle;
};



#define container_of(ptr, type, member) ({ const typeof( ((type *)0)->member ) *__mptr = (ptr); (type *)( (char *)__mptr - offsetof(type,member) );})
  
#define list_entry(ptr, type, member) \
	container_of(ptr, type, member)

#define JOURNAL_LIST_ENTRY(h) (list_entry((h), struct reiserfs_journal_list, j_list))



static int flush_older_commits(struct super_block *s,
			       struct reiserfs_journal_list *jl)
{
  struct reiserfs_journal_list *other_jl;
  struct reiserfs_journal_list *first_jl;
  struct list_head *entry;
  unsigned long trans_id = jl->j_trans_id;
  unsigned long other_trans_id;
  unsigned long first_trans_id;

 find_first:
	/*
	 * first we walk backwards to find the oldest uncommitted transation
	 */
	first_jl = jl;
	entry = jl->j_list.prev;
	while (1) {
      other_jl = JOURNAL_LIST_ENTRY(entry);
      if (//entry == &journal->j_journal_list ||
		    atomic_read(&other_jl->j_older_commits_done))
			break;

		first_jl = other_jl;
		entry = other_jl->j_list.prev;
	}

	/* if we didn't find any older uncommitted transactions, return now */
	if (first_jl == jl) {
		return 0;
	}

	first_trans_id = first_jl->j_trans_id;

	entry = &first_jl->j_list;
	while (1) {
		other_jl = JOURNAL_LIST_ENTRY(entry);
		other_trans_id = other_jl->j_trans_id;

		if (other_trans_id < trans_id) {
			if (atomic_read(&other_jl->j_commit_left) != 0) {
              //flush_commit_list(s, other_jl, 0);

				/* list we were called with is gone, return */
				//if (!journal_list_still_alive(s, trans_id))
				//	return 1;

				/* the one we just flushed is gone, this means all
				 * older lists are also gone, so first_jl is no longer
				 * valid either.  Go back to the beginning.
				 */
				//if (!journal_list_still_alive
                // (s, other_trans_id)) {
				//	goto find_first;
				//}
			}
			entry = entry->next;
			if (1)//entry == &journal->j_journal_list)
				return 0;
		} else {
			return 0;
		}
	}
	return 0;
}
/*
** if this journal list still has commit blocks unflushed, send them to disk.
**
** log areas must be flushed in order (transaction 2 can't commit before transaction 1)
** Before the commit block can by written, every other log block must be safely on disk
**
*/
static int flush_commit_list(struct super_block *s,
			     struct reiserfs_journal_list *jl, int flushall)
{
	int i;
	int bn;
	struct buffer_head *tbh = NULL;
	unsigned long trans_id = jl->j_trans_id;
	struct reiserfs_journal *journal = SB_JOURNAL(s);
	int barrier = 0;
	int retval = 0;

	reiserfs_check_lock_depth(s, "flush_commit_list");

	if (atomic_read(&jl->j_older_commits_done)) {
		return 0;
	}

	get_fs_excl();

	/* before we can put our commit blocks on disk, we have to make sure everyone older than
	 ** us is on disk too
	 */
	BUG_ON(jl->j_len <= 0);
	BUG_ON(trans_id == journal->j_trans_id);

	get_journal_list(jl);
	if (flushall) {
		if (flush_older_commits(s, jl) == 1) {
			/* list disappeared during flush_older_commits.  return */
			goto put_jl;
		}
	}

	/* make sure nobody is trying to flush this one at the same time */
	down(&jl->j_commit_lock);
	if (!journal_list_still_alive(s, trans_id)) {
		up(&jl->j_commit_lock);
		goto put_jl;
	}
	BUG_ON(jl->j_trans_id == 0);

	/* this commit is done, exit */
	if (atomic_read(&(jl->j_commit_left)) <= 0) {
		if (flushall) {
			atomic_set(&(jl->j_older_commits_done), 1);
		}
		up(&jl->j_commit_lock);
		goto put_jl;
	}

	if (!list_empty(&jl->j_bh_list)) {
		unlock_kernel();
		//write_ordered_buffers(&journal->j_dirty_buffers_lock,
		//		      journal, jl, &jl->j_bh_list);
		lock_kernel();
	}
	BUG_ON(!list_empty(&jl->j_bh_list));
	/*
	 * for the description block and all the log blocks, submit any buffers
	 * that haven't already reached the disk
	 */
	atomic_inc(&journal->j_async_throttle);
	for (i = 0; i < (jl->j_len + 1); i++) {
		bn = SB_ONDISK_JOURNAL_1st_BLOCK(s) + (jl->j_start + i) %
		    SB_ONDISK_JOURNAL_SIZE(s);
		tbh = journal_find_get_block(s, bn);
		if (buffer_dirty(tbh))	/* redundant, ll_rw_block() checks */
          ;//ll_rw_block(SWRITE, 1, &tbh);
		put_bh(tbh);
	}
	atomic_dec(&journal->j_async_throttle);

	/* We're skipping the commit if there's an error */
	if (retval || reiserfs_is_journal_aborted(journal))
		barrier = 0;

	/* wait on everything written so far before writing the commit
	 * if we are in barrier mode, send the commit down now
	 */
	barrier = reiserfs_barrier_flush(s);
	if (barrier) {
		int ret;
		lock_buffer(jl->j_commit_bh);
		ret = submit_barrier_buffer(jl->j_commit_bh);
		//if (ret == -EOPNOTSUPP) {
			set_buffer_uptodate(jl->j_commit_bh);
			disable_barrier(s);
			barrier = 0;
            //}
	}
	for (i = 0; i < (jl->j_len + 1); i++) {
		bn = SB_ONDISK_JOURNAL_1st_BLOCK(s) +
		    (jl->j_start + i) % SB_ONDISK_JOURNAL_SIZE(s);
		tbh = journal_find_get_block(s, bn);
		wait_on_buffer(tbh);
		// since we're using ll_rw_blk above, it might have skipped over
		// a locked buffer.  Double check here
		//
		if (buffer_dirty(tbh))	/* redundant, sync_dirty_buffer() checks */
			sync_dirty_buffer(tbh);
		if (unlikely(!buffer_uptodate(tbh))) {
#ifdef CONFIG_REISERFS_CHECK
			reiserfs_warning(s, "journal-601, buffer write failed");
#endif
			retval = -1;//-EIO;
		}
		put_bh(tbh);	/* once for journal_find_get_block */
		put_bh(tbh);	/* once due to original getblk in do_journal_end */
		atomic_dec(&(jl->j_commit_left));
	}

	BUG_ON(atomic_read(&(jl->j_commit_left)) != 1);

	if (!barrier) {
		/* If there was a write error in the journal - we can't commit
		 * this transaction - it will be invalid and, if successful,
		 * will just end up propogating the write error out to
		 * the file system. */
		if (likely(!retval && !reiserfs_is_journal_aborted (journal))) {
			if (buffer_dirty(jl->j_commit_bh))
				BUG();
			mark_buffer_dirty(jl->j_commit_bh) ;
			sync_dirty_buffer(jl->j_commit_bh) ;
		}
	} else
		wait_on_buffer(jl->j_commit_bh);

	check_barrier_completion(s, jl->j_commit_bh);

	/* If there was a write error in the journal - we can't commit this
	 * transaction - it will be invalid and, if successful, will just end
	 * up propogating the write error out to the filesystem. */
	if (unlikely(!buffer_uptodate(jl->j_commit_bh))) {
#ifdef CONFIG_REISERFS_CHECK
		reiserfs_warning(s, "journal-615: buffer write failed");
#endif
		retval = -1;//-EIO;
	}
	bforget(jl->j_commit_bh);
	if (journal->j_last_commit_id != 0 &&
	    (jl->j_trans_id - journal->j_last_commit_id) != 1) {
		reiserfs_warning(s, "clm-2200: last commit %lu, current %lu",
				 journal->j_last_commit_id, jl->j_trans_id);
	}
	journal->j_last_commit_id = jl->j_trans_id;

	/* now, every commit block is on the disk.  It is safe to allow blocks freed during this transaction to be reallocated */
	cleanup_freed_for_journal_list(s, jl);

	retval = retval ? retval : journal->j_errno;

	/* mark the metadata dirty */
	if (!retval)
		dirty_one_transaction(s, jl);
	atomic_dec(&(jl->j_commit_left));

	if (flushall) {
		atomic_set(&(jl->j_older_commits_done), 1);
	}
	up(&jl->j_commit_lock);
      put_jl:
	put_journal_list(s, jl);

	if (retval)
		reiserfs_abort(s, retval, "Journal write error in %s",
			       __FUNCTION__);
	put_fs_excl();
	return retval;
}



/////////////////////////////////////////////////////////////

int ptrMain (int argc, char *argv[]) {

  UDWORD dw;
  unsigned char ch;
  UBYTE *addr;
  hashThing *ht;
  struct mgsl_struct mgsl_s;
  int i;

  addr = (UBYTE*) malloc (sizeof(UBYTE) * 16);

  InitCRC();
  dw = CalcCRC32(0UL, addr, 4UL);

  ChannelDelta = 0;
  CurChannel = 0;
  
  ch = DecodeAudio (44);

  ht = (hashThing *)malloc(sizeof(hashThing));
  ht->kids.fst = (void *)malloc(sizeof(hashThing));
  ht->kids.snd = (void *)malloc(sizeof(hashThing));
  assignFstHash (ht);


  updateHash(ht);
  updateHashVoid((void *) ht);

  i = __db_pg_free_recover_int((DB_ENV *)0, 
                           (__db_pg_freedata_args *) 0,
                           (DB *)0 , (DB_LSN *) 0 , 
                           (DB_MPOOLFILE *)0 ,
                           DB_TXN_ABORT, 10); 

  i = __db_pg_alloc_recover((DB_ENV *)0,
                            (DBT *)0 , (DB_LSN *) 0, DB_TXN_ABORT,
                            (void *) 0 );


  mgsl_free_intermediate_txbuffer_memory (&mgsl_s);

  i =  __db_c_del((DBC *)0, (u_int32_t) 0 ); 
  i =  __db_c_close((DBC*)0);

  i = flush_older_commits((struct super_block *)0,
                          (struct reiserfs_journal_list *)0);

  return 0;
} 
