
#ifndef _SPINLOCK_TYPES_UP___FOO_BAR_
#define _SPINLOCK_TYPES_UP___FOO_BAR_

typedef struct { int gcc_is_buggy; } raw_rwlock_t;
#define __RAW_RW_LOCK_UNLOCKED (raw_rwlock_t) { 0 }

#endif
