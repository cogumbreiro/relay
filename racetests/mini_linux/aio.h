#ifndef _AIO___FOO_BAR_
#define _AIO___FOO_BAR_

#include "types.h"
#include "list.h"
#include "spinlock_types.h"
#include "compiler.h"
#include "atomic.h"


struct io_event {
        __u64           data;           /* the data field from the iocb */
        __u64           obj;            /* what iocb this event came from */
        __s64           res;            /* result code for this event */
        __s64           res2;           /* secondary result */
};


struct kiotcx;

struct kiocb {
	struct list_head	ki_run_list;
	long			ki_flags;
	int			ki_users;
	unsigned		ki_key;		/* id of this request */

  //	struct file		*ki_filp;
	struct kioctx		*ki_ctx;	/* may be NULL for sync ops */
	int			(*ki_cancel)(struct kiocb *, struct io_event *);
	ssize_t			(*ki_retry)(struct kiocb *);
	void			(*ki_dtor)(struct kiocb *);

	struct list_head	ki_list;	/* the aio core uses this
						 * for cancellation */

  //	union {
  //		void __user		*user;
  //		struct task_struct	*tsk;
  //	} ki_obj;
	__u64			ki_user_data;	/* user's data for completion */
	loff_t			ki_pos;
	/* State that we remember to be able to restart/retry  */
	unsigned short		ki_opcode;
	size_t			ki_nbytes; 	/* copy of iocb->aio_nbytes */
	char 			__user *ki_buf;	/* remaining iocb->aio_buf */
	size_t			ki_left; 	/* remaining bytes */
  //	wait_queue_t		ki_wait;
	long			ki_retried; 	/* just for testing */
	long			ki_kicked; 	/* just for testing */
	long			ki_queued; 	/* just for testing */

	void			*private;
};



#define AIO_RING_PAGES	8
struct aio_ring_info {
	unsigned long		mmap_base;
	unsigned long		mmap_size;

  //	struct page		**ring_pages;
	spinlock_t		ring_lock;
	long			nr_pages;

	unsigned		nr, tail;

  //	struct page		*internal_pages[AIO_RING_PAGES];
};


struct kioctx {
	atomic_t		users;
	int			dead;
  //	struct mm_struct	*mm;

	/* This needs improving */
	unsigned long		user_id;
	struct kioctx		*next;

  //	wait_queue_head_t	wait;

	spinlock_t		ctx_lock;

	int			reqs_active;
	struct list_head	active_reqs;	/* used for cancellation */
	struct list_head	run_list;	/* used for kicked reqs */

	/* sys_io_setup currently limits this to an unsigned int */
	unsigned		max_reqs;

	struct aio_ring_info	ring_info;

  //	struct work_struct	wq;
};


struct kioctx *ioctx_alloc(unsigned nr_events);


#endif
