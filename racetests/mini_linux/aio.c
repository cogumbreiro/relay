
#include "spinlock_types.h"
#include "aio.h"
#include "bug.h"
#include <errno.h>
#include "slab.h"
#include "gfp.h"

//from include/asm-i386/linkage.h

#define fastcall     __attribute__((regparm(3)))

/*------ sysctl variables----*/
static DEFINE_SPINLOCK(aio_nr_lock);
unsigned long aio_nr;		/* current system wide number of aio requests */
unsigned long aio_max_nr = 0x10000; /* system wide maximum number of aio requests */

static kmem_cache_t	*kioctx_cachep;




/* __put_ioctx
 *	Called when the last user of an aio context has gone away,
 *	and the struct needs to be freed.
 */
void fastcall __put_ioctx(struct kioctx *ctx)
{
	unsigned nr_events = ctx->max_reqs;
    int tmp_0;

	if (unlikely(ctx->reqs_active))
		BUG();

    //	cancel_delayed_work(&ctx->wq);
    //	flush_workqueue(aio_wq);
    //	aio_free_ring(ctx);
    //	mmdrop(ctx->mm);
    //	ctx->mm = NULL;
    //	pr_debug("__put_ioctx: freeing %p\n", ctx);
    //	kmem_cache_free(kioctx_cachep, ctx);

	if (nr_events) {
		spin_lock(&aio_nr_lock);
        {
          {
            while(1) {
            while_1801_continue:
            while_1613_continue:
              tmp_0 = __builtin_expect((long)(! (! ((aio_nr - (unsigned long)nr_events > aio_nr) != 0))), 0L);
              if (tmp_0) {
                ;
              }
              goto while_1613_break;
            }
          while_1801_break: ;
          }
        while_1613_break: ;
        }
        //          BUG_ON(aio_nr - nr_events > aio_nr);
		aio_nr -= nr_events;
		spin_unlock(&aio_nr_lock);
	}
}

/* ioctx_alloc
 *	Allocates and initializes an ioctx.  Returns an ERR_PTR if it failed.
 */
struct kioctx *ioctx_alloc(unsigned nr_events)
{
	struct mm_struct *mm;
	struct kioctx *ctx;
    size_t s;

	/* Prevent overflows */
	if ((nr_events > (0x10000000U / sizeof(struct io_event))) ||
	    (nr_events > (0x10000000U / sizeof(struct kiocb)))) {
		pr_debug("ENOMEM: nr_events too high\n");
		return (void *)-EINVAL; //ERR_PTR(-EINVAL);
	}
    
	if ((unsigned long)nr_events > aio_max_nr)
      return (void *)-EAGAIN; //ERR_PTR(-EAGAIN);

	ctx = kmem_cache_alloc(kioctx_cachep, 0);//GFP_KERNEL);
	if (!ctx)
		return ERR_PTR(-ENOMEM);

    s = sizeof(*ctx);
	memset(ctx, 0, s);
	ctx->max_reqs = nr_events;
	//mm = ctx->mm = current->mm;
	//atomic_inc(&mm->mm_count);

	atomic_set(&ctx->users, 1);
	spin_lock_init(&ctx->ctx_lock);
	spin_lock_init(&ctx->ring_info.ring_lock);
    //	init_waitqueue_head(&ctx->wait);

	INIT_LIST_HEAD(&ctx->active_reqs);
	INIT_LIST_HEAD(&ctx->run_list);
	//INIT_WORK(&ctx->wq, aio_kick_handler, ctx);

	//if (aio_setup_ring(ctx) < 0)
	//	goto out_freectx;

	/* limit the number of system wide aios */
	spin_lock(&aio_nr_lock);
	if (aio_nr + ctx->max_reqs > aio_max_nr ||
	    aio_nr + ctx->max_reqs < aio_nr)
		ctx->max_reqs = 0;
	else
		aio_nr += ctx->max_reqs;
	spin_unlock(&aio_nr_lock);
	if (ctx->max_reqs == 0)
		goto out_cleanup;

	/* now link into global list.  kludge.  FIXME */
	//write_lock(&mm->ioctx_list_lock);
	//ctx->next = mm->ioctx_list;
	//mm->ioctx_list = ctx;
	//write_unlock(&mm->ioctx_list_lock);

	//dprintk("aio: allocated ioctx %p[%ld]: mm=%p mask=0x%x\n",
	//	ctx, ctx->user_id, current->mm, ctx->ring_info.nr);
	return ctx;

out_cleanup:
	__put_ioctx(ctx);
	return (void *)-EAGAIN;//ERR_PTR(-EAGAIN);

out_freectx:
	//mmdrop(mm);
	//kmem_cache_free(kioctx_cachep, ctx);
	ctx = (void *)-ENOMEM;//ERR_PTR(-ENOMEM);

    //	dprintk("aio: error allocating ioctx %p\n", ctx);
	return ctx;
}
