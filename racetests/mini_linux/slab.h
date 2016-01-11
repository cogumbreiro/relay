
#ifndef _LINUX_SLAB_H
#define	_LINUX_SLAB_H

#include "gfp.h"

/*
 * kmem_cache_t
 *
 * manages a cache.
 */

typedef struct kmem_cache kmem_cache_t;

struct kmem_cache {
/* 1) per-cpu data, touched during every alloc/free */
//	struct array_cache	*array[NR_CPUS];
	unsigned int		batchcount;
	unsigned int		limit;
	unsigned int 		shared;
	unsigned int		objsize;
/* 2) touched by every alloc & free from the backend */
//	struct kmem_list3	*nodelists[MAX_NUMNODES];
	unsigned int	 	flags;	/* constant flags */
	unsigned int		num;	/* # of objs per slab */
	spinlock_t		spinlock;

/* 3) cache_grow/shrink */
	/* order of pgs per slab (2^n) */
	unsigned int		gfporder;

	/* force GFP flags, e.g. GFP_DMA */
	gfp_t			gfpflags;

	size_t			colour;		/* cache colouring range */
	unsigned int		colour_off;	/* colour offset */
	unsigned int		colour_next;	/* cache colouring */
	kmem_cache_t		*slabp_cache;
	unsigned int		slab_size;
	unsigned int		dflags;		/* dynamic flags */

	/* constructor func */
	void (*ctor)(void *, kmem_cache_t *, unsigned long);

	/* de-constructor func */
	void (*dtor)(void *, kmem_cache_t *, unsigned long);

/* 4) cache creation/removal */
	const char		*name;
	struct list_head	next;

/* 5) statistics */

	unsigned long		num_active;
	unsigned long		num_allocations;
	unsigned long		high_mark;
	unsigned long		grown;
	unsigned long		reaped;
	unsigned long 		errors;
	unsigned long		max_freeable;
	unsigned long		node_allocs;
	unsigned long		node_frees;
	atomic_t		allochit;
	atomic_t		allocmiss;
	atomic_t		freehit;
	atomic_t		freemiss;

	int			dbghead;
	int			reallen;

};


/* flags for kmem_cache_alloc() */
#define	SLAB_NOFS		GFP_NOFS
#define	SLAB_NOIO		GFP_NOIO
#define	SLAB_ATOMIC		GFP_ATOMIC
#define	SLAB_USER		GFP_USER
#define	SLAB_KERNEL		GFP_KERNEL
#define	SLAB_DMA		GFP_DMA

extern void *kmem_cache_alloc(kmem_cache_t *, gfp_t);


#endif
