#include "types.h"

#include "spinlock_types.h"
#include "list.h"
#include "semaphore.h"



// from include/linux/fs.h


struct address_space_operations {
	int (*writepage)(struct page *page, struct writeback_control *wbc);
	int (*readpage)(struct file *, struct page *);
	int (*sync_page)(struct page *);

	/* Write back some dirty pages from this mapping. */
	int (*writepages)(struct address_space *, struct writeback_control *);

	/* Set a page dirty */
	int (*set_page_dirty)(struct page *page);

	int (*readpages)(struct file *filp, struct address_space *mapping,
			struct list_head *pages, unsigned nr_pages);

	/*
	 * ext3 requires that a successful prepare_write() call be followed
	 * by a commit_write() call - they must be balanced
	 */
	int (*prepare_write)(struct file *, struct page *, unsigned, unsigned);
	int (*commit_write)(struct file *, struct page *, unsigned, unsigned);
	/* Unfortunately this kludge is needed for FIBMAP. Don't use it */
  //sector_t (*bmap)(struct address_space *, sector_t);
	int (*invalidatepage) (struct page *, unsigned long);
  //int (*releasepage) (struct page *, gfp_t);
  //ssize_t (*direct_IO)(int, struct kiocb *, const struct iovec *iov,
  //		loff_t offset, unsigned long nr_segs);
  //	struct page* (*get_xip_page)(struct address_space *, sector_t,
  //			int);
};

struct address_space {
	struct inode		*host;		/* owner: inode, block_device */
  //struct radix_tree_root	page_tree;	/* radix tree of all pages */
  //rwlock_t		tree_lock;	/* and rwlock protecting it */
	unsigned int		i_mmap_writable;/* count VM_SHARED mappings */
  //struct prio_tree_root	i_mmap;		/* tree of private and shared mappings */
	struct list_head	i_mmap_nonlinear;/*list VM_NONLINEAR mappings */
    spinlock_t		i_mmap_lock;	/* protect tree, count, list */
	unsigned int		truncate_count;	/* Cover race condition with truncate */
	unsigned long		nrpages;	/* number of total pages */
  //pgoff_t			writeback_index;/* writeback starts here */
	struct address_space_operations *a_ops;	/* methods */
	unsigned long		flags;		/* error bits/gfp mask */
	struct backing_dev_info *backing_dev_info; /* device readahead, etc */
    spinlock_t		private_lock;	/* for use by the address_space */
	struct list_head	private_list;	/* ditto */
	struct address_space	*assoc_mapping;	/* ditto */
} __attribute__((aligned(sizeof(long))));


struct super_block;
#define sb_entry(list)	list_entry((list), struct super_block, s_list)
#define S_BIAS (1<<30)
struct super_block {
	struct list_head	s_list;		/* Keep this first */
	dev_t			s_dev;		/* search index; _not_ kdev_t */
	unsigned long		s_blocksize;
	unsigned long		s_old_blocksize;
	unsigned char		s_blocksize_bits;
	unsigned char		s_dirt;
	unsigned long long	s_maxbytes;	/* Max file size */
  //struct file_system_type	*s_type;
  //struct super_operations	*s_op;
  //struct dquot_operations	*dq_op;
  //struct quotactl_ops	*s_qcop;
  //struct export_operations *s_export_op;
	unsigned long		s_flags;
	unsigned long		s_magic;
  //struct dentry		*s_root;
  //struct rw_semaphore	s_umount;
  //struct semaphore	s_lock;
	int			s_count;
	int			s_syncing;
	int			s_need_sync_fs;
	atomic_t		s_active;
	void                    *s_security;
  //struct xattr_handler	**s_xattr;

	struct list_head	s_inodes;	/* all inodes */
	struct list_head	s_dirty;	/* dirty inodes */
	struct list_head	s_io;		/* parked for writeback */
	struct hlist_head	s_anon;		/* anonymous dentries for (nfs) exporting */
	struct list_head	s_files;

	struct block_device	*s_bdev;
	struct list_head	s_instances;
  //struct quota_info	s_dquot;	/* Diskquota specific options */

	int			s_frozen;
  //wait_queue_head_t	s_wait_unfrozen;

	char s_id[32];				/* Informational name */

	void 			*s_fs_info;	/* Filesystem private info */

	/*
	 * The next field is for VFS *only*. No filesystems have any business
	 * even looking at it. You had been warned.
	 */
  //struct semaphore s_vfs_rename_sem;	/* Kludge */

	/* Granuality of c/m/atime in ns.
	   Cannot be worse than a second */
	//u32		   s_time_gran;
};


struct inode {
	struct hlist_node	i_hash;
	struct list_head	i_list;
	struct list_head	i_sb_list;
	struct list_head	i_dentry;
	unsigned long		i_ino;
	atomic_t		i_count;
	umode_t			i_mode;
	unsigned int		i_nlink;
	uid_t			i_uid;
	gid_t			i_gid;
	dev_t			i_rdev;
  //	loff_t			i_size;
  //	struct timespec		i_atime;
  //	struct timespec		i_mtime;
  //	struct timespec		i_ctime;
	unsigned int		i_blkbits;
	unsigned long		i_blksize;
	unsigned long		i_version;
	unsigned long		i_blocks;
	unsigned short          i_bytes;
  	spinlock_t		i_lock;	/* i_blocks, i_bytes, maybe i_size */
	struct semaphore	i_sem;
  //	struct rw_semaphore	i_alloc_sem;
	struct inode_operations	*i_op;
	struct file_operations	*i_fop;	/* former ->i_op->default_file_ops */
	struct super_block	*i_sb;
	struct file_lock	*i_flock;
	struct address_space	*i_mapping;
	struct address_space	i_data;
  //#ifdef CONFIG_QUOTA
  //	struct dquot		*i_dquot[MAXQUOTAS];
  //#endif
	/* These three should probably be a union */
	struct list_head	i_devices;
	struct pipe_inode_info	*i_pipe;
	struct block_device	*i_bdev;
	struct cdev		*i_cdev;
	int			i_cindex;

	__u32			i_generation;

  //#ifdef CONFIG_DNOTIFY
  //	unsigned long		i_dnotify_mask; /* Directory notify events */
  //	struct dnotify_struct	*i_dnotify; /* for directory notifications */
  //#endif

    //#ifdef CONFIG_INOTIFY
  //	struct list_head	inotify_watches; /* watches on this inode */
  //	struct semaphore	inotify_sem;	/* protects the watches list */
  //#endif

	unsigned long		i_state;
	unsigned long		dirtied_when;	/* jiffies of first dirtying */

	unsigned int		i_flags;

	atomic_t		i_writecount;
	void			*i_security;
	union {
		void		*generic_ip;
	} u;
  //#ifdef __NEED_I_SIZE_ORDERED
  //	seqcount_t		i_size_seqcount;
  //#endif
};


// from include/linux/fs.h
struct block_device {
	dev_t			bd_dev;  /* not a kdev_t - it's a search key */
	struct inode *		bd_inode;	/* will die */
	int			bd_openers;
	struct semaphore	bd_sem;	/* open/close mutex */
	struct semaphore	bd_mount_sem;	/* mount mutex */
	struct list_head	bd_inodes;
	void *			bd_holder;
	int			bd_holders;
	struct block_device *	bd_contains;
	unsigned		bd_block_size;
	struct hd_struct *	bd_part;
	/* number of times partitions within this device have been opened. */
	unsigned		bd_part_count;
	int			bd_invalidated;
	struct gendisk *	bd_disk;
	struct list_head	bd_list;
	struct backing_dev_info *bd_inode_backing_dev_info;
	/*
	 * Private data.  You must have bd_claim'ed the block_device
	 * to use this.  NOTE:  bd_claim allows an owner to claim
	 * the same device multiple times, the owner must take special
	 * care to not mess up bd_private for that case.
	 */
	unsigned long		bd_private;
};

struct bdev_inode {
	struct block_device bdev;
	struct inode vfs_inode;
};

static inline struct bdev_inode *BDEV_I(struct inode *inode)
{
  return container_of(inode, struct bdev_inode, vfs_inode);
}



//from include/linux/autoconf.h
#define CONFIG_X86_L1_CACHE_SHIFT 7

//from include/asm/cache.h
#define L1_CACHE_SHIFT	(CONFIG_X86_L1_CACHE_SHIFT)
#define L1_CACHE_BYTES	(1 << L1_CACHE_SHIFT)

#define L1_CACHE_SHIFT_MAX 7	/* largest L1 which this arch supports */


//from include/linux/hash.h
#define GOLDEN_RATIO_PRIME 0x9e370001UL


//from fs/inode.c
#define I_HASHBITS	i_hash_shift
#define I_HASHMASK	i_hash_mask
/* Inode state bits.  Protected by inode_lock. */
#define I_DIRTY_SYNC		1 /* Not dirty enough for O_DATASYNC */
#define I_DIRTY_DATASYNC	2 /* Data-related inode changes pending */
#define I_DIRTY_PAGES		4 /* Data-related inode changes pending */
#define __I_LOCK		3
#define I_LOCK			(1 << __I_LOCK)
#define I_FREEING		16
#define I_CLEAR			32
#define I_NEW			64
#define I_WILL_FREE		128

static unsigned int i_hash_mask;
static unsigned int i_hash_shift;

static struct hlist_head *inode_hashtable;
static inline unsigned long hash(struct super_block *sb, unsigned long hashval)
{
	unsigned long tmp;

	tmp = (hashval * (unsigned long)sb) ^ (GOLDEN_RATIO_PRIME + hashval) /
			L1_CACHE_BYTES;
	tmp = tmp ^ ((tmp ^ GOLDEN_RATIO_PRIME) >> I_HASHBITS);
	return tmp & I_HASHMASK;
}

struct inode *iget5_locked(struct super_block *sb, unsigned long hashval,
		int (*test)(struct inode *, void *),
		int (*set)(struct inode *, void *), void *data)
{
	struct hlist_head *head = inode_hashtable + hash(sb, hashval);
	struct inode *inode;

	inode = ifind(sb, head, test, data, 1);
	if (inode)
		return inode;
	/*
	 * get_new_inode() will do the right thing, re-trying the search
	 * in case it had to block at any point.
	 */
	return get_new_inode(sb, head, test, set, data);
}

//from include/linux/mount.h
struct vfsmount {
	struct list_head mnt_hash;
	struct vfsmount *mnt_parent;	/* fs we are mounted on */
  //struct dentry *mnt_mountpoint;	/* dentry of mountpoint */
  //	struct dentry *mnt_root;	/* root of the mounted tree */
	struct super_block *mnt_sb;	/* pointer to superblock */
	struct list_head mnt_mounts;	/* list of children, anchored here */
	struct list_head mnt_child;	/* and going through their mnt_child */
	atomic_t mnt_count;
	int mnt_flags;
	int mnt_expiry_mark;		/* true if marked for expiry */
	char *mnt_devname;		/* Name of device e.g. /dev/dsk/hda1 */
	struct list_head mnt_list;
	struct list_head mnt_expire;	/* link in fs-specific expiry list */
	struct list_head mnt_share;	/* circular list of shared mounts */
	struct list_head mnt_slave_list;/* list of slave mounts */
	struct list_head mnt_slave;	/* slave list entry */
	struct vfsmount *mnt_master;	/* slave is on master->mnt_slave_list */
  //struct namespace *mnt_namespace; /* containing namespace */
	int mnt_pinned;
};

//from include/linux/stat.h
#define S_IFBLK  0060000


//from include/linux/gfp.h
#define __GFP_WAIT	(0x10u)	/* Can wait and reschedule? */
#define __GFP_HIGH	(0x20u)	/* Should access emergency pools? */
#define __GFP_IO	(0x40u)	/* Can start physical IO? */
#define __GFP_FS	(0x80u)	/* Can call down to low-level FS? */
#define __GFP_COLD	(0x100u)	/* Cache-cold page required */
#define __GFP_NOWARN	(0x200u)	/* Suppress page allocation failure warning */
#define __GFP_REPEAT	(0x400u)	/* Retry the allocation.  Might fail */
#define __GFP_NOFAIL	(0x800u)	/* Retry for ever.  Cannot fail */
#define __GFP_NORETRY	(0x1000u)/* Do not retry.  Might fail */
#define __GFP_NO_GROW	(0x2000u)/* Slab internal usage */
#define __GFP_COMP	(0x4000u)/* Add compound page metadata */
#define __GFP_ZERO	(0x8000u)/* Return zeroed page on success */
#define __GFP_NOMEMALLOC (0x10000u) /* Don't use emergency reserves */
#define __GFP_HARDWALL   (0x20000u) /* Enforce hardwall cpuset memory allocs */

#define GFP_USER	(__GFP_WAIT | __GFP_IO | __GFP_FS | __GFP_HARDWALL)


//from include/linux/backing-dev.h
struct backing_dev_info {
	unsigned long ra_pages;	/* max readahead in PAGE_CACHE_SIZE units */
	unsigned long state;	/* Always use atomic bitops on this */
	unsigned int capabilities; /* Device capabilities */
  //	congested_fn *congested_fn; /* Function pointer if device is md/dm */
	void *congested_data;	/* Pointer to aux data for congested func */
	void (*unplug_io_fn)(struct backing_dev_info *, struct page *);
	void *unplug_io_data;
};
#define BDI_CAP_MAP_COPY	0x00000004	/* Copy can be mapped (MAP_PRIVATE) */

//from include/asm/page.h
#define PAGE_SHIFT	12
#define PAGE_SIZE	(1UL << PAGE_SHIFT)



//from include/linux/pagemap.h
#define PAGE_CACHE_SIZE		PAGE_SIZE



//from mm/readahead.c
#define VM_MAX_READAHEAD	128	/* kbytes */
#define VM_MIN_READAHEAD	16	/* kbytes (includes current page) */
#define VM_MAX_CACHE_HIT    	256	/* max pages in a row in cache before */

struct backing_dev_info default_backing_dev_info = {
	.ra_pages	= (VM_MAX_READAHEAD * 1024) / PAGE_CACHE_SIZE,
	.state		= 0,
	.capabilities	= BDI_CAP_MAP_COPY,
	//.unplug_io_fn	= default_unplug_io_fn,
};



//from fs/block_dev.c
struct vfsmount;

static struct vfsmount *bd_mnt ;

static DEFINE_SPINLOCK(bdev_lock);

static LIST_HEAD(all_bdevs);

struct address_space_operations def_blk_aops;
/* = {
	.readpage	= blkdev_readpage,
	.writepage	= blkdev_writepage,
	.sync_page	= block_sync_page,
	.prepare_write	= blkdev_prepare_write,
	.commit_write	= blkdev_commit_write,
	.writepages	= generic_writepages,
	.direct_IO	= blkdev_direct_IO,
    };*/

static int bdev_test(struct inode *inode, void *data)
{
	return BDEV_I(inode)->bdev.bd_dev == *(dev_t *)data;
}

static int bdev_set(struct inode *inode, void *data)
{
	BDEV_I(inode)->bdev.bd_dev = *(dev_t *)data;
	return 0;
}

static inline unsigned long hash_dev(dev_t dev)
{
	return MAJOR(dev)+MINOR(dev);
}

struct block_device *bdget(dev_t dev)
{
	struct block_device *bdev;
	struct inode *inode;

    //
	inode = iget5_locked(bd_mnt->mnt_sb, hash_dev(dev),
			bdev_test, bdev_set, &dev);

	if (!inode)
		return NULL;

	bdev = &BDEV_I(inode)->bdev;

	if (inode->i_state & I_NEW) {
		bdev->bd_contains = NULL;
		bdev->bd_inode = inode;
		bdev->bd_block_size = (1 << inode->i_blkbits);
		bdev->bd_part_count = 0;
		bdev->bd_invalidated = 0;
		inode->i_mode = S_IFBLK;
		inode->i_rdev = dev;
		inode->i_bdev = bdev;
		inode->i_data.a_ops = &def_blk_aops;
        //
		mapping_set_gfp_mask(&inode->i_data, GFP_USER);
		inode->i_data.backing_dev_info = &default_backing_dev_info;
		spin_lock(&bdev_lock);
        //
		list_add(&bdev->bd_list, &all_bdevs);
		spin_unlock(&bdev_lock);
		unlock_new_inode(inode);
	}
	return bdev;
}
