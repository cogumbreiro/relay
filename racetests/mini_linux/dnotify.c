#include <string.h>
#include "spinlock_types.h"

#define DN_MULTISHOT   0x80000000




int dir_notify_enable = 1;

struct dnotify_struct;

struct inode {
  //	struct hlist_node	i_hash;
  //	struct list_head	i_list;
  //	struct list_head	i_sb_list;
  //	struct list_head	i_dentry;
	unsigned long		i_ino;
  //	atomic_t		i_count;
  //	umode_t			i_mode;
	unsigned int		i_nlink;
  //	uid_t			i_uid;
  //	gid_t			i_gid;
  //	dev_t			i_rdev;
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
  //	struct semaphore	i_sem;
  //	struct rw_semaphore	i_alloc_sem;
  //	struct inode_operations	*i_op;
  //	struct file_operations	*i_fop;	/* former ->i_op->default_file_ops */
  //	struct super_block	*i_sb;
  //	struct file_lock	*i_flock;
  //	struct address_space	*i_mapping;
  //	struct address_space	i_data;

  //	struct dquot		*i_dquot[MAXQUOTAS];

	/* These three should probably be a union */
  //	struct list_head	i_devices;
  //	struct pipe_inode_info	*i_pipe;
  //	struct block_device	*i_bdev;
  //	struct cdev		*i_cdev;
	int			i_cindex;

  //	__u32			i_generation;


	unsigned long		i_dnotify_mask; /* Directory notify events */
  	struct dnotify_struct	*i_dnotify; /* for directory notifications */



  //	struct list_head	inotify_watches; /* watches on this inode */
  //	struct semaphore	inotify_sem;	/* protects the watches list */


	unsigned long		i_state;
	unsigned long		dirtied_when;	/* jiffies of first dirtying */

	unsigned int		i_flags;

  //	atomic_t		i_writecount;
	void			*i_security;
	union {
		void		*generic_ip;
	} u;

  //	seqcount_t		i_size_seqcount;

};

struct dentry {
  //	atomic_t d_count;
	unsigned int d_flags;		/* protected by d_lock */
	spinlock_t d_lock;		/* per dentry lock */
	struct inode *d_inode;		/* Where the name belongs to - NULL is
					 * negative */
	/*
	 * The next three fields are touched by __d_lookup.  Place them here
	 * so they all fit in a cache line.
	 */
  //	struct hlist_node d_hash;	/* lookup hash list */
  	struct dentry *d_parent;	/* parent directory */
  //	struct qstr d_name;

  //	struct list_head d_lru;		/* LRU list */
  //	struct list_head d_child;	/* child of parent list */
  //	struct list_head d_subdirs;	/* our children */
  //	struct list_head d_alias;	/* inode alias list */
	unsigned long d_time;		/* used by d_revalidate */
  //	struct dentry_operations *d_op;
  //	struct super_block *d_sb;	/* The root of the dentry tree */
	void *d_fsdata;			/* fs-specific data */
  //	struct rcu_head d_rcu;
  //	struct dcookie_struct *d_cookie; /* cookie, if any */
	int d_mounted;
  //	unsigned char d_iname[DNAME_INLINE_LEN_MIN];	/* small names */
};


struct file {
	/*
	 * fu_list becomes invalid after file_free is called and queued via
	 * fu_rcuhead for RCU freeing
	 */
  //	union {
  //		struct list_head	fu_list;
  //		struct rcu_head 	fu_rcuhead;
  //	} f_u;
	struct dentry		*f_dentry;
  //	struct vfsmount         *f_vfsmnt;
  //	struct file_operations	*f_op;
  //	atomic_t		f_count;
	unsigned int 		f_flags;
  //	mode_t			f_mode;
  //	loff_t			f_pos;
  //	struct fown_struct	f_owner;
	unsigned int		f_uid, f_gid;
  //	struct file_ra_state	f_ra;

  	unsigned long		f_version;
	void			*f_security;

	/* needed for tty driver, and maybe others */
	void			*private_data;

	/* Used by fs/eventpoll.c to link all the hooks to this file */
  //	struct list_head	f_ep_links;
	spinlock_t		f_ep_lock;

  //	struct address_space	*f_mapping;
};

struct files_struct;

struct fdtable {
	unsigned int max_fds;
	int max_fdset;
	int next_fd;
	struct file ** fd;      /* current fd array */
  //	fd_set *close_on_exec;
  //	fd_set *open_fds;
  //	struct rcu_head rcu;
	struct files_struct *free_files;
	struct fdtable *next;
};

/*
 * Open file table structure
 */
struct files_struct {
  //	atomic_t count;
	struct fdtable *fdt;
	struct fdtable fdtab;
  //	fd_set close_on_exec_init;
  //	fd_set open_fds_init;
  //	struct file * fd_array[NR_OPEN_DEFAULT];
	spinlock_t file_lock;    
  /* Protects concurrent writers.  Nests inside tsk->alloc_lock */
};

typedef struct files_struct *fl_owner_t;


struct dnotify_struct {
	struct dnotify_struct *	dn_next;
	unsigned long		dn_mask;
	int			dn_fd;
	struct file *		dn_filp;
	fl_owner_t		dn_owner;
};



static void redo_inode_mask(struct inode *inode)
{
	unsigned long new_mask;
	struct dnotify_struct *dn;

	new_mask = 0;
	for (dn = inode->i_dnotify; dn != NULL; dn = dn->dn_next)
		new_mask |= dn->dn_mask & ~DN_MULTISHOT;
	inode->i_dnotify_mask = new_mask;
}


void dnotify_flush(struct file *filp, fl_owner_t id)
{
	struct dnotify_struct *dn;
	struct dnotify_struct **prev;
	struct inode *inode;

	inode = filp->f_dentry->d_inode;
    //	if (!S_ISDIR(inode->i_mode))
    //		return;
	spin_lock(&inode->i_lock);
	prev = &inode->i_dnotify;
	while ((dn = *prev) != NULL) {
		if ((dn->dn_owner == id) && (dn->dn_filp == filp)) {
			*prev = dn->dn_next;
			redo_inode_mask(inode);
            //			kmem_cache_free(dn_cache, dn);
			break;
		}
		prev = &dn->dn_next;
	}
	spin_unlock(&inode->i_lock);
}


void __inode_dir_notify(struct inode *inode, unsigned long event)
{
	struct dnotify_struct *	dn;
	struct dnotify_struct **prev;
	struct fown_struct *	fown;
	int			changed = 0;

	spin_lock(&inode->i_lock);
	prev = &inode->i_dnotify;
	while ((dn = *prev) != NULL) {
		if ((dn->dn_mask & event) == 0) {
			prev = &dn->dn_next;
			continue;
		}
        //		fown = &dn->dn_filp->f_owner;
        //		send_sigio(fown, dn->dn_fd, POLL_MSG);
		if (dn->dn_mask & DN_MULTISHOT)
			prev = &dn->dn_next;
		else {
			*prev = dn->dn_next;
			changed = 1;
            //			kmem_cache_free(dn_cache, dn);
		}
	}
	if (changed)
		redo_inode_mask(inode);
	spin_unlock(&inode->i_lock);
}


struct file *acct_globals;

void callDNotifyStuff () {

  dnotify_flush (acct_globals, 0);

  __inode_dir_notify (acct_globals->f_dentry->d_inode, 0);

}
