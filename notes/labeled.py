from relay.warnings.search import *
from relay.warnings.models import *

#--- For the 3_8_2007 run ---
r = Run.objects.get(id=9)
rcs = Race_cluster.objects.filter(run=r)


#### Adding tags

initLabel = getLabel("Initialization")
aliasLabel = getLabel("Unlikely aliasing")
uniqueLabel = getLabel("Unique")
reentLabel = getLabel("Reentrant lock")
notParLabel = getLabel("Not in parallel")
condLabel = getLabel("Conditional lock")
raceLabel = getLabel("Race")
bugLabel = getLabel("Bug")

#------------------ from 1_30_2007 notes -----------------

#--

results = race_on_at(rcs, "shift_state", "shift_state",
                     [("drivers/char/keyboard.c", 385),
                      ("drivers/char/keyboard.c", 361)])

#3 rc

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "app_abort_code", "app_abort_code",
                     [("net/rxrpc/call.c", 102),
                      ("net/rxrpc/call.c", 889)])

#2 rc

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "tk_calldata", "tk_calldata",
                     [("net/sunrpc/sched.c", 285),
                      ("net/sunrpc/sched.c", 898)])


#2 rc

for rc in results:
    rc.add_label(notParLabel) # custom mutex
    rc.add_label(initLabel)



#--

results = race_on_at(rcs, "wasted_size", "wasted_size",
                     [("fs/jffs2/nodemgmt.c", 176),
                      ("fs/jffs2/nodemgmt.c", 329),
                      ("fs/jffs2/wbuf.c", 162)])

#1 rc

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "cl_dentry", "cl_dentry",
                     [("net/sunrpc/clnt.c", 81),
                      ("net/sunrpc/clnt.c", 82),
                      ])

#2 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "mnt_parent", "mnt_parent",
                     [("fs/namespace.c", 683),
                      ("fs/super.c", 841),
                      ])


#1 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "end", "end",
                     [("net/core/skbuff.c", 658),
                      ("net/core/skbuff.c", 860),
                      ])
#2 rc

results[0].add_label(uniqueLabel)
results[1].add_label(initLabel)

# one involving line 164 is also init

#3 total, but don't know what 3rd is

#--

results = race_on_at(rcs, "i_state", "i_state",
                     [("fs/fs-writeback.c", 100),
                      ("fs/fs-writeback.c", 75),
                      ])

#2 rc

#10 total
#benign race -- idempotent

#0, 1, 2, 3

results[3].add_label(raceLabel)
results[4].add_label(raceLabel)

#5, 6, 7, 8, 9 skip


#--

results = race_on_at(rcs, "i_security", "i_security",
                     [("fs/inode.c", 136)
                      ])

#1 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "nr_sectors", "nr_sectors",
                     [("block/ll_rw_blk.c", 1298),
                      ("block/ll_rw_blk.c", 2681),
                      ])
    
#1 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vm86_irqs[0].tsk", "vm86_irqs[0].tsk",
                     [("arch/i386/kernel/vm86.c", 739),
                      ("arch/i386/kernel/vm86.c", 750),
                      ])

#2 rc

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "(c->gc_task)->prio",
                     "((pdflush_list.next)->who)->prio",
                     [("kernel/sched.c", 819),
                      ("kernel/sched.c", 609),
                      ("kernel/sched.c", 610),
                      ("kernel/sched.c", 650),
                      ("kernel/sched.c", 651),
                      ("kernel/sched.c", 677),
                      ("kernel/sched.c", 684),
                      ("kernel/sched.c", 818),
                      ("kernel/sched.c", 1384),
                      ])

#1 rc

for rc in results:
    rc.add_label(aliasLabel)

#--


results = race_on_at(rcs, "number",
                     "number",
                     [("drivers/pci/probe.c", 359),
                      ("drivers/pci/probe.c", 741),
                      ])

#4 rc

#0 is init (for 6_10_2007), 1 not, ...

results[0].add_label(initLabel)
results[2].add_label(initLabel)
results[3].add_label(initLabel)
results[4].add_label(initLabel)


for rc in results:
    rc.add_label(initLabel)



#--

results = race_on_at(rcs, "array", "array",
                     [("kernel/sched.c", 1258),
                      ("kernel/sched.c", 1431),
                      ])

#1 rc

results[1].add_label(initLabel)

#2 total, but skipped

#--

results = race_on_at(rcs, "df_list.next", "(ibmphp_slot_head.next)->next",
                     [("include/linux/list.h", 313),
                      ("drivers/pci/hotplug/ibmphp_hpc.c", 845),
                      ])

#1 rc

for rc in results:
    rc.add_label(aliasLabel)


#--

results = race_on_at(rcs, "stime", "stime",
                     [("kernel/fork.c", 946),
                      ("kernel/posix-cpu-timers.c", 462),
                      ])

#2 rc

for rc in results:
    rc.add_label(initLabel)

#--


results = race_on_at(rcs, "proc_inum_idr.id_free_cnt",
                     "proc_inum_idr.id_free_cnt",
                     [("lib/idr.c", 59),
                      ("lib/idr.c", 77),
                      ])

#REASON: RACE but designed that way (detects and retries)

#1 rc

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "i_bdev", "i_bdev",
                     [("fs/inode.c", 133),
                      ("fs/inode.c", 261),
                      ])
#3 rc

for rc in results:
    rc.add_label(initLabel)

#2 rc (6_10)
results = race_on_at_exact(rcs, "i_bdev", "i_bdev", 
                           ("fs/inode.c", 133), ("fs/inode.c", 261))

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "exit_signal", "exit_signal",
                      [("kernel/fork.c", 1026),
                      ])

#1 rc

for rc in results:
    rc.add_label(initLabel)

#--

#BOOKMARK

results = race_on_at(rcs, "assoc_mapping", "assoc_mapping",
                     [("fs/inode.c", 150),
                      ])

#2 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "utime", "utime",
                     [("kernel/fork.c", 945),
                      ])

#2 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "real_parent", "real_parent",
                     [("kernel/fork.c", 1073),
                      ("kernel/fork.c", 1070),
                      ])

#0 rc

#--

results = race_on_at(rcs, "dentry_unused", "dentry_unused",
                     [("include/linux/list.h", 164),
                      ("fs/dcache.c", 465),
                      ])
#1 rc

for rc in results:
    rc.add_label(aliasLabel)


results = race_on_at_exact(rcs, "dentry_unused", "dentry_unused",
                           ("include/linux/list.h", 164),
                           ("fs/dcache.c", 465))

#--

Possible race between access to:
((pdflush_list.next)->who)->prio : mm/pdflush.c:47 and
((pdflush_list.next)->who)->prio : mm/pdflush.c:47
        Accessed at locs:
        kernel/sched.c:819 and
        kernel/sched.c:819
        Possible paths & LS (first 3):

update (2/16):

Possible race between access to:
((pdflush_list.next)->who)->prio @ mm/pdflush.c:47 and
_a164_620707_fork.prio @ kernel/fork.c:157
        Accessed at locs:
        [kernel/sched.c:819, ] and
        [kernel/sched.c:819, kernel/sched.c:1496, kernel/sched.c:1508, ]


//REASON lost lock: task_rq_lock uses ASM ptr arith to get lock?

SKIPPED

#--

#BOOKMARK

results = race_on_at(rcs, "pdflush_list.next", "(krxtimod_list.next)->next",
                     [("include/linux/list.h", 223),
                      ("net/rxrpc/krxtimod.c", 164),
                      ])

#1 rc

for rc in results:
    rc.add_label(aliasLabel)


#6_10 
results = race_on_at(rcs, "next", "next",
                     [("include/linux/list.h", 223),
                      ("net/rxrpc/krxtimod.c", 164)])

#0 rc

#--

results = race_on_at(rcs, "audit_freelist.next",
                     "(GlobalSMBSessionList.next)->next",
                     [("include/linux/list.h", 164),
                      ("fs/cifs/connect.c:135", 135),
                      ])
#1 rc

for rc in results:
    rc.add_label(aliasLabel)

#6_10 
results = race_on_at_exact(rcs, "next",
                           "next",
                           ("include/linux/list.h", 164),
                           ("fs/cifs/connect.c:135", 135))
#0 rcs?

#--

results = race_on_at(rcs, "(GlobalSMBSessionList.next)->next",
                     "cache_defer_list.next",
                     [("fs/cifs/connect.c", 135),
                      ("include/linux/list.h:164", 164),
                      ])
#0 rc

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "_a164_620707_fork", "_a164_620707_fork",
                     [("kernel/fork.c", 1124),
                      ])
#0 rc

for rc in results:
    rc.add_label(initLabel)


#--


results = race_on_at(rcs, "data_len", "data_len",
                     [("include/linux/skbuff.h", 764),
                      ("net/core/skbuff.c", 811),
                      ])

#2 rc

results[0].add_label(initLabel)
results[2].add_label(initLabel)

#3 total, but skipped 1

#6_10
results = race_on_at_exact(rcs, "data_len", "data_len",
                           ("include/linux/skbuff.h", 764), 
                           ("net/core/skbuff.c", 811))


#--

results = race_on_at(rcs, "per_cpu__sockets_in_use", "per_cpu__sockets_in_use",
                     [("net/socket.c", 526),
                      ("net/socket.c", 483),
                      ])
#0 rc


#--

results = race_on_at(rcs, "i_op", "i_op",
                     [("fs/inode.c", 121),
                      ])

#7 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vaddr", "vaddr",
                     [("net/ipv4/ipvs/ip_vs_conn.c", 614),
                      ])

#1 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "d_parent", "d_parent",
                     [("fs/dcache.c", 200),])

#3 rc

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "d_parent", "d_parent",
                     [("fs/dcache.c", 741),])

#6 rc

for rc in results:
    rc.add_label(initLabel)
    

#--

results = race_on_at(rcs, "virtual", "virtual",
                     [("mm/highmem.c", 543),
                      ("mm/highmem.c", 574),
                      ])


#0 rc

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "last_delta", "last_delta",
                     [("drivers/char/random.c", 602),
                      ("drivers/char/random.c", 603),
                      ])
#1 rc

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "i_nlink", "i_nlink",
                     [("fs/inode.c", 123),
                      ])
#7 rc

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "mnt_root", "mnt_root",
                     [("fs/dcache.c", 175),])

#1 rc
for rc in results:
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "page_address_pool", "page_address_pool",
                     [("mm/highmem.c", 573),
                      ("mm/highmem.c", 542),
                      ])

#0 rc

for rc in results:
    rc.add_label(uniqueLabel)

#--


results = race_on_at(rcs, "number", "number",
                     [("drivers/pci/hotplug/cpqphp_pci.c", 171),])

#3 rc

for rc in results:
    rc.add_label(initLabel)
    rc.add_label(aliasLabel)

#--


results = race_on_at(rcs, "d_op", "d_op",
                     [("fs/dcache.c", 167),
                      ("fs/dcache.c", 743),
                      ])
#2 rc
for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "pointer", None,
                     [("drivers/acpi/utilities/utmisc.c", 502),
                      ("include/asm/string.h", 425),
                      ])

#4 rc
for rc in results:
    rc.add_label(aliasLabel)


#--

results = race_on_at(rcs, None, "pointer", 
                     [("include/asm/string.h", 425),
                      ("drivers/acpi/utilities/utmisc.c", 502),
                      ])

#2 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "vector_irq", "vector_irq",
                     [("arch/i386/kernel/io_apic.c", 1173),
                      ])
#2 rc
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "last_delta2", "last_delta2",
                     [("drivers/char/random.c", 606),
                      ("drivers/char/random.c", 605),
                      ])
#2 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "d_flags", "d_flags",
                     [("fs/dcache.c", 175),])

#13 rc

for rc in results:
    rc.add_label(uniqueLabel)

# some may be unlikely alias, but i'm not sure (lots of directory entries from all over -- e.g., from caches, or from task lists)

#--

results = race_on_at(rcs, "i_hash", "i_hash",
                     [("include/linux/list.h", 541),
                      ("include/linux/list.h", 527),
                      ])
#1 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "wasted_size", "wasted_size", # debug
                      [("fs/jffs2/debug.c", 28)
                       ("fs/jffs2/debug.c", 32)])

results2 = race_on_at(rcs, "wasted_size", "wasted_size",
                      [("fs/jffs2/gc.c", 1190)              # uses other lock
                       ("fs/jffs2/gc.c", 1140)])

results3 = race_on_at(rcs, "wasted_size", "wasted_size",
                      [("fs/jffs2/nodemgmt.c", 542),
                       ("fs/jffs2/debug.c", 28)])

#1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "ops", "ops",
                     [("net/socket.c", 519),
                      ])
#2 rc

#two different threads, each given a unique socket

for rc in results:
    rc.add_label(aliasLabel)
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "count", "count",
                     [("lib/idr.c", 307),
                      ])

#1 rc
for rc in results:
    rc.add_label(aliasLabel)


#--

results = race_on_at(rcs, "crd_infd", "crd_infd",
                     [("init/do_mounts_rd.c", 407),
                      ])

#2 rc

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "nr_hw_segments", "nr_hw_segments",
                     [("block/ll_rw_blk.c", 1288),
                      ])

#skip (a lost lock but... don't know why)

#--

results = race_on_at(rcs, "hiwater_rss", "hiwater_rss",
                     [("mm/mmap.c", 1660),
                      ])

#0 rc

#LOST A RACE?

#--

results = race_on_at(rcs, "array", "array",
                     [("kernel/sched.c", 1431),
                      ])

#2 rc
results[0].add_label(aliasLabel)
results[1].add_label(initLabel)

#--

results = race_on_at(rcs, "ackr_win_bot", "ackr_win_bot",
                     [("net/rxrpc/call.c", 537),
                      ])
#5 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "sync_recv_mesg_maxlen", "sync_recv_mesg_maxlen",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 422),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 422),
                      ])
#2 rc
for rc in results:
    rc.add_label(notParLabel)


#--

results = race_on_at(rcs, "irq_2_pin", "irq_2_pin",
                     [("arch/i386/kernel/io_apic.c", 113),
                      ("arch/i386/kernel/io_apic.c", 113),
                      ])

#2 rc
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "mnt_pinned", "mnt_pinned",
                     [("fs/namespace.c", 308),
                      ])
#2 rc
for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "sleep_avg", "sleep_avg",
                     [("kernel/sched.c", 763),
                      ("kernel/sched.c", 756),
                      ])

#skip.. ptr arith

#--

results = race_on_at(rcs, "timeout", "timeout",
                     [("include/rxrpc/krxtimod.h", 36),
                      ])

#1 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "type", "type",
                     [("drivers/scsi/qla2xxx/qla_gs.c", 1519),
                      ])

#1 rc
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "ackr_pend_cnt", "ackr_pend_cnt",
                     [("net/rxrpc/call.c", 527),
                      ])

#2 rc
for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "__totlen", "__totlen",
                     [("fs/jffs2/wbuf.c", 315),
                      ])

#2 rc
for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "idle_timeout", "idle_timeout",
                     [("net/sunrpc/xprtsock.c", 1212),
                      ])
#2 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "nr_active", "nr_active",
                     [("mm/vmscan.c", 730),
                      ])

# skip

#--

results = race_on_at(rcs, "sel_start", "sel_start",
                     [("drivers/char/selection.c", 68),
                      ("drivers/char/selection.c", 70),
                      ])

#1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "bv_len", "bv_len",
                     [("mm/highmem.c", 411),
                      ])

#not sure anymore -- one path gives fresh objs... other path?

#--

results = race_on_at(rcs, "i_size", "i_size",
                     [("fs/inode.c", 125),
                      ]
#5 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "(mq->queue)->boundary_rq",
                     "((tr->blkcore_priv)->rq)->boundary_rq",
                     [("block/elevator.c", 347),
                      ("block/elevator.c", 472),
                      ])

#1 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "XXX", "XXX",
                     [("YYY", 277),
                      ("YYY", 270),
                      ])

#------------------ from 2_5_2007 ------------------

results = race_on_at(rcs, "bv_offset", "bv_offset",
                     [("mm/highmem.c", 412),
                      ])

#6 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "collisions", "collisions",
                     [("drivers/net/wireless/airo.c", 2251),
                      ])
#1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "(pgd_cache->array[0])->avail",
                     "((uid_cachep->nodelists[0])->shared)->avail",
                     [("mm/slab.c", 2786),
                      ("mm/slab.c", 2699),
                      ])

# not sure what to say about them anymore, but others seem to
# be unlikely aliases

#0 rc

#--

results = race_on_at(rcs, "ar_hln", "ar_hln",
                     [("net/core/netpoll.c", 430),
                      ])

# lost lock, skip for now

#--

results = race_on_at(rcs, "vc_origin", "vc_origin",
                     [("drivers/char/vt.c", 573),
                      ("drivers/char/vt.c", 572),
                      ])
#1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "s_element", "s_element",
                     [("fs/sysfs/dir.c", 48),
                      ])

#1 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vc_origin", "inode",
                     [("mm/slab.c", 2293),
                      ("drivers/char/vt.c", 597),
                      ])

#0 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "counter", "counter",
                     [("fs/jbd/journal.c", 361),
                      ])
#2 rc

# uses bit_spin_lock... count as not in parallel for now
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "event", "event",
                     [("fs/inotify.c:251", 251),
                      ])

#3 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "tx_bytes", "tx_bytes",
                     [("net/bluetooth/bnep/core.c", 456),
                      ])
#1 rc
for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs,  "journal", "swapper_space",
                     [("lib/radix-tree.c", 753),
                      ("lib/radix-tree.c", 273),
                      ])

#4 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "mnt_sb", "mnt_sb",
                     [("fs/super.c", 838),
                      ])
#11 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vm_mm", "vm_mm",
                     [("mm/hugetlb.c", 315),
                      ("kernel/fork.c", 236),
                      ])

#1 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "rpc_task_id", "rpc_task_id",
                     [("net/sunrpc/sched.c", 787),
                      ])

#1 rc
# may be a bug? race on a monotonic ID variable (what if two get the same?)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "tk_work.data", "tk_work.data",
                     [("net/sunrpc/sched.c", 285),
                      ])

#0 rc (TODO look at the ones for other fields though)

#--

results = race_on_at(rcs, "skbuff_fclone_cache", "skbuff_fclone_cache",
                     [("net/core/skbuff.c", 1798),
                      ])
#3 rc

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vc_pos", "vc_pos",
                     [("drivers/char/vt.c", 534),
                      ])

#3 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "mpc_dstapic", "mpc_dstapic",
                     [("arch/i386/kernel/io_apic.c", 1825),
                      ("arch/i386/kernel/io_apic.c", 783),
                      ])

#2 rc

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "queuelist.prev", "queuelist.prev",
                     [("block/ll_rw_blk.c:270", 270),
                      ("include/linux/list.h", 222),
                      ])
# skip for now (2)

#--

results = race_on_at(rcs, "nr_threads", "nr_threads",
                     [("kernel/fork.c", 1139),
                      ("kernel/fork.c", 917),
                      ])
#3 rc

for rc in results:
    rc.add_label(raceLabel)
    
#--

results = race_on_at(rcs, "audit_freelist", "pci_devices",
                     [("include/linux/list.h", 164),
                      ("drivers/pci/search.c", 245),
                      ])

#1 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "parser_state.aml", "vc_origin",
                     [("drivers/acpi/parser/psargs.c", 80),
                      ("drivers/char/vt.c", 597),
                      ])
#1 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "tk_callback", "tk_callback",
                     [("net/sunrpc/sched.c", 622),
                      ("net/sunrpc/sched.c", 610),
                      ])

#2 rc
for rc in results:
    rc.add_label(initLabel)
    rc.add_label(uniqueLabel)

#-- 

results = race_on_at(rcs, "fl_type", "fl_type",
                     [("fs/locks.c", 286),
                      ("fs/locks.c", 1102),
                      ])

#4 rc
for rc in results:
    rc.add_label(initLabel)
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "i_mmap_writable", "i_mmap_writable",
                     [("mm/mmap.c", 169),
                      ])

#3 rc
for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "g_cpucache_up", "g_cpucache_up",
                     [("mm/slab.c", 654),
                      ])

#0 rc
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "end", "end",
                     [("net/core/skbuff.c", 658),
                      ])
#3 rc

# supposed to have a ref count of 1... should check why
for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "s_count", "s_count",
                     [("fs/super.c", 172),
                      ])

# atomic_dec_and_lock thing skip

#--

results = race_on_at(rcs, "j_commit_sequence", "j_commit_sequence",
                     [("fs/jbd/commit.c", 828),
                      ])

#1 rc
#racy read for printing

for rc in results:
    rc.add_label(raceLabel)
    

#--

results = race_on_at(rcs, "body_len", "body_len",
                     [("init/initramfs.c", 103),
                      ])
#2 rc
for rc in results:
    rc.add_label(reentLabel)
    
#--

results = race_on_at(rcs, "tinfo.task", "tinfo.task",
                     [("arch/i386/kernel/irq.c", 138),
                      ])

#1 rc
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "fl_owner", "fl_owner",
                     [("fs/locks.c", 226),
                      ])

#2 rc

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "event", "event",
                     [("fs/namespace.c", 137),
                      ("fs/namespace.c", 144),
                      ])
#2 rc

for rc in results:
    rc.add_label(reentLabel)

#--

# pending.signal.sig[0] @ signal.h:182 (may be race if called through asm folks?)

#--

results = race_on_at(rcs, "size_or_mask", "size_or_mask",
                     [("arch/i386/kernel/cpu/mtrr/main.c", 620),
                      ])

#2 rc
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "fu_list.next", "fu_list.next",
                     [("fs/file_table.c", 98),
                      ])

#6 rc
for rc in results:
    rc.add_label(initLabel)

#results[0] could be lock_kernel

#--

results = race_on_at(rcs, "vc_origin", "inode",
                     [("drivers/char/vt.c", 597),
                      ("mm/slab.c", 2299),
                      ])
#0 rc
for rc in results:
    rc.add_label(aliasLabel)

results = race_on_at(rcs, "inode", "vc_origin", 
                     [("drivers/char/vt.c", 597),
                      ("mm/slab.c", 2299),
                      ])

#2 rc
for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "crc_32_tab", "crc_32_tab",
                     [("init/../lib/inflate.c", 1066),
                      ("init/initramfs.c", 409),
                      ])
#4 rc
for rc in results:
    rc.add_label(reentLabel)


#--

results = race_on_at(rcs, "stop_backup_sync", "stop_backup_sync",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 739),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 712),
                      ])

#2 rc
for rc in results:
    rc.add_label(raceLabel)

#--
results = race_on_at(rcs, "name_buf", "name_buf",
                     [("init/initramfs.c", 422),
                      ("init/initramfs.c", 424),
                      ])

#2 rc
for rc in results:
    rc.add_label(reentLabel)

#------------------ from 3_8_2007 (w/ locks) -----


#--
results = race_on_at(rcs, "nr_msix_devices", "nr_msix_devices",
                     [("drivers/pci/msi.c", 497),
                      ])

# race that was removed in later version changed
for rc in results:
    rc.add_label(raceLabel)


#--
results = race_on_at(rcs, "cl_lease_time", "cl_lease_time",
                     [("fs/nfs/nfs4proc.c", 2680),
                      ("fs/nfs/nfs4renewd.c", 106),
                      ])

# 1 rc
for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "t_checkpoint_list", "t_checkpoint_list",
                     [("fs/jbd/checkpoint.c", 593),
                      ("fs/jbd/commit.c", 715),
                      ])

# actual race is with parent thread, not the path given

# 1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "vmlist->size", "vmlist->size",
                     [("mm/vmalloc.c", 277),
                      ("mm/vmalloc.c", 270),
                      ])

#1 rc

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "((*(c->inocache_list)))->state",
                     "((*(c->inocache_list)))->state",
                     [("fs/jffs2/gc.c", 194),
                      ("fs/jffs2/gc.c", 167),
                      ])

#lost lock from switch stmt? skip?

#--

results = race_on_at(rcs, "console_locked", "console_locked",
                     [("kernel/printk.c", 731),
                      ("kernel/printk.c", 619),
                      ])

#2 rc

#for warning if console is unlocked
#(won't have locks on itself in that case)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "nf_bridge", "nf_bridge",
                     [("net/core/skbuff.c", 432),
                      ])

#3 rc

#either gets fresh or reuses old
for rc in results:
    rc.add_label(initLabel)
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "fsm.result", "fsm.result",
                     [("drivers/net/irda/sir_kthread.c", 266),
                      ])
#2 rc
for rc in results:
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "b_frozen_data", "b_frozen_data",
                     [("fs/jbd/commit.c", 752),
                      ("fs/jbd/commit.c", 750),
                      ])

#2 rc
#bit spinlock
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "log_level_unknown", "log_level_unknown",
                     [("kernel/printk.c", 598),
                      ("kernel/printk.c", 604),
                      ])

#3 rc
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "offset", "offset",
                     [("net/rxrpc/call.c", 1288),
                      ])

#2 rc
for rc in results:
    rc.add_label(uniqueLabel)



#--

results = race_on_at(rcs, "lowest_bit", "lowest_bit",
                     [("mm/swapfile.c", 259),
                      ])

#3 rc
for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "anon_vma", "anon_vma",
                     [("mm/rmap.c", 106),
                      ])

#5 rc

#(double-checked locking?)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "idr.layers", "idr.layers",
                     [("lib/idr.c", 329),
                      ("lib/idr.c", 337),
                      ])

#3 rc

#(depends on memory consistency?!)
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "nh", "nh",
                     [("net/core/skbuff.c", 395),
                      ])

#1 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "ifi_change", "ifi_change",
                     [("net/core/wireless.c", 1124),
                      ])

#1 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "oops_in_progress", "oops_in_progress",
                     [("arch/i386/mm/fault.c", 42),
                      ("arch/i386/mm/fault.c", 48),
                      ])

#3 rc

# after an oops... bad if you're trying to unoops and oops
# at the same time? then it's an atomicity issue anyway
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "rta_len", "rta_len",
                     [("net/core/rtnetlink.c", 128),
                      ])

#1 rc
for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "cl_lease_time", "cl_lease_time",
                     [("fs/nfs/nfs4proc.c", 2655),
                      ])

#1 rc
for rc in results:
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "h_reclaiming", "h_reclaiming",
                     [("fs/lockd/clntlock.c", 245),
                      ])

#1 rc
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "oldx", "oldx",
                     [("drivers/char/vt.c", 461),
                      ])
#3 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "shared.vm_set.list.prev", "shared.vm_set.list.prev",
                     [("include/linux/list.h", 223),
                      ])
#3 rc
for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "gc_task", "gc_task",
                     [("fs/jffs2/background.c", 81),
                      ])

#2 rc
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "mc_list", "mc_list",
                     [("net/ipv4/igmp.c", 1148),
                      ("net/ipv4/devinet.c", 123),
                      ])

#1 rc

# (conditional lock / unique / overlapping locksets)
for rc in results:
    rc.add_label(condLabel)
    rc.add_label(uniqueLabel)



#--

results = race_on_at(rcs, "proto_filter", "proto_filter",
                     [("net/bluetooth/bnep/core.c", 121),
                      ("net/bluetooth/bnep/core.c", 124),
                      ("net/bluetooth/bnep/core.c", 127),
                      ])

#1 rc

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "logged_chars", "logged_chars",
                     [("kernel/printk.c", 449),
                      ])

#3 rc

for rc in results:
    rc.add_label(reentLabel)


#--

results = race_on_at(rcs, "nextblock", "nextblock",
                     [("fs/jffs2/nodemgmt.c", 264),
                      ])

#1 rc

# uses alloc_sem sometimes, c->erase_completion_lock sometimes
# (may not have a common lock all the time?)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "pidmap_array.page", "pidmap_array.page",
                     [("kernel/pid.c", 96),
                      ])

#2 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "printk_buf", "printk_buf",
                     [("kernel/printk.c", 554),
                      ])
#2 rc
for rc in results:
    rc.add_label(reentLabel)

#--
results = race_on_at(rcs, "irq_desc.status", "irq_desc.status",
                     [("kernel/irq/manage.c", 42),
                      ])

# 1 rc

# (racy read used for sync?)
for rc in results:
    rc.add_label(raceLabel)

#--
results = race_on_at(rcs, "thread_finished", "thread_finished",
                     [("drivers/pci/hotplug/cpci_hotplug_core.c", 584),
                      ])

#4 rc

# on of those flag sets for killing a thread
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "swap_list.next", "swap_list.next",
                     [("mm/swapfile.c", 263),
                      ])
#5 rc

for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "audit_skb_queue.qlen", "audit_skb_queue.qlen",
                     [("include/linux/skbuff.h", 644),
                      ("include/linux/skbuff.h", 590)
                      ])
#1 rc
for rc in results:
    rc.add_label(raceLabel)


#--
results = race_on_at(rcs, "swap_token_mm", "swap_token_mm",
                     [("include/linux/swap.h", 244),
                      ])

#4 rc (double-checked locking)
for rc in results:
    rc.add_label(raceLabel)


#--
results = race_on_at(rcs, "nr_skbs", "nr_skbs",
                     [("net/core/netpoll.c", 220),
                      ])
#1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "swap_token_check", "swap_token_check",
                     [("mm/thrash.c", 68),
                      ])

#3 rc

# (usually not reachable, but if a timeout val is set, it will happen)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "nr_free_blocks", "nr_free_blocks",
                     [("fs/jffs2/gc.c", 1087),
                      ])

#1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "j_revoke", "j_revoke",
                     [("fs/jbd/revoke.c", 515),
                      ])

#1 rc
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "(tr->blkcore_priv)->rq", "(tr->blkcore_priv)->rq",
                     [("drivers/mtd/mtd_blkdevs.c", 80),
                      ])
#1 rc
for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "surprise_rm_pending", "surprise_rm_pending",
                     [("drivers/pci/hotplug/pciehp_ctrl.c", 476),
                      ])

#2 rc
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "nr_reserved_vectors", "nr_reserved_vectors",
                     [("drivers/pci/msi.c", 499),
                      ])

#1 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "j_flags", "j_flags",
                     [("fs/jbd/commit.c", 198),
                      ("fs/jbd/journal.c", 1460),
                      ])

#1 rc
#should use atomic read/write bitvector funcs?
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "gcblock", "gcblock",
                     [("fs/jffs2/gc.c", 511),
                      ("fs/jffs2/gc.c", 1134),
                      ("fs/jffs2/gc.c", 1184),                      
                      ])

#1 rc
# race (not sure of severity)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "(journal->j_superblock)->s_start",
                     "(journal->j_superblock)->s_start",
                     [("fs/jbd/journal.c", 937),
                      ])
#1 rc
#double checked
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "acct_globals.file", "acct_globals.file",
                     [("kernel/acct.c", 559),
                      ])

#1 race
#due to "accelerating common fastpath"... not sure of implications
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "dirty_size", "dirty_size",
                     [("fs/jffs2/debug.c", 38),
                      ("fs/jffs2/debug.c", 41),
                      ])

#1 race
# for debugging
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "nr_erasing_blocks", "nr_erasing_blocks",
                     [("fs/jffs2/gc.c", 1087),
                      ])

#1 race
#(similar to other c->blah races which use f->sem instead of erase_lock)
for rc in results:
    rc.add_label(raceLabel)
    
#--

results = race_on_at(rcs, "erasing_size", "erasing_size",
                     [("fs/jffs2/debug.c", 38),
                      ("fs/jffs2/debug.c", 41),
                      ])
#1 rc
#for debug
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "(jffs2_compressor_list.next)->compr_buf",
                     "(jffs2_compressor_list.next)->compr_buf",
                     [("fs/jffs2/compr.c", 104),
                      ("fs/jffs2/compr.c", 126),
                      ])

#1 race
#i said may be a bug (from trying to free null?)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "rt_deadline", "rt_deadline",
                     [("net/ipv4/route.c", 681),
                      ("net/ipv4/route.c", 715),
                      ])

#2 rc 
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "unchecked_size", "unchecked_size",
                     [("fs/jffs2/debug.c", 38),
                      ("fs/jffs2/debug.c", 41),
                      ])
#1 rc
# for debugging
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "none_stat_decompr_blocks",
                     "none_stat_decompr_blocks",
                     [("fs/jffs2/compr.c", 182),
                      ("fs/jffs2/gc.c", 747),
                      ])

#1 rc
for rc in results:
    rc.add_label(notParLabel)


#--

results = race_on_at(rcs, "none_stat_compr_blocks",
                     "none_stat_compr_blocks",
                     [("fs/jffs2/compr.c", 157),
                      ("fs/jffs2/gc.c", 1267),
                      ])

#1 rc
for rc in results:
    rc.add_label(notParLabel)


#--

results = race_on_at(rcs, "softcursor_original", "softcursor_original",
                     [("drivers/char/vt.c", 520),
                      ])
#2 rc
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "(audit_freelist.next)->gfp_mask",
                     "(audit_freelist.next)->gfp_mask",
                     [("kernel/audit.c", 591),
                      ])

#1 rc
#(custom free list)
for rc in results:
    rc.add_label(initLabel)

#--


results = race_on_at(rcs, "mem_map->mapping", "mem_map->mapping",
                     [("mm/page_alloc.c", 129),
                      ("fs/buffer.c", 848)])

#1 rc
#(maybe race... comment said so)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "console_sem.wait.task_list.next",
                     "console_sem.wait.task_list.next",
                     [("include/linux/wait.h", 84),
                      ])

#1 rc
# how severe?
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "socket->socket", "socket->socket",
                     [("drivers/pcmcia/cs.c", 649),
                      ("drivers/pcmcia/cs.c", 324),
                      ])
#1 rc
#(idempotent)
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "((irda_rq_queue.request_list.next)->data)->fsm.state", "((irda_rq_queue.request_list.next)->data)->fsm.state",
                     [("drivers/net/irda/sir_kthread.c", 413),
                      ("drivers/net/irda/sir_kthread.c", 254),
                      ])
#2 rc
for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "((irda_rq_queue.request_list.next)->data)->fsm.param", "((irda_rq_queue.request_list.next)->data)->fsm.param",
                     [("drivers/net/irda/sir_kthread.c", 290),
                      ])
#2 rc
for rc in results:
    rc.add_label(uniqueLabel)
    
#--

results = race_on_at(rcs, "swap_info[0].highest_bit",
                     "swap_info[0].highest_bit",
                     [("mm/swapfile.c", 261),
                      ("mm/swapfile.c", 260),
                      ])

#3 rc
for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "stop_backup_sync", "stop_backup_sync",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 739),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 742),
                      ])

#2 rc
#(thread stop flag)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "swap_cache_info.exist_race",
                     "swap_cache_info.exist_race",
                     [("mm/swap_state.c", 110),
                      ])

#2 rc stat variable (to count races... haha)
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "audit_backlog_wait_time",
                     "audit_backlog_wait_time",
                     [("kernel/audit.c", 695),
                      ])

#3 rc

# always set to the same var X, and X is always 0?
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "sync_send_mesg_maxlen", "sync_send_mesg_maxlen",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 414),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 414),
                      ])

#2 rc
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "console_printk[0]", "console_printk[0]",
                     [("arch/i386/mm/fault.c", 54),
                      ])

#3 rc (during panic / when unbusting spinlocks)
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "shift_down[0]", "shift_down[0]",
                     [("drivers/char/keyboard.c", 384),
                      ])

#1 rc
#(during panic)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "panic_blink", "panic_blink",
                     [("kernel/panic.c", 99),
                      ])

#2 rc
#(during panic)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "input_timer_state.last_time",
                     "input_timer_state.last_time",
                     [("drivers/char/random.c", 600),
                      ("drivers/char/random.c", 599),
                      ])
#2 rc
#(benign -- for randomizer)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "XXX", "XXX",
                     [("YYY", 277),
                      ("YYY", 270),
                      ])
