#!/usr/bin/env python


# Set up environment to work when script is symlink
import sys
import os
sys.path.insert(0, os.getcwd())


# copy Django shell stuff
import django
from django.core.management import setup_environ
try:
    import settings # Assumed to be in the same directory.
except ImportError:
    import sys
    sys.stderr.write("Error: Can't find the file 'settings.py' in the directory containing %r. It appears you've customized things.\nYou'll have to run django-admin.py, passing it your settings module.\n(If the file settings.py does indeed exist, it's causing an ImportError somehow.)\n" % __file__)
    sys.exit(1)

if __name__ == "__main__":
    setup_environ(settings)



# do the loading of labels
from relay.warnings.search import *
from relay.warnings.models import *


#--- For the 6_10_2007 run ---
r = Run.objects.get(id=13)
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


def expect (results, num):
    l = len(results)
    tail = ""
    if l != num :
        tail = "DIFFERENT!"
    print "expecting %d %s" % (num, tail)
    if (l > 0):
        print results[0].first_race()
    print "\n\n"
    return


#---------------------------------------------------------
#------------------ from 1_30_2007 notes -----------------

#--

results = race_on_at(rcs, "shift_state", "shift_state",
                     [("drivers/char/keyboard.c", 385),
                      ("drivers/char/keyboard.c", 361)])

expect (results, 3)

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "app_abort_code", "app_abort_code",
                     [("net/rxrpc/call.c", 102),
                      ("net/rxrpc/call.c", 889)])

expect (results, 2)

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "tk_calldata", "tk_calldata",
                     [("net/sunrpc/sched.c", 285),
                      ("net/sunrpc/sched.c", 898)])


expect (results, 2)

for rc in results:
    rc.add_label(notParLabel) # custom mutex
    rc.add_label(initLabel)



#--

results = race_on_at(rcs, "wasted_size", "wasted_size",
                     [("fs/jffs2/nodemgmt.c", 176),
                      ("fs/jffs2/nodemgmt.c", 329),
                      ("fs/jffs2/wbuf.c", 162)])

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "cl_dentry", "cl_dentry",
                     [("net/sunrpc/clnt.c", 81),
                      ("net/sunrpc/clnt.c", 82),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "mnt_parent", "mnt_parent",
                     [("fs/namespace.c", 683),
                      ("fs/super.c", 841),
                      ])


expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "end", "end",
                     [("net/core/skbuff.c", 658),
                      ("net/core/skbuff.c", 860),
                      ])

expect (results, 2)

results[0].add_label(uniqueLabel)
results[1].add_label(initLabel)

# one involving line 164 is also init

#3 total, but don't know what 3rd is

#--

results = race_on_at(rcs, "i_state", "i_state",
                     [("fs/fs-writeback.c", 100),
                      ("fs/fs-writeback.c", 75),
                      ])

expect (results, 2)

#10 total
#benign race -- idempotent

#0 is benign
results[0].add_label(raceLabel)
results[1].add_label(raceLabel)

#2, 3, 4, ... skip (but look like they're benign too... 
#don't want to artificially inflate)


#--

results = race_on_at(rcs, "i_security", "i_security",
                     [("fs/inode.c", 136)
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at_exact(rcs, "nr_sectors", "nr_sectors",
                     ("block/ll_rw_blk.c", 1298), ("block/ll_rw_blk.c", 2681))
    
expect (results, 3)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vm86_irqs[0].tsk", "vm86_irqs[0].tsk",
                     [("arch/i386/kernel/vm86.c", 739),
                      ("arch/i386/kernel/vm86.c", 750),
                      ])

expect (results, 0)

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

expect (results, 1)


for rc in results:
    rc.add_label(aliasLabel)

#--


results = race_on_at(rcs, "number",
                     "number",
                     [("drivers/pci/probe.c", 359),
                      ("drivers/pci/probe.c", 741),
                      ])

expect (results, 4)


#0 is init (for 6_10_2007), 1 not, ...

results[0].add_label(initLabel)
results[2].add_label(initLabel)
results[3].add_label(initLabel)
results[4].add_label(initLabel)


for rc in results:
    rc.add_label(initLabel)



#--

results = race_on_at_exact(rcs, "array", "array",
                           ("kernel/sched.c", 1258),
                           ("kernel/sched.c", 1431))

expect (results, 1)

results[1].add_label(initLabel)

#2 total, but skipped

#--

#results = race_on_at(rcs, "df_list.next", "(ibmphp_slot_head.next)->next",
#                     [("include/linux/list.h", 313),
#                      ("drivers/pci/hotplug/ibmphp_hpc.c", 845),
#                      ])


results = race_on_at_exact(rcs, "next", "next",
                           ("include/linux/list.h", 313),
                           ("drivers/pci/hotplug/ibmphp_hpc.c", 845))

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)


#--

results = race_on_at(rcs, "stime", "stime",
                     [("kernel/fork.c", 946),
                      ("kernel/posix-cpu-timers.c", 462),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)

#--


results = race_on_at(rcs, "proc_inum_idr.id_free_cnt",
                     "proc_inum_idr.id_free_cnt",
                     [("lib/idr.c", 59),
                      ("lib/idr.c", 77),
                      ])

#REASON: RACE but designed that way (detects and retries)

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at_exact(rcs, "i_bdev", "i_bdev", 
                           ("fs/inode.c", 133), ("fs/inode.c", 261))

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "exit_signal", "exit_signal",
                      [("kernel/fork.c", 1026),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "assoc_mapping", "assoc_mapping",
                     [("fs/inode.c", 150),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "utime", "utime",
                     [("kernel/fork.c", 945),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "real_parent", "real_parent",
                     [("kernel/fork.c", 1073),
                      ("kernel/fork.c", 1070),
                      ])

expect (results, 0)

#--

results = race_on_at(rcs, "dentry_unused", "dentry_unused",
                     [("include/linux/list.h", 164),
                      ("fs/dcache.c", 465),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)


#--

#BOOKMARK

results = race_on_at(rcs, "pdflush_list.next", "(krxtimod_list.next)->next",
                     [("include/linux/list.h", 223),
                      ("net/rxrpc/krxtimod.c", 164),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)


#6_10 
results = race_on_at(rcs, "next", "next",
                     [("include/linux/list.h", 223),
                      ("net/rxrpc/krxtimod.c", 164)])

print "expecting 0 -- skip"

#--

results = race_on_at(rcs, "audit_freelist.next",
                     "(GlobalSMBSessionList.next)->next",
                     [("include/linux/list.h", 164),
                      ("fs/cifs/connect.c:135", 135),
                      ])
expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)

#6_10 

results = race_on_at_exact(rcs, "next",
                           "next",
                           ("include/linux/list.h", 164),
                           ("fs/cifs/connect.c:135", 135))

print "expecting 0? -- skip"

#--

results = race_on_at(rcs, "(GlobalSMBSessionList.next)->next",
                     "cache_defer_list.next",
                     [("fs/cifs/connect.c", 135),
                      ("include/linux/list.h:164", 164),
                      ])

expect (results, 0)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "_a164_620707_fork", "_a164_620707_fork",
                     [("kernel/fork.c", 1124),
                      ])

expect (results, 0)

if len(results) > 0:
	print results[0].first_race()
	print "\n\n"

for rc in results:
    rc.add_label(initLabel)


#--


results = race_on_at(rcs, "data_len", "data_len",
                     [("include/linux/skbuff.h", 764),
                      ("net/core/skbuff.c", 811),
                      ])

expect (results, 2)

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
expect (results, 0)

#--

#BOOKMARK

results = race_on_at(rcs, "i_op", "i_op",
                     [("fs/inode.c", 121),
                      ])

expect (results, 7)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vaddr", "vaddr",
                     [("net/ipv4/ipvs/ip_vs_conn.c", 614),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "d_parent", "d_parent",
                     [("fs/dcache.c", 200),])

expect (results, 3)

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "d_parent", "d_parent",
                     [("fs/dcache.c", 741),])

expect (results, 6)

for rc in results:
    rc.add_label(initLabel)
    

#--

results = race_on_at(rcs, "virtual", "virtual",
                     [("mm/highmem.c", 543),
                      ("mm/highmem.c", 574),
                      ])


expect (results, 0)

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "last_delta", "last_delta",
                     [("drivers/char/random.c", 602),
                      ("drivers/char/random.c", 603),
                      ])
expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "i_nlink", "i_nlink",
                     [("fs/inode.c", 123),
                      ])
expect (results, 7)

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "mnt_root", "mnt_root",
                     [("fs/dcache.c", 175),])

expect (results, 1)
for rc in results:
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "page_address_pool", "page_address_pool",
                     [("mm/highmem.c", 573),
                      ("mm/highmem.c", 542),
                      ])

expect (results, 0)

for rc in results:
    rc.add_label(uniqueLabel)

#--


results = race_on_at(rcs, "number", "number",
                     [("drivers/pci/hotplug/cpqphp_pci.c", 171),])

expect (results, 3)

for rc in results:
    rc.add_label(initLabel)
    rc.add_label(aliasLabel)

#--


results = race_on_at(rcs, "d_op", "d_op",
                     [("fs/dcache.c", 167),
                      ("fs/dcache.c", 743),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "pointer", None,
                     [("drivers/acpi/utilities/utmisc.c", 502),
                      ("include/asm/string.h", 425),
                      ])

expect (results, 4)

for rc in results:
    rc.add_label(aliasLabel)


#--

results = race_on_at(rcs, None, "pointer", 
                     [("include/asm/string.h", 425),
                      ("drivers/acpi/utilities/utmisc.c", 502),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "vector_irq", "vector_irq",
                     [("arch/i386/kernel/io_apic.c", 1173),
                      ])

expect (results, 2)
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "last_delta2", "last_delta2",
                     [("drivers/char/random.c", 606),
                      ("drivers/char/random.c", 605),
                      ])
expect (results, 2)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "d_flags", "d_flags",
                     [("fs/dcache.c", 175),])

expect (results, 13)

for rc in results:
    rc.add_label(uniqueLabel)

# some may be unlikely alias, but i'm not sure (lots of directory entries from all over -- e.g., from caches, or from task lists)

#--

results = race_on_at(rcs, "i_hash", "i_hash",
                     [("include/linux/list.h", 541),
                      ("include/linux/list.h", 527),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "wasted_size", "wasted_size", # debug
                      [("fs/jffs2/debug.c", 28),
                       ("fs/jffs2/debug.c", 32)])

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "wasted_size", "wasted_size",
                      [("fs/jffs2/gc.c", 1190),         
                       ("fs/jffs2/gc.c", 1140)])

# uses wrong lock for protection?

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)


results = race_on_at(rcs, "wasted_size", "wasted_size",
                      [("fs/jffs2/nodemgmt.c", 542),
                       ("fs/jffs2/debug.c", 28)])

print "expecting ? skip"

#--

results = race_on_at(rcs, "ops", "ops",
                     [("net/socket.c", 519),
                      ])

expect (results, 2)

#two different threads, each given a unique socket

for rc in results:
    rc.add_label(aliasLabel)
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "count", "count",
                     [("lib/idr.c", 307),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)


#--

results = race_on_at(rcs, "crd_infd", "crd_infd",
                     [("init/do_mounts_rd.c", 407),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "nr_hw_segments", "nr_hw_segments",
                     [("block/ll_rw_blk.c", 1288),
                      ])

#skip (a lost lock but... don't know why)

print "expecting ? -- skip"

#--

results = race_on_at(rcs, "hiwater_rss", "hiwater_rss",
                     [("mm/mmap.c", 1660),
                      ])

expect (results, 0)

#LOST A RACE?

#--

results = race_on_at(rcs, "array", "array",
                     [("kernel/sched.c", 1431),
                      ])

expect (results, 2)

results[0].add_label(aliasLabel)
results[1].add_label(initLabel)

#--

results = race_on_at(rcs, "ackr_win_bot", "ackr_win_bot",
                     [("net/rxrpc/call.c", 537),
                      ])

expect (results, 5)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "sync_recv_mesg_maxlen", "sync_recv_mesg_maxlen",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 422),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 422),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(notParLabel)


#--

results = race_on_at(rcs, "irq_2_pin", "irq_2_pin",
                     [("arch/i386/kernel/io_apic.c", 113),
                      ("arch/i386/kernel/io_apic.c", 113),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "mnt_pinned", "mnt_pinned",
                     [("fs/namespace.c", 308),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "sleep_avg", "sleep_avg",
                     [("kernel/sched.c", 763),
                      ("kernel/sched.c", 756),
                      ])

#skip.. ptr arith

print "expecting ? -- skip (ptr arith)"

#--

results = race_on_at(rcs, "timeout", "timeout",
                     [("include/rxrpc/krxtimod.h", 36),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "type", "type",
                     [("drivers/scsi/qla2xxx/qla_gs.c", 1519),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "ackr_pend_cnt", "ackr_pend_cnt",
                     [("net/rxrpc/call.c", 527),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "__totlen", "__totlen",
                     [("fs/jffs2/wbuf.c", 315),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)


#--

results = race_on_at(rcs, "idle_timeout", "idle_timeout",
                     [("net/sunrpc/xprtsock.c", 1212),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "nr_active", "nr_active",
                     [("mm/vmscan.c", 730),
                      ])

# skip

print "expecting ? -- skip"

#--

results = race_on_at(rcs, "sel_start", "sel_start",
                     [("drivers/char/selection.c", 68),
                      ("drivers/char/selection.c", 70),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "bv_len", "bv_len",
                     [("mm/highmem.c", 411),
                      ])

#not sure anymore -- one path gives fresh objs... other path?

print "expecting ? -- skip"

#--

results = race_on_at(rcs, "i_size", "i_size",
                     [("fs/inode.c", 125),
                      ])


expect (results, 5)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "(mq->queue)->boundary_rq",
                     "((tr->blkcore_priv)->rq)->boundary_rq",
                     [("block/elevator.c", 347),
                      ("block/elevator.c", 472),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)


#---------------------------------------------------
#------------------ from 2_5_2007 ------------------

results = race_on_at(rcs, "bv_offset", "bv_offset",
                     [("mm/highmem.c", 412),
                      ])

expect (results, 6)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "collisions", "collisions",
                     [("drivers/net/wireless/airo.c", 2251),
                      ])

expect (results, 1)

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

expect (results, 0)

#--

results = race_on_at(rcs, "ar_hln", "ar_hln",
                     [("net/core/netpoll.c", 430),
                      ])

# lost lock, skip for now

print "expecting 1 -- skipping"

#--

results = race_on_at(rcs, "vc_origin", "vc_origin",
                     [("drivers/char/vt.c", 573),
                      ("drivers/char/vt.c", 572),
                      ])
expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "s_element", "s_element",
                     [("fs/sysfs/dir.c", 48),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vc_origin", "inode",
                     [("mm/slab.c", 2293),
                      ("drivers/char/vt.c", 597),
                      ])

expect (results, 0)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "counter", "counter",
                     [("fs/jbd/journal.c", 361),
                      ])
expect (results, 2)

# uses bit_spin_lock... count as not in parallel for now
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "event", "event",
                     [("fs/inotify.c:251", 251),
                      ])

expect (results, 3)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "tx_bytes", "tx_bytes",
                     [("net/bluetooth/bnep/core.c", 456),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs,  "journal", "swapper_space",
                     [("lib/radix-tree.c", 753),
                      ("lib/radix-tree.c", 273),
                      ])

expect (results, 4)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "mnt_sb", "mnt_sb",
                     [("fs/super.c", 838),
                      ])

expect (results, 11)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vm_mm", "vm_mm",
                     [("mm/hugetlb.c", 315),
                      ("kernel/fork.c", 236),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "rpc_task_id", "rpc_task_id",
                     [("net/sunrpc/sched.c", 787),
                      ])

expect (results, 1)

# may be a bug? race on a monotonic ID variable (what if two get the same?)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "tk_work.data", "tk_work.data",
                     [("net/sunrpc/sched.c", 285),
                      ])

print "expecting 0 -- TODO look at the ones for other fields"

#--

results = race_on_at(rcs, "skbuff_fclone_cache", "skbuff_fclone_cache",
                     [("net/core/skbuff.c", 1798),
                      ])

expect (results, 3)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "vc_pos", "vc_pos",
                     [("drivers/char/vt.c", 534),
                      ])

expect (results, 3)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "mpc_dstapic", "mpc_dstapic",
                     [("arch/i386/kernel/io_apic.c", 1825),
                      ("arch/i386/kernel/io_apic.c", 783),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "queuelist.prev", "queuelist.prev",
                     [("block/ll_rw_blk.c:270", 270),
                      ("include/linux/list.h", 222),
                      ])

print "expecting 2 -- skipped"

#--

results = race_on_at(rcs, "nr_threads", "nr_threads",
                     [("kernel/fork.c", 1139),
                      ("kernel/fork.c", 917),
                      ])
expect (results, 3)

for rc in results:
    rc.add_label(raceLabel)
    
#--

results = race_on_at(rcs, "audit_freelist", "pci_devices",
                     [("include/linux/list.h", 164),
                      ("drivers/pci/search.c", 245),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "parser_state.aml", "vc_origin",
                     [("drivers/acpi/parser/psargs.c", 80),
                      ("drivers/char/vt.c", 597),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "tk_callback", "tk_callback",
                     [("net/sunrpc/sched.c", 622),
                      ("net/sunrpc/sched.c", 610),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(initLabel)
    rc.add_label(uniqueLabel)

#-- 

results = race_on_at(rcs, "fl_type", "fl_type",
                     [("fs/locks.c", 286),
                      ("fs/locks.c", 1102),
                      ])

expect (results, 4)

for rc in results:
    rc.add_label(initLabel)
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "i_mmap_writable", "i_mmap_writable",
                     [("mm/mmap.c", 169),
                      ])

expect (results, 3)
for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "g_cpucache_up", "g_cpucache_up",
                     [("mm/slab.c", 654),
                      ])

expect (results, 0)

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "end", "end",
                     [("net/core/skbuff.c", 658),
                      ])
expect (results, 3)

# supposed to have a ref count of 1... should check why
for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "s_count", "s_count",
                     [("fs/super.c", 172),
                      ])

# atomic_dec_and_lock thing skip

print "expecting 1 -- skipped"

#--

results = race_on_at(rcs, "j_commit_sequence", "j_commit_sequence",
                     [("fs/jbd/commit.c", 828),
                      ])

expect (results, 1)
#racy read for printing

for rc in results:
    rc.add_label(raceLabel)
    

#--

results = race_on_at(rcs, "body_len", "body_len",
                     [("init/initramfs.c", 103),
                      ])
expect (results, 2)
for rc in results:
    rc.add_label(reentLabel)
    
#--

results = race_on_at(rcs, "tinfo.task", "tinfo.task",
                     [("arch/i386/kernel/irq.c", 138),
                      ])

expect (results, 1)
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "fl_owner", "fl_owner",
                     [("fs/locks.c", 226),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "event", "event",
                     [("fs/namespace.c", 137),
                      ("fs/namespace.c", 144),
                      ])
expect (results, 2)

for rc in results:
    rc.add_label(reentLabel)

#--

# pending.signal.sig[0] @ signal.h:182 (may be race if called through asm folks?)

#--

results = race_on_at(rcs, "size_or_mask", "size_or_mask",
                     [("arch/i386/kernel/cpu/mtrr/main.c", 620),
                      ])

expect (results, 2)
for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "fu_list.next", "fu_list.next",
                     [("fs/file_table.c", 98),
                      ])

expect (results, 6)
for rc in results:
    rc.add_label(initLabel)

#results[0] could be lock_kernel

#--

results = race_on_at(rcs, "vc_origin", "inode",
                     [("drivers/char/vt.c", 597),
                      ("mm/slab.c", 2299),
                      ])
expect (results, 0)

for rc in results:
    rc.add_label(aliasLabel)


results = race_on_at(rcs, "inode", "vc_origin", 
                     [("drivers/char/vt.c", 597),
                      ("mm/slab.c", 2299),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(aliasLabel)

#--

results = race_on_at(rcs, "crc_32_tab", "crc_32_tab",
                     [("init/../lib/inflate.c", 1066),
                      ("init/initramfs.c", 409),
                      ])

expect (results, 4)

for rc in results:
    rc.add_label(reentLabel)


#--

results = race_on_at(rcs, "stop_backup_sync", "stop_backup_sync",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 739),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 712),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(raceLabel)

#--
results = race_on_at(rcs, "name_buf", "name_buf",
                     [("init/initramfs.c", 422),
                      ("init/initramfs.c", 424),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(reentLabel)



#-------------------------------------------------
#------------------ from 3_8_2007 (w/ locks) -----


#--
results = race_on_at(rcs, "nr_msix_devices", "nr_msix_devices",
                     [("drivers/pci/msi.c", 497),
                      ])

expect (results, 1)

# race that was fixed in a later version (big changes)

for rc in results:
    rc.add_label(raceLabel)


#--
results = race_on_at(rcs, "cl_lease_time", "cl_lease_time",
                     [("fs/nfs/nfs4proc.c", 2680),
                      ("fs/nfs/nfs4renewd.c", 106),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "t_checkpoint_list", "t_checkpoint_list",
                     [("fs/jbd/checkpoint.c", 593),
                      ("fs/jbd/commit.c", 715),
                      ])

# actual race is with parent thread, not the path given

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "vmlist->size", "vmlist->size",
                     [("mm/vmalloc.c", 277),
                      ("mm/vmalloc.c", 270),
                      ])

expect (results, 1)

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

expect (results, 2)

#for warning if console is unlocked
#(won't have locks on itself in that case)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "nf_bridge", "nf_bridge",
                     [("net/core/skbuff.c", 432),
                      ])

expect (results, 3)

#either gets fresh or reuses old
for rc in results:
    rc.add_label(initLabel)
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "fsm.result", "fsm.result",
                     [("drivers/net/irda/sir_kthread.c", 266),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "b_frozen_data", "b_frozen_data",
                     [("fs/jbd/commit.c", 752),
                      ("fs/jbd/commit.c", 750),
                      ])

expect (results, 2)

#bit spinlock
for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "log_level_unknown", "log_level_unknown",
                     [("kernel/printk.c", 598),
                      ("kernel/printk.c", 604),
                      ])

expect (results, 3)

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "offset", "offset",
                     [("net/rxrpc/call.c", 1288),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(uniqueLabel)



#--

results = race_on_at(rcs, "lowest_bit", "lowest_bit",
                     [("mm/swapfile.c", 259),
                      ])

expect (results, 3)

for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "anon_vma", "anon_vma",
                     [("mm/rmap.c", 106),
                      ])

expect (results, 5)

#(double-checked locking?)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "idr.layers", "idr.layers",
                     [("lib/idr.c", 329),
                      ("lib/idr.c", 337),
                      ])

expect (results, 3)

#(depends on memory consistency?!)
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "nh", "nh",
                     [("net/core/skbuff.c", 395),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "ifi_change", "ifi_change",
                     [("net/core/wireless.c", 1124),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "oops_in_progress", "oops_in_progress",
                     [("arch/i386/mm/fault.c", 42),
                      ("arch/i386/mm/fault.c", 48),
                      ])

expect (results, 3)

# after an oops... bad if you're trying to unoops and oops
# at the same time? then it's an atomicity issue anyway

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "rta_len", "rta_len",
                     [("net/core/rtnetlink.c", 128),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(initLabel)

#--

results = race_on_at(rcs, "cl_lease_time", "cl_lease_time",
                     [("fs/nfs/nfs4proc.c", 2655),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(uniqueLabel)


#--

results = race_on_at(rcs, "h_reclaiming", "h_reclaiming",
                     [("fs/lockd/clntlock.c", 245),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(reentLabel)

#--

results = race_on_at(rcs, "oldx", "oldx",
                     [("drivers/char/vt.c", 461),
                      ])
expect (results, 3)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "shared.vm_set.list.prev", "shared.vm_set.list.prev",
                     [("include/linux/list.h", 223),
                      ])
expect (results, 3)

for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "gc_task", "gc_task",
                     [("fs/jffs2/background.c", 81),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "mc_list", "mc_list",
                     [("net/ipv4/igmp.c", 1148),
                      ("net/ipv4/devinet.c", 123),
                      ])

expect (results, 1)

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

expect (results, 1)

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "logged_chars", "logged_chars",
                     [("kernel/printk.c", 449),
                      ])

expect (results, 3)

for rc in results:
    rc.add_label(reentLabel)


#--

results = race_on_at(rcs, "nextblock", "nextblock",
                     [("fs/jffs2/nodemgmt.c", 264),
                      ])

expect (results, 1)

# uses alloc_sem sometimes, c->erase_completion_lock sometimes
# (may not have a common lock all the time?)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "pidmap_array.page", "pidmap_array.page",
                     [("kernel/pid.c", 96),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "printk_buf", "printk_buf",
                     [("kernel/printk.c", 554),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(reentLabel)

#--
results = race_on_at(rcs, "irq_desc.status", "irq_desc.status",
                     [("kernel/irq/manage.c", 42),
                      ])

expect (results, 1)

# (racy read used for sync?)

for rc in results:
    rc.add_label(raceLabel)

#--
results = race_on_at(rcs, "thread_finished", "thread_finished",
                     [("drivers/pci/hotplug/cpci_hotplug_core.c", 584),
                      ])

expect (results, 4)

# one of those flags set for killing a thread

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "swap_list.next", "swap_list.next",
                     [("mm/swapfile.c", 263),
                      ])
expect (results, 5)

for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "audit_skb_queue.qlen", "audit_skb_queue.qlen",
                     [("include/linux/skbuff.h", 644),
                      ("include/linux/skbuff.h", 590)
                      ])
expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)


#--
results = race_on_at(rcs, "swap_token_mm", "swap_token_mm",
                     [("include/linux/swap.h", 244),
                      ])

expect (results, 4) 

#(double-checked locking)

for rc in results:
    rc.add_label(raceLabel)


#--
results = race_on_at(rcs, "nr_skbs", "nr_skbs",
                     [("net/core/netpoll.c", 220),
                      ])
expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "swap_token_check", "swap_token_check",
                     [("mm/thrash.c", 68),
                      ])

expect (results, 3)

# (usually not reachable, but if a timeout val is set, it will happen)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "nr_free_blocks", "nr_free_blocks",
                     [("fs/jffs2/gc.c", 1087),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "j_revoke", "j_revoke",
                     [("fs/jbd/revoke.c", 515),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "(tr->blkcore_priv)->rq", "(tr->blkcore_priv)->rq",
                     [("drivers/mtd/mtd_blkdevs.c", 80),
                      ])
expect (results, 1)

for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "surprise_rm_pending", "surprise_rm_pending",
                     [("drivers/pci/hotplug/pciehp_ctrl.c", 476),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "nr_reserved_vectors", "nr_reserved_vectors",
                     [("drivers/pci/msi.c", 499),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "j_flags", "j_flags",
                     [("fs/jbd/commit.c", 198),
                      ("fs/jbd/journal.c", 1460),
                      ])

expect (results, 1)

#should use atomic read/write bitvector funcs?

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "gcblock", "gcblock",
                     [("fs/jffs2/gc.c", 511),
                      ("fs/jffs2/gc.c", 1134),
                      ("fs/jffs2/gc.c", 1184),                      
                      ])

expect (results, 1)

# race (not sure of severity)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "(journal->j_superblock)->s_start",
                     "(journal->j_superblock)->s_start",
                     [("fs/jbd/journal.c", 937),
                      ])
expect (results, 1)

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
expect (results, 1)

#for debug

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "(jffs2_compressor_list.next)->compr_buf",
                     "(jffs2_compressor_list.next)->compr_buf",
                     [("fs/jffs2/compr.c", 104),
                      ("fs/jffs2/compr.c", 126),
                      ])

expect (results, 1)

# I think it may be a bug (from trying to free null?)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "rt_deadline", "rt_deadline",
                     [("net/ipv4/route.c", 681),
                      ("net/ipv4/route.c", 715),
                      ])

expect (results, 2) 

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "unchecked_size", "unchecked_size",
                     [("fs/jffs2/debug.c", 38),
                      ("fs/jffs2/debug.c", 41),
                      ])
expect (results, 1)

# for debugging

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "none_stat_decompr_blocks",
                     "none_stat_decompr_blocks",
                     [("fs/jffs2/compr.c", 182),
                      ("fs/jffs2/gc.c", 747),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(notParLabel)


#--

results = race_on_at(rcs, "none_stat_compr_blocks",
                     "none_stat_compr_blocks",
                     [("fs/jffs2/compr.c", 157),
                      ("fs/jffs2/gc.c", 1267),
                      ])

expect (results, 1)

for rc in results:
    rc.add_label(notParLabel)


#--

results = race_on_at(rcs, "softcursor_original", "softcursor_original",
                     [("drivers/char/vt.c", 520),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "(audit_freelist.next)->gfp_mask",
                     "(audit_freelist.next)->gfp_mask",
                     [("kernel/audit.c", 591),
                      ])

expect (results, 1)

#(custom free list)

for rc in results:
    rc.add_label(initLabel)

#--


results = race_on_at(rcs, "mem_map->mapping", "mem_map->mapping",
                     [("mm/page_alloc.c", 129),
                      ("fs/buffer.c", 848)])

expect (results, 1)

#(maybe race... comment said so)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "console_sem.wait.task_list.next",
                     "console_sem.wait.task_list.next",
                     [("include/linux/wait.h", 84),
                      ])

expect (results, 1)

# how severe?

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "socket->socket", "socket->socket",
                     [("drivers/pcmcia/cs.c", 649),
                      ("drivers/pcmcia/cs.c", 324),
                      ])
expect (results, 1)

#(idempotent)

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "((irda_rq_queue.request_list.next)->data)->fsm.state", "((irda_rq_queue.request_list.next)->data)->fsm.state",
                     [("drivers/net/irda/sir_kthread.c", 413),
                      ("drivers/net/irda/sir_kthread.c", 254),
                      ])
expect (results, 2)
for rc in results:
    rc.add_label(uniqueLabel)

#--

results = race_on_at(rcs, "((irda_rq_queue.request_list.next)->data)->fsm.param", "((irda_rq_queue.request_list.next)->data)->fsm.param",
                     [("drivers/net/irda/sir_kthread.c", 290),
                      ])
expect (results, 2)
for rc in results:
    rc.add_label(uniqueLabel)
    
#--

results = race_on_at(rcs, "swap_info[0].highest_bit",
                     "swap_info[0].highest_bit",
                     [("mm/swapfile.c", 261),
                      ("mm/swapfile.c", 260),
                      ])

expect (results, 3)
for rc in results:
    rc.add_label(condLabel)

#--

results = race_on_at(rcs, "stop_backup_sync", "stop_backup_sync",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 739),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 742),
                      ])

expect (results, 2)

#(thread stop flag)

for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "swap_cache_info.exist_race",
                     "swap_cache_info.exist_race",
                     [("mm/swap_state.c", 110),
                      ])

expect (results, 2) 

#stat variable (to count races... haha)

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "audit_backlog_wait_time",
                     "audit_backlog_wait_time",
                     [("kernel/audit.c", 695),
                      ])

expect (results, 3)

# always set to the same var X, and X is always 0?
for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "sync_send_mesg_maxlen", "sync_send_mesg_maxlen",
                     [("net/ipv4/ipvs/ip_vs_sync.c", 414),
                      ("net/ipv4/ipvs/ip_vs_sync.c", 414),
                      ])

expect (results, 2)

for rc in results:
    rc.add_label(notParLabel)

#--

results = race_on_at(rcs, "console_printk[0]", "console_printk[0]",
                     [("arch/i386/mm/fault.c", 54),
                      ])

expect (results, 3) 

#during panic / when unbusting spinlocks

for rc in results:
    rc.add_label(raceLabel)


#--

results = race_on_at(rcs, "shift_down[0]", "shift_down[0]",
                     [("drivers/char/keyboard.c", 384),
                      ])

expect (results, 1)

#(during panic)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "panic_blink", "panic_blink",
                     [("kernel/panic.c", 99),
                      ])

expect (results, 2)

#(during panic)
for rc in results:
    rc.add_label(raceLabel)

#--

results = race_on_at(rcs, "input_timer_state.last_time",
                     "input_timer_state.last_time",
                     [("drivers/char/random.c", 600),
                      ("drivers/char/random.c", 599),
                      ])

expect (results, 2)

#(benign -- for randomizer)
for rc in results:
    rc.add_label(raceLabel)

#--

#results = race_on_at(rcs, "XXX", "XXX",
#                     [("YYY", 277),
#                      ("YYY", 270),
#                      ])
