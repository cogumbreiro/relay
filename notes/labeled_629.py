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


#--- For the 6_29_2007 run ---
r = Run.objects.get(id=20)
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


from relay.warnings.labeled import *

# what is considered a search term?
#      1) a tuple of two lval strings and a list of locations
# OR   2) a tuple of two lval strings and two locations
# locations are a pair of filename and line number

# refactoring stuff
# 1)
#    results = race_on_at(rcs, \(.*\), \(.*\), \(\[.*\]\))
#     -> 
#    searchTerm = (\1, \2, \3)
#    tempRes = race_on_search(rcs, searchTerm)

# 2)
#    results = race_on_at_exact(rcs, \(.*\), \(.*\), \(.*\), \(.*\))
#     ->
#    searchTerm = (\1, \2, \3, \4)
#    tempRes = race_on_search_exact(rcs, searchTerm)

#
# Refactor
# expect (results, \(.*\))
#
# for rc in results:
#     rc.add_label(\(.*\))
# -> 
#
# results.addResult(tempRes, \2, \1, searchTerm)
#

results = ResultTable ({})

#---------------------------------------------------------
#------------------ from 1_30_2007 notes -----------------

#--

searchTerm = ("shift_state", "shift_state", ImmutableList([("drivers/char/keyboard.c", 385), ("drivers/char/keyboard.c", 361)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)


#--

searchTerm = ("app_abort_code", "app_abort_code", ("net/rxrpc/call.c", 102), ("net/rxrpc/call.c", 889))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2, searchTerm)

#--

searchTerm = ("tk_calldata", "tk_calldata", ImmutableList([("net/sunrpc/sched.c", 285), ("net/sunrpc/sched.c", 898)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, notParLabel, 2, searchTerm) # custom mutex

results.addResult(tempRes, initLabel, 2, searchTerm)



#--

# HMMM... the other results globbed in from this might 
# not be a race (the racey read is from debug.c, but these are the 
# corresponding writes... check again later)


#searchTerm = ("wasted_size", "wasted_size", ImmutableList([("fs/jffs2/nodemgmt.c", 176), ("fs/jffs2/nodemgmt.c", 329), ("fs/jffs2/wbuf.c", 162)]))
#tempRes = race_on_search(rcs, searchTerm)

#results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("cl_dentry", "cl_dentry", ImmutableList([("net/sunrpc/clnt.c", 81), ("net/sunrpc/clnt.c", 82)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)

#--

searchTerm = ("mnt_parent", "mnt_parent", ImmutableList([("fs/namespace.c", 683), ("fs/super.c", 841)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

#, ("net/core/skbuff.c", 860))

searchTerm = ("end", "end", ImmutableList([("net/core/skbuff.c", 658)]))
tempRes = race_on_search(rcs, searchTerm)

# access at line 658 is unique, 451 is init ... they come as a pair (merged)
results.addResult(tempRes, uniqueLabel, 2, searchTerm)
results.addResult(tempRes, initLabel, 2, searchTerm)


#--

searchTerm = ("i_state", "i_state", ("fs/fs-writeback.c", 100), ("fs/fs-writeback.c", 75))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)


#searchTerm = ("i_state", "i_state", ImmutableList([("fs/fs-writeback.c", 100), ("fs/fs-writeback.c", 75)]))
#tempRes = race_on_search(rcs, searchTerm)

#expect (results, 2)

#10 total
#benign race -- idempotent

#0 is benign
#results[0].add_label(raceLabel)
#results[1].add_label(raceLabel)

#2, 3, 4, ... skip (but look like they're benign too... 
#don't want to artificially inflate)


#--

searchTerm = ("i_security", "i_security", ImmutableList([("fs/inode.c", 136)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)


#--

searchTerm = ("nr_sectors", "nr_sectors", ("block/ll_rw_blk.c", 1298), ("block/ll_rw_blk.c", 2681))
tempRes = race_on_search_exact(rcs, searchTerm)
    
results.addResult(tempRes, initLabel, 3, searchTerm)

#--

searchTerm = ("vm86_irqs[0].tsk", "vm86_irqs[0].tsk", ImmutableList([("arch/i386/kernel/vm86.c", 739), ("arch/i386/kernel/vm86.c", 750)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 0, searchTerm)


#--

searchTerm = ("(c->gc_task)->prio", "((pdflush_list.next)->who)->prio", ImmutableList([("kernel/sched.c", 819), ("kernel/sched.c", 609), ("kernel/sched.c", 610), ("kernel/sched.c", 650), ("kernel/sched.c", 651), ("kernel/sched.c", 677), ("kernel/sched.c", 684), ("kernel/sched.c", 818), ("kernel/sched.c", 1384)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, aliasLabel, 1, searchTerm)

#--


#searchTerm = ("number", "number", ImmutableList([("drivers/pci/probe.c", 359), ("drivers/pci/probe.c", 741)]))
#tempRes = race_on_search(rcs, searchTerm)

#expect (results, 4)


#0 is init (for 6_10_2007), 1 not, ...

#results[0].add_label(initLabel)
#results[2].add_label(initLabel)
#results[3].add_label(initLabel)
#results[4].add_label(initLabel)

searchTerm = ("number", "number", ("drivers/pci/probe.c", 741), ("drivers/pci/probe.c", 359))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)



#--

searchTerm = ("array", "array", ("kernel/sched.c", 1258), ("kernel/sched.c", 1431))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)

#2 total, but first result can also be unlikely aliasing?

#--

#searchTerm = ("df_list.next", "(ibmphp_slot_head.next)->next", ImmutableList([("include/linux/list.h", 313), ("drivers/pci/hotplug/ibmphp_hpc.c", 845)]))
#tempRes = race_on_search(rcs, searchTerm)


searchTerm = ("next", "next", ("include/linux/list.h", 313), ("drivers/pci/hotplug/ibmphp_hpc.c", 845))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)


#--

searchTerm = ("stime", "stime", ImmutableList([("kernel/fork.c", 946), ("kernel/posix-cpu-timers.c", 462)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)

#--


searchTerm = ("proc_inum_idr.id_free_cnt", "proc_inum_idr.id_free_cnt", ImmutableList([("lib/idr.c", 59), ("lib/idr.c", 77)]))
tempRes = race_on_search(rcs, searchTerm)

#REASON: RACE but designed that way (detects and retries)

results.addResult(tempRes, raceLabel, 1, searchTerm)


#--

searchTerm = ("i_bdev", "i_bdev", ("fs/inode.c", 133), ("fs/inode.c", 261))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)


#--

searchTerm = ("exit_signal", "exit_signal", ImmutableList([("kernel/fork.c", 1026)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

searchTerm = ("assoc_mapping", "assoc_mapping", ImmutableList([("fs/inode.c", 150)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)

#--

searchTerm = ("utime", "utime", ImmutableList([("kernel/fork.c", 945)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)

#--

searchTerm = ("real_parent", "real_parent", ("kernel/fork.c", 1073), ("kernel/fork.c", 1070))
tempRes = race_on_search_exact(rcs, searchTerm)

# expect (results, 0)

#turns out both accesses have lock...

#--

# It's not there anymore because dentry_unused is now a REP_NODE... 
# locks shouldn't be REP_NODEs though... (a global like dcache_lock)

searchTerm = ("dentry_unused", "dentry_unused", ImmutableList([("include/linux/list.h", 164), ("fs/dcache.c", 465)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)


#--

searchTerm = ("next", "next", ("include/linux/list.h", 223), ("net/rxrpc/krxtimod.c", 164))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)


#6_10 
searchTerm = ("next", "next", ImmutableList([("include/linux/list.h", 223), ("net/rxrpc/krxtimod.c", 164)]))
tempRes = race_on_search(rcs, searchTerm)

# returns 24 things now...

print "expecting 0 -- skip\n"

#--

searchTerm = ("next", "next", ("include/linux/list.h", 164), ("fs/cifs/connect.c:135", 135))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)

#6_10 

#--

searchTerm = ("(GlobalSMBSessionList.next)->next", "cache_defer_list.next", ImmutableList([("fs/cifs/connect.c", 135), ("include/linux/list.h:164", 164)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 0, searchTerm)

#--

searchTerm = ("_a164_620707_fork", "_a164_620707_fork", ImmutableList([("kernel/fork.c", 1124)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)


#--


searchTerm = ("data_len", "data_len", ImmutableList([("net/core/skbuff.c", 811)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)

# for 6_10, 3 total ... one of them is unlikely alias too? 
# allocation comes from different call path 
# (didn't tag that -- should be middle one)


#--

searchTerm = ("per_cpu__sockets_in_use", "per_cpu__sockets_in_use", ImmutableList([("net/socket.c", 526), ("net/socket.c", 483)]))
tempRes = race_on_search(rcs, searchTerm)

#?
# expect (results, 0)

#--

searchTerm = ("i_op", "i_op", ImmutableList([("fs/inode.c", 121)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 7, searchTerm)

#--

searchTerm = ("vaddr", "vaddr", ImmutableList([("net/ipv4/ipvs/ip_vs_conn.c", 614)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

searchTerm = ("d_parent", "d_parent", ImmutableList([("fs/dcache.c", 200),]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 3, searchTerm)

#--

searchTerm = ("d_parent", "d_parent", ImmutableList([("fs/dcache.c", 741)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 6, searchTerm)
  

#--

searchTerm = ("virtual", "virtual", ImmutableList([("mm/highmem.c", 543), ("mm/highmem.c", 574)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, uniqueLabel, 0, searchTerm)

#--

searchTerm = ("last_delta", "last_delta", ImmutableList([("drivers/char/random.c", 602), ("drivers/char/random.c", 603)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#shows two races now (one for 602+603, other for 603+603)

#--

searchTerm = ("i_nlink", "i_nlink", ImmutableList([("fs/inode.c", 123)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 7, searchTerm)

#14 now...

#--

#6_24_2007 changed from "mnt_root" to "d_flags" (as that's the real field name)

searchTerm = ("d_flags", "d_flags", ImmutableList([("fs/dcache.c", 175),]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, uniqueLabel, 1, searchTerm)


#--

searchTerm = ("virtual", "virtual", ImmutableList([("mm/highmem.c", 573)]))
tempRes = race_on_search(rcs, searchTerm)

# in transition between lists

results.addResult(tempRes, uniqueLabel, 0, searchTerm)

#--

#BOOKMARK

searchTerm = ("number", "number", ImmutableList([("drivers/pci/hotplug/cpqphp_pci.c", 171)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 3, searchTerm)
    
#rc.add_label(aliasLabel)

#--


searchTerm = ("d_op", "d_op", ImmutableList([("fs/dcache.c", 743)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)


#--

searchTerm = ("pointer", "vc_pos", ImmutableList([("drivers/acpi/utilities/utmisc.c", 502), ("drivers/char/vt.c", 534)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 4, searchTerm)


#--

searchTerm = (None, "pointer", ImmutableList([("include/asm/string.h", 425), ("drivers/acpi/utilities/utmisc.c", 502)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 2, searchTerm)

#--

searchTerm = ("vector_irq", "vector_irq", ImmutableList([("arch/i386/kernel/io_apic.c", 1173)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

searchTerm = ("last_delta2", "last_delta2", ImmutableList([("drivers/char/random.c", 606), ("drivers/char/random.c", 605)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--

searchTerm = ("d_flags", "d_flags", ImmutableList([("fs/dcache.c", 175)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 13, searchTerm)

# some may be unlikely alias, but i'm not sure (lots of directory entries from all over -- e.g., from caches, or from task lists)

#--

searchTerm = ("i_hash", "i_hash", ("include/linux/list.h", 541), ("include/linux/list.h", 527))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)

#--

# racey read for debug
searchTerm = ("wasted_size", "wasted_size", ImmutableList([("fs/jffs2/debug.c", 28), ("fs/jffs2/debug.c", 32)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

# uses wrong lock for protection?
searchTerm = ("wasted_size", "wasted_size", ImmutableList([("fs/jffs2/gc.c", 1190), ("fs/jffs2/gc.c", 1140)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

# racey read for debug
searchTerm = ("wasted_size", "wasted_size", ImmutableList([("fs/jffs2/debug.c", 28)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

#two different threads, each given a unique socket?

searchTerm = ("ops", "ops", ImmutableList([("net/socket.c", 519)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 2, searchTerm)


#--

searchTerm = ("count", "count", ImmutableList([("lib/idr.c", 307)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)


#--

searchTerm = ("crd_infd", "crd_infd", ImmutableList([("init/do_mounts_rd.c", 407)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

searchTerm = ("nr_hw_segments", "nr_hw_segments", ImmutableList([("block/ll_rw_blk.c", 1288)]))
tempRes = race_on_search(rcs, searchTerm)

#skip (a lost lock but... don't know why)

print "expecting ? -- skip\n"

#--

searchTerm = ("hiwater_rss", "hiwater_rss", ImmutableList([("mm/mmap.c", 1660)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 0, searchTerm)

#LOST A RACE?

#--

#searchTerm = ("array", "array", ImmutableList([("kernel/sched.c", 1431)]))
#tempRes = race_on_search(rcs, searchTerm)

#expect (results, 2)

#results[0].add_label(uniqueLabel)
#results[1].add_label(initLabel)

searchTerm = ("array", "array", ("kernel/sched.c", 1431), ("kernel/sched.c", 1431))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 1, searchTerm)


#--

searchTerm = ("ackr_win_bot", "ackr_win_bot", ImmutableList([("net/rxrpc/call.c", 537)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 5, searchTerm)

#--

searchTerm = ("sync_recv_mesg_maxlen", "sync_recv_mesg_maxlen", ImmutableList([("net/ipv4/ipvs/ip_vs_sync.c", 422), ("net/ipv4/ipvs/ip_vs_sync.c", 422)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2, searchTerm)


#--

searchTerm = ("pin", "pin", ImmutableList([("arch/i386/kernel/io_apic.c", 113), ("arch/i386/kernel/io_apic.c", 113)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

searchTerm = ("mnt_pinned", "mnt_pinned", ImmutableList([("fs/namespace.c", 308)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, condLabel, 2, searchTerm)

#--

searchTerm = ("sleep_avg", "sleep_avg", ImmutableList([("kernel/sched.c", 763), ("kernel/sched.c", 756)]))
tempRes = race_on_search(rcs, searchTerm)

#skip.. ptr arith

print "expecting ? -- skip (ptr arith)\n"

#--

searchTerm = ("timeout", "timeout", ImmutableList([("include/rxrpc/krxtimod.h", 36)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

searchTerm = ("type", "type", ImmutableList([("drivers/scsi/qla2xxx/qla_gs.c", 1519)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 1, searchTerm)

#--

searchTerm = ("ackr_pend_cnt", "ackr_pend_cnt", ImmutableList([("net/rxrpc/call.c", 527)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)


#--

searchTerm = ("__totlen", "__totlen", ImmutableList([("fs/jffs2/wbuf.c", 315)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)


#--

searchTerm = ("idle_timeout", "idle_timeout", ImmutableList([("net/sunrpc/xprtsock.c", 1212)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)

#--

searchTerm = ("nr_active", "nr_active", ImmutableList([("mm/vmscan.c", 730)]))
tempRes = race_on_search(rcs, searchTerm)

# skip

print "expecting ? -- skip\n"

#--

searchTerm = ("sel_start", "sel_start", ImmutableList([("drivers/char/selection.c", 68), ("drivers/char/selection.c", 70)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("bv_len", "bv_len", ImmutableList([("mm/highmem.c", 411)]))
tempRes = race_on_search(rcs, searchTerm)

#not sure anymore -- one path gives fresh objs... other path?

print "expecting ? -- skip\n"

#--

searchTerm = ("i_size", "i_size", ImmutableList([("fs/inode.c", 125)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, initLabel, 5, searchTerm)

#--

searchTerm = ("boundary_rq", "boundary_rq", ("block/elevator.c", 347), ("block/elevator.c", 472))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)




#---------------------------------------------------
#------------------ from 2_5_2007 ------------------


searchTerm = ("bv_offset", "bv_offset", ImmutableList([("mm/highmem.c", 412)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 6, searchTerm)

#--

searchTerm = ("collisions", "collisions", ImmutableList([("drivers/net/wireless/airo.c", 2251)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("(pgd_cache->array[0])->avail", "((uid_cachep->nodelists[0])->shared)->avail", ImmutableList([("mm/slab.c", 2786), ("mm/slab.c", 2699)]))
tempRes = race_on_search(rcs, searchTerm)

# not sure what to say about them anymore, but others seem to
# be unlikely aliases

# expect (results, 0)

#--

searchTerm = ("ar_hln", "ar_hln", ImmutableList([("net/core/netpoll.c", 430)]))
tempRes = race_on_search(rcs, searchTerm)

# lost lock, skip for now

print "expecting 1 -- skipping\n"

#--

searchTerm = ("vc_origin", "vc_origin", ImmutableList([("drivers/char/vt.c", 573), ("drivers/char/vt.c", 572)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("s_element", "s_element", ImmutableList([("fs/sysfs/dir.c", 48)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

searchTerm = ("vc_origin", "inode", ImmutableList([("mm/slab.c", 2293), ("drivers/char/vt.c", 597)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 0, searchTerm)

#--

# uses bit_spin_lock... count as not in parallel for now
searchTerm = ("counter", "counter", ImmutableList([("fs/jbd/journal.c", 361)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2 ,searchTerm)

#--

searchTerm = ("len", "len", ImmutableList([("fs/inotify.c", 251)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 3, searchTerm)

#--

searchTerm = ("tx_bytes", "tx_bytes", ImmutableList([("net/bluetooth/bnep/core.c", 456)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 1, searchTerm)

#--

searchTerm = ( "count", "count", ImmutableList([("lib/radix-tree.c", 753), ("lib/radix-tree.c", 273)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 4, searchTerm)

#--

searchTerm = ("mnt_sb", "mnt_sb", ImmutableList([("fs/super.c", 838)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 11, searchTerm)

#--

searchTerm = ("vm_mm", "vm_mm", ImmutableList([("mm/hugetlb.c", 315), ("kernel/fork.c", 236)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

# may be a bug? race on a monotonic ID variable (what if two get the same?)
searchTerm = ("rpc_task_id", "rpc_task_id", ("net/sunrpc/sched.c", 787), ("net/sunrpc/sched.c", 787))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("tk_work.data", "tk_work.data", ImmutableList([("net/sunrpc/sched.c", 285)]))
tempRes = race_on_search(rcs, searchTerm)


print "expecting 0 -- TODO look at the ones for other fields\n"

#--

searchTerm = ("skbuff_fclone_cache", "skbuff_fclone_cache", ImmutableList([("net/core/skbuff.c", 1798)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 3, searchTerm)

#--

searchTerm = ("vc_pos", "vc_pos", ImmutableList([("drivers/char/vt.c", 534)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)

#--

searchTerm = ("mpc_dstapic", "mpc_dstapic", ImmutableList([("arch/i386/kernel/io_apic.c", 1825), ("arch/i386/kernel/io_apic.c", 783)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

searchTerm = ("queuelist.prev", "queuelist.prev", ImmutableList([("block/ll_rw_blk.c:270", 270), ("include/linux/list.h", 222)]))
tempRes = race_on_search(rcs, searchTerm)

print "expecting 2 -- skipped\n"

#--

searchTerm = ("nr_threads", "nr_threads", ImmutableList([("kernel/fork.c", 1139), ("kernel/fork.c", 917)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)
    
#--

searchTerm = ("next", "next", ("include/linux/list.h", 164), ("drivers/pci/search.c", 245))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)

#--

searchTerm = ("vc_origin", "parser_state.aml", ImmutableList([("drivers/acpi/parser/psargs.c", 80), ("drivers/char/vt.c", 597)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 1, searchTerm)

#--

# removed from list in some contexts, fresh memory in others

searchTerm = ("tk_callback", "tk_callback", ImmutableList([("net/sunrpc/sched.c", 622), ("net/sunrpc/sched.c", 610)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, searchTerm)
results.addResult(tempRes, uniqueLabel, 2, searchTerm)

#-- 

searchTerm = ("fl_type", "fl_type", ImmutableList([("fs/locks.c", 286), ("fs/locks.c", 1102)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 3, searchTerm)
results.addResult(tempRes, reentLabel, 3, searchTerm)

#--

searchTerm = ("i_mmap_writable", "i_mmap_writable", ImmutableList([("mm/mmap.c", 169)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, condLabel, 3, searchTerm)

#--

searchTerm = ("g_cpucache_up", "g_cpucache_up", ImmutableList([("mm/slab.c", 654)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 0, searchTerm)

#--

# supposed to have a ref count of 1... should check why

searchTerm = ("end", "end", ImmutableList([("net/core/skbuff.c", 658)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 3, searchTerm)

#--

searchTerm = ("s_count", "s_count", ImmutableList([("fs/super.c", 172)]))
tempRes = race_on_search(rcs, searchTerm)

# atomic_dec_and_lock thing skip

print "expecting 1 -- skipped\n"

#--

#racy read for printing

searchTerm = ("j_commit_sequence", "j_commit_sequence", ImmutableList([("fs/jbd/commit.c", 828)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)    

#--

searchTerm = ("body_len", "body_len", ImmutableList([("init/initramfs.c", 103)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

searchTerm = ("tinfo.task", "tinfo.task", ImmutableList([("arch/i386/kernel/irq.c", 138)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, reentLabel, 1, searchTerm)

#--

searchTerm = ("fl_owner", "fl_owner", ImmutableList([("fs/locks.c", 226)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

searchTerm = ("event", "event", ImmutableList([("fs/namespace.c", 137), ("fs/namespace.c", 144)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

# pending.signal.sig[0] @ signal.h:182 (may be race if called through asm folks?)

#--

searchTerm = ("size_or_mask", "size_or_mask", ImmutableList([("arch/i386/kernel/cpu/mtrr/main.c", 620)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

searchTerm = ("fu_list.next", "fu_list.next", ImmutableList([("fs/file_table.c", 98)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 6, searchTerm)

#results[0] could be lock_kernel

#--

searchTerm = ("inode", "vc_origin", ("mm/slab.c", 2299), ("drivers/char/vt.c", 597))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, aliasLabel, 2, searchTerm)

# no longer there... it was *(blah...inode), which is now a REP node

#--

searchTerm = ("crc_32_tab", "crc_32_tab", ImmutableList([("init/../lib/inflate.c", 1066), ("init/initramfs.c", 409)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 4, searchTerm)


#--

searchTerm = ("stop_backup_sync", "stop_backup_sync", ImmutableList([("net/ipv4/ipvs/ip_vs_sync.c", 739), ("net/ipv4/ipvs/ip_vs_sync.c", 712)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--
searchTerm = ("name_buf", "name_buf", ImmutableList([("init/initramfs.c", 422), ("init/initramfs.c", 424)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)



#-------------------------------------------------
#------------------ from 3_8_2007 (w/ locks) -----


#--

# race that was fixed in a later version (big changes)
searchTerm = ("nr_msix_devices", "nr_msix_devices", ImmutableList([("drivers/pci/msi.c", 497)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--
searchTerm = ("cl_lease_time", "cl_lease_time", ImmutableList([("fs/nfs/nfs4proc.c", 2680), ("fs/nfs/nfs4renewd.c", 106)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 1, searchTerm)

#--

searchTerm = ("t_checkpoint_list", "t_checkpoint_list", ImmutableList([("fs/jbd/checkpoint.c", 593), ("fs/jbd/commit.c", 715)]))
tempRes = race_on_search(rcs, searchTerm)

# actual race is with parent thread, not the path given

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("size", "size", ("mm/vmalloc.c", 277), ("mm/ioremap.c", 270))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("((*(c->inocache_list)))->state", "((*(c->inocache_list)))->state", ImmutableList([("fs/jffs2/gc.c", 194), ("fs/jffs2/gc.c", 167)]))
tempRes = race_on_search(rcs, searchTerm)

#lost lock from switch stmt? skip?
print "skipping a 'lost lock'\n"

#--

#for warning if console is unlocked
#(won't have locks on itself in that case)

searchTerm = ("console_locked", "console_locked", ImmutableList([("kernel/printk.c", 731), ("kernel/printk.c", 619)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--

#either gets fresh or reuses old
searchTerm = ("nf_bridge", "nf_bridge", ImmutableList([("net/core/skbuff.c", 432)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 2, initLabel)
results.addResult(tempRes, initLabel, 2, uniqueLabel)

#--

searchTerm = ("fsm.result", "fsm.result", ImmutableList([("drivers/net/irda/sir_kthread.c", 266)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 2, searchTerm)


#--

#bit spinlock
searchTerm = ("b_frozen_data", "b_frozen_data", ImmutableList([("fs/jbd/commit.c", 752), ("fs/jbd/commit.c", 750)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2, searchTerm)

#--

searchTerm = ("log_level_unknown", "log_level_unknown", ImmutableList([("kernel/printk.c", 598), ("kernel/printk.c", 604)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 3, searchTerm)

#--

searchTerm = ("offset", "offset", ImmutableList([("net/rxrpc/call.c", 1288)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 2, searchTerm)

#--

searchTerm = ("lowest_bit", "lowest_bit", ImmutableList([("mm/swapfile.c", 259)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, condLabel, 3, searchTerm)

#--

#(double-checked locking?)

searchTerm = ("anon_vma", "anon_vma", ImmutableList([("mm/rmap.c", 106)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 5, searchTerm)


#--

#(depends on memory consistency?!)

searchTerm = ("idr.layers", "idr.layers", ImmutableList([("lib/idr.c", 329),("lib/idr.c", 337)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, raceLabel, 3, searchTerm)


#--

searchTerm = ("nh", "nh", ImmutableList([("net/core/skbuff.c", 395)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

searchTerm = ("ifi_change", "ifi_change", ImmutableList([("net/core/wireless.c", 1124)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

# after an oops... bad if you're trying to unoops and oops
# at the same time? then it's an atomicity issue anyway

searchTerm = ("oops_in_progress", "oops_in_progress", ImmutableList([("arch/i386/mm/fault.c", 42), ("arch/i386/mm/fault.c", 48)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)

#--

searchTerm = ("rta_len", "rta_len", ImmutableList([("net/core/rtnetlink.c", 128)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 1, searchTerm)

#--

searchTerm = ("cl_lease_time", "cl_lease_time", ImmutableList([("fs/nfs/nfs4proc.c", 2655)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 1, searchTerm)


#--

searchTerm = ("h_reclaiming", "h_reclaiming", ImmutableList([("fs/lockd/clntlock.c", 245)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 1, searchTerm)

#--

searchTerm = ("oldx", "oldx", ImmutableList([("drivers/char/vt.c", 461)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)

#--

searchTerm = ("shared.vm_set.list.prev", "shared.vm_set.list.prev", ImmutableList([("include/linux/list.h", 223)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, condLabel, 3, searchTerm)

#--

searchTerm = ("gc_task", "gc_task", ImmutableList([("fs/jffs2/background.c", 81)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2, searchTerm)

#--

# (conditional lock / unique / overlapping locksets)
searchTerm = ("mc_list", "mc_list", ImmutableList([("net/ipv4/igmp.c", 1148), ("net/ipv4/devinet.c", 123)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, condLabel, 1, searchTerm)
results.addResult(tempRes, uniqueLabel, 1, searchTerm)

#--

searchTerm = ("proto_filter", "proto_filter", ImmutableList([("net/bluetooth/bnep/core.c", 121), ("net/bluetooth/bnep/core.c", 124), ("net/bluetooth/bnep/core.c", 127)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 1, searchTerm)

#--

searchTerm = ("logged_chars", "logged_chars", ImmutableList([("kernel/printk.c", 449)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 3, searchTerm)


#--

# uses alloc_sem sometimes, c->erase_completion_lock sometimes
# (may not have a common lock all the time?)

searchTerm = ("nextblock", "nextblock", ImmutableList([("fs/jffs2/nodemgmt.c", 264)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("pidmap_array.page", "pidmap_array.page", ImmutableList([("kernel/pid.c", 96)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--

searchTerm = ("printk_buf", "printk_buf", ImmutableList([("kernel/printk.c", 554)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, reentLabel, 2, searchTerm)

#--

# (racy read used for sync?)

searchTerm = ("irq_desc.status", "irq_desc.status", ImmutableList([("kernel/irq/manage.c", 42)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, raceLabel, 1, searchTerm)

#--


# one of those flags set for killing a thread

searchTerm = ("thread_finished", "thread_finished", ImmutableList([("drivers/pci/hotplug/cpci_hotplug_core.c", 584)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 4, searchTerm)


#--

searchTerm = ("swap_list.next", "swap_list.next", ImmutableList([("mm/swapfile.c", 263)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, condLabel, 5, searchTerm)

#--

searchTerm = ("audit_skb_queue.qlen", "audit_skb_queue.qlen", ImmutableList([("include/linux/skbuff.h", 644), ("include/linux/skbuff.h", 590)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)


#--

#(double-checked locking)

searchTerm = ("swap_token_mm", "swap_token_mm", ImmutableList([("include/linux/swap.h", 244)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 4, searchTerm)

#--

searchTerm = ("nr_skbs", "nr_skbs", ImmutableList([("net/core/netpoll.c", 220)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

# (usually not reachable, but if a timeout val is set, it will happen)

searchTerm = ("swap_token_check", "swap_token_check", ImmutableList([("mm/thrash.c", 68)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)

#--

searchTerm = ("nr_free_blocks", "nr_free_blocks", ImmutableList([("fs/jffs2/gc.c", 1087)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("j_revoke", "j_revoke", ImmutableList([("fs/jbd/revoke.c", 515)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 1, searchTerm)

#--

searchTerm = ("(tr->blkcore_priv)->rq", "(tr->blkcore_priv)->rq", ImmutableList([("drivers/mtd/mtd_blkdevs.c", 80)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 1, searchTerm)

#--

searchTerm = ("surprise_rm_pending", "surprise_rm_pending", ImmutableList([("drivers/pci/hotplug/pciehp_ctrl.c", 476)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2, searchTerm)

#--

searchTerm = ("nr_reserved_vectors", "nr_reserved_vectors", ImmutableList([("drivers/pci/msi.c", 499)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

#should use atomic read/write bitvector funcs?

searchTerm = ("j_flags", "j_flags", ImmutableList([("fs/jbd/commit.c", 198), ("fs/jbd/journal.c", 1460)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

# race (not sure of severity)

searchTerm = ("gcblock", "gcblock", ImmutableList([("fs/jffs2/gc.c", 511), ("fs/jffs2/gc.c", 1134), ("fs/jffs2/gc.c", 1184)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

#double checked

searchTerm = ("(journal->j_superblock)->s_start", "(journal->j_superblock)->s_start", ImmutableList([("fs/jbd/journal.c", 937)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

#due to "accelerating common fastpath"... not sure of implications

searchTerm = ("acct_globals.file", "acct_globals.file", ImmutableList([("kernel/acct.c", 559)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

# for debugging

searchTerm = ("dirty_size", "dirty_size", ImmutableList([("fs/jffs2/debug.c", 38), ("fs/jffs2/debug.c", 41)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

#(similar to other c->blah races which use f->sem instead of erase_lock)

searchTerm = ("nr_erasing_blocks", "nr_erasing_blocks", ImmutableList([("fs/jffs2/gc.c", 1087)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)
    
#--

#for debug

searchTerm = ("erasing_size", "erasing_size", ImmutableList([("fs/jffs2/debug.c", 38), ("fs/jffs2/debug.c", 41)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

# I think it may be a bug (from trying to free null?)

searchTerm = ("(jffs2_compressor_list.next)->compr_buf", "(jffs2_compressor_list.next)->compr_buf", ImmutableList([("fs/jffs2/compr.c", 104), ("fs/jffs2/compr.c", 126)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("rt_deadline", "rt_deadline", ImmutableList([("net/ipv4/route.c", 681), ("net/ipv4/route.c", 715)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2, searchTerm)

#--

# for debugging

searchTerm = ("unchecked_size", "unchecked_size", ImmutableList([("fs/jffs2/debug.c", 38), ("fs/jffs2/debug.c", 41)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("none_stat_decompr_blocks", "none_stat_decompr_blocks", ImmutableList([("fs/jffs2/compr.c", 182), ("fs/jffs2/gc.c", 747)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 1, searchTerm)


#--

searchTerm = ("none_stat_compr_blocks", "none_stat_compr_blocks", ImmutableList([("fs/jffs2/compr.c", 157), ("fs/jffs2/gc.c", 1267)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 1, searchTerm)


#--

searchTerm = ("softcursor_original", "softcursor_original", ImmutableList([("drivers/char/vt.c", 520)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--

#(grabs object from a custom free list)

searchTerm = ("gfp_mask", "gfp_mask", ImmutableList([("kernel/audit.c", 591)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, initLabel, 3, searchTerm)

#--

#(maybe race... comment said so)... matches with 4 now

searchTerm = ("mapping", "mapping", ("mm/page_alloc.c", 689), ("fs/buffer.c", 848))
tempRes = race_on_search_exact(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

# how severe?

searchTerm = ("console_sem.wait.task_list.next", "console_sem.wait.task_list.next", ImmutableList([("include/linux/wait.h", 84)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)


#--

#(idempotent)

searchTerm = ("socket->socket", "socket->socket", ImmutableList([("drivers/pcmcia/cs.c", 649), ("drivers/pcmcia/cs.c", 324)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

searchTerm = ("fsm.state", "fsm.state", ImmutableList([("drivers/net/irda/sir_kthread.c", 413), ("drivers/net/irda/sir_kthread.c", 254)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, uniqueLabel, 2, searchTerm)

#--

searchTerm = ("fsm.param", "fsm.param", ImmutableList([("drivers/net/irda/sir_kthread.c", 290)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, uniqueLabel, 2, searchTerm)
    
#--

searchTerm = ("swap_info[0].highest_bit", "swap_info[0].highest_bit", ImmutableList([("mm/swapfile.c", 261), ("mm/swapfile.c", 260)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, condLabel, 3, searchTerm)

#--

#(thread stop flag)

searchTerm = ("stop_backup_sync", "stop_backup_sync", ImmutableList([("net/ipv4/ipvs/ip_vs_sync.c", 739), ("net/ipv4/ipvs/ip_vs_sync.c", 742)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--

#stat variable (to count races... haha)

searchTerm = ("swap_cache_info.exist_race", "swap_cache_info.exist_race", ImmutableList([("mm/swap_state.c", 110)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)


#--

# always set to the same var X, and X is always 0?

searchTerm = ("audit_backlog_wait_time", "audit_backlog_wait_time", ImmutableList([("kernel/audit.c", 695)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)

#--

searchTerm = ("sync_send_mesg_maxlen", "sync_send_mesg_maxlen", ImmutableList([("net/ipv4/ipvs/ip_vs_sync.c", 414), ("net/ipv4/ipvs/ip_vs_sync.c", 414)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, notParLabel, 2, searchTerm)

#--

#during panic / when unbusting spinlocks

searchTerm = ("console_printk[0]", "console_printk[0]", ImmutableList([("arch/i386/mm/fault.c", 54)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 3, searchTerm)


#--

#(during panic)

searchTerm = ("shift_down[0]", "shift_down[0]", ImmutableList([("drivers/char/keyboard.c", 384)]))
tempRes = race_on_search(rcs, searchTerm)


results.addResult(tempRes, raceLabel, 1, searchTerm)

#--

#(during panic)

searchTerm = ("panic_blink", "panic_blink", ImmutableList([("kernel/panic.c", 99)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--

#(benign -- for randomizer)

searchTerm = ("input_timer_state.last_time", "input_timer_state.last_time", ImmutableList([("drivers/char/random.c", 600), ("drivers/char/random.c", 599)]))
tempRes = race_on_search(rcs, searchTerm)

results.addResult(tempRes, raceLabel, 2, searchTerm)

#--

#results = race_on_at(rcs, "XXX", "XXX",
#                     [("YYY", 277),
#                      ("YYY", 270),
#                      ])


#------------------------------------------------------------
# Save the search term -> result / label mapping

save ("temp.labeled", results)

#-----------------------------------------------------------
# Also print out the distribution after applying the filters

import relay.warnings.stats

relay.warnings.stats.print_stats(r, results)


"""
TEST Stuff


r2 = load("temp.labeled")
labs = [initLabel, aliasLabel, uniqueLabel, reentLabel, notParLabel, condLabel, raceLabel, bugLabel]
r2.get_label_distr(labs, rcs)

"""
