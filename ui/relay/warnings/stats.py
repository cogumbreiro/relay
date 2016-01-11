from relay.warnings.models import *
from relay.warnings.search import *
from relay.warnings.labeled import *
import time


#------------------- simple DB filter-based ---------------

#TODO allow exclusion
def num_filter2(run, filter1, filter2):
    t = time.time()
    counts = [0, 0, 0]
    this_run = Race_cluster.objects.filter(run = run)
    for r in this_run:
        acc1 = r.races.filter(**filter1)
        acc2 = r.races.filter(**filter2)
        counts[(acc1[:1]).count() + (acc2[:1]).count()] += 1
    print "Counted in %.3f" % (time.time() - t)
    return counts


def num_filter(run, include, exclude):
    t = time.time()
    counts = [0, 0]
    this_run = Race_cluster.objects.filter(run = run)
    for r in this_run:
        n = r.races.filter(**include).exclude(**exclude)
        counts[(n[:1]).count()] += 1
    print "Counted in %.3f" % (time.time() - t)
    return counts


#-------------------- count those matching arbitrary predicates

def num_matches2(run, check1, check2, d):
    t = time.time()
    counts = [0, 0, 0]
    this_run = Race_cluster.objects.filter(run = run)
    for rc in this_run:
        races = rc.races.select_related(depth=d)
        a1 = exists(check1, races)
        a2 = exists(check2, races)
        counts[a1 + a2] += 1
    print "Counted in %.3f" % (time.time() - t)
    return counts


def num_matches(run, check, d):
    return len(race_matches(run, check, d))


def num_filter_match(run, include, exclude, check, d):
    return len(race_filter_match(run, include, exclude, check, d))

#--------------------

def num_tr_races(run, substr):
    """
    Given that boot_name is the name of a boot-up function, count the number
    of race clusters where there exists a race with zero, one, or both accesses
    beginning at this bootup function
    """
    filter1 = {'access1__accessed_through__root_function__name' : substr}
    filter2 = {'access2__accessed_through__root_function__name' : substr} 
    return num_filter2(run, filter1, filter2)

def num_boot_races(run, boot_name='init'):
    return num_tr_races(run, boot_name)

def num_fun_contains(run, substr):
    filter1 = {'access1__occurs_at__parent_function__name__icontains' : substr}
    filter2 = {'access2__occurs_at__parent_function__name__icontains' : substr}
    return num_filter2(run, filter1, filter2)

def num_lval_contains(run, substr):
    filter1 = {'access1__lval__printed__icontains' : substr}
    filter2 = {'access2__lval__printed__icontains' : substr}
    return num_filter2(run, filter1, filter2)

def num_lval_regex(run, regex):
    filter1 = {'access1__lval__printed__regex' : regex}
    filter2 = {'access2__lval__printed__regex' : regex}
    return num_filter2(run, filter1, filter2)

# test code
"""
from relay.warnings.stats import *
from relay.warnings.models import *
r = Run.objects.get(id=5)
num_boot_races(r)

num_fun_contains(r, 'nolock')
num_lval_contains(r, 'stats')
"""


#---------------------


def num_alloc_races(run):
    """
    Count the number of race clusters where there exists a race with
    zero, one, or both accesses involving a freshly allocated variable
    """
    return num_matches2(run, is_alloc1, is_alloc2, 4)

# test code
"""
from relay.warnings.stats import *
from relay.warnings.models import *
r = Run.objects.get(id=5)
num_alloc_races(r)

this_run = Race_cluster.objects.filter(run = r)
for rc in this_run:
   races = rc.races.select_related(depth=4)
   lval = races[0].access2.lval
   print lval.var_id
"""

#-------------------


def num_fun_matches(run, pat):
    return num_matches2(run, fun_match_pat1(pat), fun_match_pat2(pat), 5)


boot_regex = re.compile('init')
debug_regex = re.compile('.*dbg.*nolock.*')
stats_regex = re.compile('.*stats.*')

# test code
"""
from relay.warnings.stats import *
from relay.warnings.models import *
r = Run.objects.get(id=5)
num_fun_matches(r, debug_regex)
"""

#-------------------

def num_blob_n_races(run, n):
    """
    Count the number of race clusters where there exists a race with
    accesses involving representative nodes of size greater than n
    """
    return num_matches(run, uses_blob(n), 4)
    
def num_blob_n_races2(run, n):
    rcs = Race_cluster.objects.filter(run = run)
    return rcs.filter(uses_blob_lt_filter(n)).distinct.count()


# test code
"""
from relay.warnings.stats import *
from relay.warnings.models import *
r = Run.objects.get(id=5)
num_blob_n_races(r, 1)
"""

#-------------------

def num_syntactic(run):
    return num_matches(run, lvals_syntactic, 4)

# test code
"""
from relay.warnings.stats import *
from relay.warnings.models import *
r = Run.objects.get(id=9)
num_syntactic(r)
"""

def with_locks(races):
    return filter_more(races, with_lock)

#-------------------

def min_locks_races(run):
    counts = {}
    t = time.time()
    this_run = Race_cluster.objects.filter(run = run)
    for rc in this_run:
        races = rc.races.select_related(depth=4)
        m1 = min ([r.access1.locks.count() for r in races])
        m2 = min ([r.access2.locks.count() for r in races])
        counts[(m1, m2)] = counts.get((m1, m2), 0) + 1
    print "Counted in %.3f" % (time.time() - t)
    return counts

# test code
"""
from relay.warnings.stats import *
from relay.warnings.models import *
r = Run.objects.get(id=5)
min_locks_races(r)

import hotshot, hotshot.stats
prof = hotshot.Profile("temp.prof")
benchtime = prof.runcall(min_locks_races, r)
prof.close()
stats = hotshot.stats.load("temp.prof")
stats.strip_dirs()
stats.sort_stats('time', 'calls')
stats.print_stats(20)
"""

#-------------------- Combos -----------------

def alloc_and_syntactic(run):
    return num_matches(run, all_of_list(
        [(lvals_syntactic, True), (lval_match_pat1(lval_alloc_re), True)]),
                       4)

def blob_and_not_syntactic(run, n):
    return num_matches(run, all_of_list(
        [(uses_blob(n), True), (lvals_syntactic, False)]), 4)

def likely_races(run):
    incl = {}
    excl = {}
    #excl = {'access1__accessed_through__root_function__name' : 'init',
    #        'access2__accessed_through__root_function__name' : 'init',
    #        }
    # using that filter it's really slow & gives more races? 1024
    pred = all_of_list(
        [(lvals_syntactic, True), (uses_blob(1), False), # blobby
         (lval_match_pat1(lval_alloc_re), False),        # uses alloc var
         (tr_match_pat1(boot_regex), False),             # in init() thread
         (tr_match_pat2(boot_regex), False),
         (fun_match_pat1(debug_regex), False),           # in 'debug' funcs
         (fun_match_pat2(debug_regex), False),
         (lval_match_pat1(stats_regex), False),          # stat variable
         ])
    return race_filter_match(run, incl, excl, pred, 5)


def num_likely_races(run):
    return len(likely_races(run))


def likely_races2(run):
    incl = {}
    excl = {}
    pred = all_of_list(
        [(lvals_syntactic, True), (uses_blob(1), False), # blobby
         (lval_match_pat1(lval_alloc_re), False),        # uses alloc var
         (tr_match_pat1(boot_regex), False),             # in init() thread
         (tr_match_pat2(boot_regex), False),
         (fun_match_pat1(debug_regex), False),           # in 'debug' funcs
         (fun_match_pat2(debug_regex), False),
         (lval_match_pat1(stats_regex), False),          # stat variable
         (same_thread, False),                           # for not in parallel
         ])
    return race_filter_match(run, incl, excl, pred, 5)


def getKnownLabels():
    initLabel = getLabel("Initialization")
    aliasLabel = getLabel("Unlikely aliasing")
    uniqueLabel = getLabel("Unique")
    reentLabel = getLabel("Reentrant lock")
    notParLabel = getLabel("Not in parallel")
    condLabel = getLabel("Conditional lock")
    raceLabel = getLabel("Race")
    bugLabel = getLabel("Race bug")
    return  [initLabel, aliasLabel, uniqueLabel, reentLabel,
             notParLabel, condLabel, raceLabel, bugLabel]


def print_stats(run, results):
    t = time.time()
    incl = {}
    excl = {}
    labs = getKnownLabels()
    #
    races = Race_cluster.objects.filter(run=run)
    print "No filters:\t%d" % races.count()
    l_print_label_distr(races, labs)
    o_print_label_distr(races, labs, results)
    # successively filter races
    print "\n"
    races = filter_more(races, all_of_list(
        [(lval_match_pat1(lval_alloc_re), False),
         (lval_match_pat2(lval_alloc_re), False)]), 5)
    print ("& not alloced in thread:\t%d" % len(races))
    l_print_label_distr(races, labs)
    o_print_label_distr(races, labs, results)
    #
    print "\n"
    races = filter_more(races, isnt(uses_blob(1)))
    print ("& not blobby:\t%d" % len(races))
    l_print_label_distr(races, labs)
    o_print_label_distr(races, labs, results)
    #
    #races = filter_more(races, lvals_syntactic)
    #races = race_filter_match(run, incl, excl, lvals_syntactic, 5)
    #print ("syntactic match:\t%d" % len(races))
    #l_print_label_distr(races, labs)
    #o_print_label_distr(races, labs, results)
    #
    #races = filter_more(races, isnt(lval_match_pat1(lval_alloc_re)))
    #print ("& not alloced in thread:\t%d" % len(races))
    #l_print_label_distr(races, labs)
    #o_print_label_distr(races, labs, results)
    #
    print "\n"
    races = filter_more(races, all_of_list(
        [(tr_match_pat1(boot_regex), False),             # in init() thread
         (tr_match_pat2(boot_regex), False),]))
    print ("& not bootup:\t%d" % len(races))
    l_print_label_distr(races, labs)
    o_print_label_distr(races, labs, results)
    #
    #print "\n"
    #races = filter_more(races, all_of_list(
    #    [(fun_match_pat1(debug_regex), False),           # in 'debug' funcs
    #     (fun_match_pat2(debug_regex), False),]))
    #print ("& not debug func:\t%d" % len(races))
    #l_print_label_distr(races, labs)
    #o_print_label_distr(races, labs, results)
    #
    #print "\n"
    #races = filter_more(races, all_of_list(              # uses a 'stat' var
    #    [(lval_match_pat1(stats_regex), False),
    #     (lval_match_pat2(stats_regex), False)]))
    #print ("& not stat variable:\t%d" % len(races))
    #l_print_label_distr(races, labs)
    #o_print_label_distr(races, labs, results)
    #
    print "\n"
    races = filter_more(races, isnt(same_thread))
    print ("& not same thread:\t%d" % len(races))
    l_print_label_distr(races, labs)
    o_print_label_distr(races, labs, results)
    #
    print "\n"
    races = filter_more(races, with_lock)
    print ("& has some lock:\t%d" % len(races))     # filters not in parallel
    l_print_label_distr(races, labs)
    o_print_label_distr(races, labs, results)
    #
    print "\n"
    races = filter_more(races, uses_global)
    print ("& is global:\t%d" % len(races))
    l_print_label_distr(races, labs)
    o_print_label_distr(races, labs, results)
    #
    print "\n"
    print "Time spent gathering stats: %.3f" % (time.time() - t)
    return races # might as well return the survivors
    

"""
from relay.warnings.stats import *
from relay.warnings.models import *
from relay.warnings.labeled import *

r = Run.objects.get(id=9)

alloc_and_syntactic(r)

blob_and_not_syntactic(r, 1)

likely_rs = likely_races(r)
len(likely_rs)

357 (for run id = 9)

locked = with_locks(likely_rs)
len(locked)

52 (for run id = 9)

likely_races2(r)

182 (for run id = 9)

with_locks(likely_races2)

29 (for run id = 9)
28 (locked_glob for likely_races2)


not_locked = [r for r in likely_rs if not (r in locked)]

locked_glob = [r for r in locked if (exists(uses_global, r.races.select_related(4)))]

not_locked_glob = [r for r in not_locked if (exists(uses_global, r.races.select_related(4)))]


print_stats(r)

"""


