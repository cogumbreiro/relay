from relay.warnings.models import *
import re
import time
from django.db.models.query import Q

# Search for certain kinds of races

#------------------------------------------------------------
# UI :: Search forms, etc.
#------------------------------------------------------------

from django import newforms as forms

class SearchForm(forms.Form):
    lval_1 = forms.CharField(max_length=75)
    file_1 = forms.CharField(max_length=100)
    line_1 = forms.IntegerField()
    lval_2 = forms.CharField(max_length=75)
    file_2 = forms.CharField(max_length=100)
    line_2 = forms.IntegerField()


#-- UI for table of links to results for each label?





#------------------------------------------------------------
# Quantifiers, "true", "false", and, or, etc. 
# for building more complex filtering predicates
#------------------------------------------------------------


def exists(condition, list):
    return reduce(lambda x,y,c=condition: x or c(y), list, 0)

def forall(condition, list):
    return reduce(lambda x,y,c=condition: x and c(y), list, 1)

def both(c1, c2):
    return lambda x: c1(x) and c2(x)

def either(c1, c2):
    return lambda x: c1(x) or c2(x)

def isnt(c):
    return lambda x: not c(x)

def trueP(x):
    return True

def falseP(x):
    return False

def all_of_list(l):
    uber_cond = trueP
    for (m, p) in l:
        if (p) :
            uber_cond = both(uber_cond, m)
        else :
            uber_cond = both(uber_cond, isnt(m))
    return uber_cond

def any_of_list(l):
    uber_cond = falseP
    for (m, p) in l:
        if (p) :
            uber_cond = either(uber_cond, m)
        else :
            uber_cond = either(uber_cond, isnt(m))
    return uber_cond

def filters_of_list(l):
    uber_filt = {}
    for f in l:
        uber_filt.update(f)
    return uber_filt

#------------------------------------------------------------
# Mechanism for filtering races, given a predicates
#------------------------------------------------------------

def race_matches(run, check, _depth):
    """ Return all races which evaluate to true for a given check
    (and from the given run) """
    t = time.time()
    rcs = []
    this_run = Race_cluster.objects.filter(run = run)
    for rc in this_run:
        races = rc.races.select_related(depth=_depth)
        if (exists(check, races)):
            rcs.append(rc)
    print "Filtered in %.3f" % (time.time() - t)
    return rcs

def race_filter_match(run, include, exclude, check, _depth):
    """ Return all races which are included in the search criteria (include),
    and not excluded (from exclude) and are from the given run """
    t = time.time()
    rcs = []
    this_run = Race_cluster.objects.filter(run = run)
    for rc in this_run:
        races = rc.races.select_related(depth=_depth).filter(**include).exclude(**exclude)
        if (exists(check, races)):
            rcs.append(rc)
    print "Filtered in %.3f" % (time.time() - t)
    return rcs


def filter_more(race_clusts, check, depth=5):
    """ Given a list of races (presumably this is the result list of
    a previously run filter), return the subset that pass the check """
    t = time.time()
    res = [r for r in race_clusts
           if (exists(check, r.races.select_related(depth)))]
    print "Filtered in %.3f" % (time.time() - t)
    return res



#------------------------------------------------------------
# Predicates for race heuristic filters
#------------------------------------------------------------


def lval_match_pat1(pat):
    return lambda race: pat.match (str(race.access1.lval.printed)) != None

def lval_match_pat2(pat):
    return lambda race: pat.match (str(race.access2.lval.printed)) != None

lval_alloc_re = re.compile('.*_a\d+_\d+.*')

is_alloc1 = lval_match_pat1(lval_alloc_re)
is_alloc2 = lval_match_pat2(lval_alloc_re)

def lvals_syntactic(race):
    return str(race.access1.lval.printed) == str(race.access2.lval.printed)

#---

def fun_match(pf, pat):
    if (pf):
        return pat.match (str(pf.name)) != None
    else:
        return False
    
def fun_match_pat1(pat):
    return lambda race: fun_match(race.access1.occurs_at.parent_function, pat)

def fun_match_pat2(pat):
    return lambda race: fun_match(race.access2.occurs_at.parent_function, pat)

#---

def tr_match_pat1(pat):
    return lambda race: fun_match(race.access1.accessed_through.root_function, pat)

def tr_match_pat2(pat):
    return lambda race: fun_match(race.access2.accessed_through.root_function, pat)

#---

def uses_blob(n):
    return lambda race : (race.access1.lval.rep_size > n or
                          race.access2.lval.rep_size > n)

#---

def uses_global(race):
    return race.access1.lval.is_global or race.access2.lval.is_global

#---

def with_lock(race):
    return race.access1.locks.count() > 0 or race.access2.locks.count() > 0

#---

def same_thread(race):
    return (race.access1.accessed_through.spawn_site == race.access2.accessed_through.spawn_site) and (race.access1.accessed_through.root_function == race.access2.accessed_through.root_function)



#------------------------------------------------------------
# Search for races matching given search terms

def race_on_at(rcs, lv1=None, lv2=None, locs=[], d=5):
    """
    Return race clusters on races that access the given lvals
    (printed form) at some location in the given locations (filename, line)
    """
    t = time.time()
    filt = Q()
    if (lv1 != None):
        filt = filt & Q(races__access1__lval__printed__icontains = lv1)
    if (lv2 != None):
        filt = filt & Q(races__access2__lval__printed__icontains = lv2)
    filts = Q()
    for (f, l) in locs:
        filts = filts | (filt & Q(races__access1__occurs_at__line_num = l,
                                  races__access1__occurs_at__file_name__icontains = f))
        filts = filts | (filt & Q(races__access2__occurs_at__line_num = l,
                                  races__access2__occurs_at__file_name__icontains = f))
    x = rcs.filter(filts).distinct()
    print "Num results: %d -> %s" % (x.count(), [rc.id for rc in x])
    print "Filtered in %.3f" % (time.time() - t)
    return x


def race_on_exact(rcs, lv1, lv2, (f1,l1), (f2,l2), d=5):
    """
    Return race clusters on races that access the given lvals
    (printed form) at the exact given locations (filename, line)
    """
    t = time.time()
    filt = Q()
    filt = filt & Q(races__access1__lval__printed__icontains = lv1)
    filt = filt & Q(races__access2__lval__printed__icontains = lv2)
    filts = ((filt & Q(races__access1__occurs_at__line_num = l1,
        races__access1__occurs_at__file_name__icontains = f1) &
        Q(races__access2__occurs_at__line_num = l2,
        races__access2__occurs_at__file_name__icontains = f2)) |
        (filt & Q(races__access1__occurs_at__line_num = l2,
        races__access1__occurs_at__file_name__icontains = f2) &
        Q(races__access2__occurs_at__line_num = l1,
        races__access2__occurs_at__file_name__icontains = f1)))
    x = rcs.filter(filts).distinct()
    print "Num results: %d -> %s" % (x.count(), [rc.id for rc in x])
    print "Filtered in %.3f" % (time.time() - t)
    return x

def race_on_search(rcs, searchTerm):
    lv1, lv2, locs = searchTerm
    return race_on_at(rcs, lv1, lv2, locs)

def race_on_search_exact(rcs, searchTerm):
    lv1, lv2, loc1, loc2 = searchTerm
    return race_on_exact(rcs, lv1, lv2, loc1, loc2)

def race_on_at2(rcs, lv1=None, lv2=None, locs=[], d=5):
    """
    OLD VERSION: Return race clusters on races that access the given lvals
    (printed form) at some location in the given locations (filename, line)
    """
    t = time.time()
    filt = {}
    if (lv1 != None):
        filt = dict(filt, **{'access1__lval__printed__icontains' : lv1})
    if (lv2 != None):
        filt = dict(filt, **{'access2__lval__printed__icontains' : lv2})
    filts = [dict({'access1__occurs_at__line_num' : l,
                   'access1__occurs_at__file_name__icontains' : f})
             for (f, l) in locs]
    filts = filts + [dict({'access2__occurs_at__line_num' : l,
                           'access2__occurs_at__file_name__icontains' : f})
                     for (f, l) in locs]
    results = []
    found = False # hack because continue restarts inner loop
    for r in rcs:
        found = False
        rs = r.races.select_related().filter(**filt)
        if (rs.count() <= 0):
            continue
        for f in filts:
            if(found):
                continue
            rs = rs.filter(**f)
            if (rs.count() > 0):
                results.append(r)
                found = True
                continue
    print "Filtered in %.3f" % (time.time() - t)
    return results


#------------- TESTS -------------

"""
from relay.warnings.search import *
from relay.warnings.models import *
r = Run.objects.get(id=9)
rcs = Race_cluster.objects.filter(run=r)


results = race_on_at(rcs, "shift_state", "shift_state",
   [("drivers/char/keyboard.c", 385), ("drivers/char/keyboard.c", 361)])
len(results)
>>> 3

results = race_on_at(rcs, "app_abort_code", "app_abort_code",
                     [("net/rxrpc/call.c", 102),
                      ("net/rxrpc/call.c", 889)])
len(results)
>>> 2

"""
