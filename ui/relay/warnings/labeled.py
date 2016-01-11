""" 
Handle label filtering. Also, track how labels were originally added
(based on whatever search terms were used to apply labels)
"""

import sys
import pickle

from relay.warnings.models import *
from relay.warnings.search import *



#------------------------------------------------------------
# Label filtering
#------------------------------------------------------------


# QuerySet-based methods

def labeled_races(rcs):
    return rcs.filter(races__labels__isnull = False).distinct()

def races_with_label(rcs, label):
    """ Return race clusters within given rcs that have a given label """
    result = rcs.filter(races__labels = label).distinct()
    return result

def label_distr(rcs, labels):
    """ Given a list of labels, return a map from label -> races """
    result = {}
    for l in labels:
        result = dict(result, **{ l : races_with_label(rcs, l) })
    return result

def print_label_distr(rcs, labels):
    t = time.time()
    dist = label_distr(rcs, labels)
    for l in labels:
        print "%s: \t%d" % (l.label, dist[l].count())
    print "Total labeled: %d" % (labeled_races(rcs).count())
    print "Filtered in %.3f" % (time.time() - t)

# Slower(?) list based methods

def has_label(l):
    """ return a function that accepts a race and returns true 
    if the race is labeled with l """
    return lambda race: (race.filter(labels=l).count() > 0)

def racelist_with_label(rcs, label, notlabels):
    result = []
    for rc in rcs:
        if rc.races.filter(labels = label).count() > 0:
            if rc.races.filter(labels__in = notlabels).count() > 0:
                continue
            else:
               result.append(rc) 
    return result
#    for l in notlabels:
#        qss = [qs.exclude(labels=l) for qs in qss]
#    return [rc for rc in qss
#            if rc.filter(labels=label).count() > 0]


def labeled_racelist(rcs):
    """ Return list of Race_clusters that have SOME label """
    return [rc for rc in rcs if rc.races.filter(labels__isnull = False).count() > 0]


def l_label_distr(rcs, labels):
    """ Given a list of labels, return a map from label -> races """
    result = {}
    for i in range(len(labels)):
        l = labels[i]
        exclude = labels[:i]
        result = dict(result, **{ l : racelist_with_label(rcs, l, exclude) })
    return result


def l_print_label_distr(rcs, labels):
    """ Print the distribution of labels in the given list of rcs """
    t = time.time()
    total = len(labeled_racelist(rcs))
    dist = l_label_distr(rcs, labels)
    for l in labels:
        inCat = len(dist[l])
        print "%s: \t%d\t%.3f" % (l.label, inCat, float(inCat)/float(total))
    print "Total labeled: %d" % total
    print "Filtered in %.3f" % (time.time() - t)


#------------------------------------------------------------
# Track the original search terms (as we are adding labels)
#------------------------------------------------------------

#---- Helper classes / functions

class ImmutableList (list):
    theList = []
    #
    def __init__(self, aList):
        """ track the given aList """
        self.theList = aList
    #
    def __hash__(self):
        """ Override hash to combine the hash-value of each element """
        h = 0
        i = 0
        # xor each item's hash... should probably take into consideration
        # the ordering of the elements though...
        for e in self.theList:
            h ^= (hash(e) << i)
            i =  (i + 1 % 32)
        return h
    #
    def __eq__(self, other):
        """ Override to check equality across all elements in list """
        if (len(self.theList) == len(other.theList)):
            for (i, j) in zip(self.theList, other.theList):
                if (i != j):
                    return False
            return True
        else:
            return False
    #
    def __ne__(self, other):
        return not (self == other)
    #
    def __repr__(self):
        """ Override __repr__ """
        return repr(self.theList)
    #
    def __getitem__(self, i):
        return self.theList[i]
    #
    def __iter__(self):
        return iter(self.theList)
    #
    def __len__(self):
        return len(self.theList)
    # contains, slicing, append, add, etc... DON'T work "correctly"

""" 
TEST: 
l1 = ImmutableList ([1, 3, 23])
l2 = ImmutableList ([1, 3, 23])
l3 = ImmutableList ([1, 23, 3])

(hash(l1) == hash(l2)) == True
(l1 == l2) == True
(l2 == l1) == True
(l1 == l3) == False
(hash(l1) != hash(l3))
(l1 != l3) == True
(l1 != l2) == False
t = {}
t[l1] = 2441
t[l1] == 2441
t[l2] == 2441
"""


def doesIntersect (rcs1, rcs2):
    """ return true if there is a common element between the 
    two given lists of races (not arbitrary lists, unfortunately) """
    temp = {}
    if len(rcs1) < len(rcs2):
        smaller, bigger = rcs1, rcs2
    else:
        smaller, bigger = rcs2, rcs1
    for r in smaller:
        # hash r.id because original objs can have different hashes
        # even if the ids are identical
        temp[r.id] = 1 
    for r in bigger:
        if temp.has_key(r.id):
            return True
    return False


def firstLabel (labelOrder, race):
    """ Given a label order (represented as list of labels -- 
    first item has highest priority), return the first label 

    that the given race contains. If none found, return None """
    for l in labelOrder:
        if (race.filter(labels=l).count() > 0):
            return l
    return None


def printExpectation (results, label, num):
    """ Inform user that we will be adding new result. 
    Warn if the number of results is different from the expected num """
    l = len(results)
    isDiff = ""
    if l != num :
        isDiff = "DIFFERENT!"
    print "labeling: %s\tgot: %d\texpecting %d %s" % \
        (label, l, num, isDiff)
    if (l > 0):
        print results[0].first_race()
    print "\n\n"
    return


#---- Track labeled results temporarily to allow later comparison
class ResultTable :
    # map from search term to its race clusters and the added label
    table = { }
    #
    def __init__(self, tab):
        table = tab
    def addResult (self, newResults, newLabel, expectedNum, searchTerm):
        """ Add the newLabel to the newResults and track the searchTerm used
        to find the newResults """
        printExpectation(newResults, newLabel, expectedNum)
        for rc in newResults:
            # Decouple adding of labels from tracking search terms?
            rc.add_label(newLabel) 
        self.table[searchTerm] = (newResults, newLabel)
        return
    #
    def get_label_distr (self, labelOrder, rcs):
        """ Intersect the search term table with the set of race clusters
        and return the label distribution. If a result set has more than
        one label, only count the label that's first in the list labelOrder
        """
        label_distr = {}
        # initialize counts
        for l in labelOrder:
            label_distr[l.id] = 0 # hash on label id, not the original obj!
        total = 0
        # add up counts of non-zero matches
        for searchTerm, (results, mainLabel) in self.table.items():
            if doesIntersect (results, rcs):
                # add 1 to the count for the results' main label
                label_distr[mainLabel.id] = label_distr[mainLabel.id] + 1
                total += 1
        return (total, label_distr)


def o_print_label_distr(rcs, labels, results):
    """ Print the distribution of labels based on the original search terms """
    t = time.time()
    total, dist = results.get_label_distr(labels, rcs)
    for l in labels:
        inCat = dist[l.id]
        print "%s: \t%d\t%.3f" % (l.label, inCat, float(inCat)/float(total))
    print "Total labeled: %d" % total
    print "Filtered in %.3f" % (time.time() - t)


#------------------------------------------------------------
# Have a way to keep the table persistent 
# TODO: be able to pretty print everything back out 
# (the code that originally generated the table!
    

def save (fname, results):
    """ Save a previously computed ResultTable (results) to the file fname """
    try:
        fd = open (fname, 'w')
        pickle.dump (results.table, fd)
        fd.close()
    except IOError:
        print >> sys.stderr, "Labeled: Couldn't save table to %s" % fname
        raise


def load (fname):
    """ Load a previously stored ResultTable. Doesn't seem to work... """
    try:
        fd = open (fname, 'r')
        obj = ResultTable(pickle.load (fd))
        fd.close
        return obj
    except IOError:
        print >> sys.stderr, "Labeled: Couldn't load table from %s" % fname
        raise


#----------------------------------------------------------

"""
TESTING


# TEST label distribution printing

from relay.warnings.search import *
from relay.warnings.models import *
from relay.warnings.labeled import *

r = Run.objects.get(id=9)
rcs = Race_cluster.objects.filter(run=r)

initLabel = getLabel("Initialization")
aliasLabel = getLabel("Unlikely aliasing")
uniqueLabel = getLabel("Unique")
reentLabel = getLabel("Reentrant lock")
notParLabel = getLabel("Not in parallel")
condLabel = getLabel("Conditional lock")
raceLabel = getLabel("Race")
bugLabel = getLabel("Race bug")

inits = races_with_label(rcs, initLabel)

inits = racelist_with_label(rcs, initLabel)

l_print_label_distr(rcs, [initLabel, aliasLabel, uniqueLabel, reentLabel,
             notParLabel, condLabel, raceLabel, bugLabel])


# TEST SAVING 

r = load ("temp.labeled")

"""
