#!/usr/bin/env python

#-- read file with function depths and summary sizes and process.


import sys
import re

#-- usage
def print_usage () :
    print 'usage: %prog depths.txt sums.txt\n'
    sys.exit (127)


class Stats:
    key = 0
    depth = 0
    locks = 0
    reads = 0
    writes = 0
    updFun = max
    def __init__(self, k, f, i):
        self.key = k
        self.updFun = f
        self.depth = i
        self.locks = i
        self.writes = i
        self.reads = i
        
    def updateDepth(self, n):
        self.depth = self.updFun(self.depth, n)
    def updateLocks(self, n):
        self.locks = self.updFun(self.locks, n)
    def updateReads(self, n):
        self.reads = self.updFun(self.reads, n)
    def updateWrites(self, n):
        self.writes = self.updFun(self.writes, n)
    def __cmp__(self, other):
        if(self.depth < other.depth):
            return -1
        elif (self.depth == other.depth):
            return 0
        else:
            return 1

def add (x, y):
    return x + y

def sortStats (st) :
    return sorted(st.items(), lambda x, y: cmp(x[1], y[1]))


def readDepths (stats, filename):
    """ Return a map from function -> stats (depth field filled) """
    funStPat = re.compile('(\d*)\t(\d*)')
    try:
        fd = open (filename, 'r')
    except IOError:
        sys.stderr.write('Can\'t open file\n')
        sys.exit (0)
    line = fd.readline ()
    while (line):
        m = funStPat.match(line)
        if m :
            fkey = int(m.group(1).strip())
            depth = int(m.group(2).strip())
            s = stats.get(fkey, Stats(fkey, max, 0))
            s.updateDepth(depth)
            stats[fkey] = s
        line = fd.readline ()
    fd.close ()
    return stats

def readSizes (stats, filename):
    """ Return a map from function -> stats (summary sizes filled) """
    funStPat = re.compile('.*SUMS .*:\t(\d+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)')
    try:
        fd = open (filename, 'r')
    except IOError:
        sys.stderr.write('Can\'t open file\n')
        sys.exit (0)
    line = fd.readline ()
    while (line):
        m = funStPat.match(line)
        if m :
            fkey = int(m.group(1).strip())
            locks = int(m.group(2).strip())
            unlocks = int(m.group(3).strip())
            writes = int(m.group(4).strip())
            reads = int(m.group(5).strip())
            s = stats.get(fkey, Stats(fkey, max, 0))
            s.updateLocks(locks + unlocks)
            s.updateWrites(writes)
            s.updateReads(reads)
            stats[fkey] = s
        line = fd.readline ()
    fd.close ()
    return stats


def update (st, new):
    st.updateLocks(new.locks)
    st.updateWrites(new.writes)
    st.updateReads(new.reads)
    
class Aggregate:
    mins = {}
    maxs = {}
    total = {}
    counts = {}
    def aggregate(self, stats):
        for f, s in stats.items():
            if (s.depth >= 0):
                k = s.depth
                mi = self.mins.get(k, Stats(k, min, sys.maxint))
                update(mi, s)
                self.mins[k] = mi
                ma = self.maxs.get(k, Stats(k, max, -sys.maxint-1))
                update(ma, s)
                self.maxs[k] = ma
                to = self.total.get(k, Stats(k, add, 0))
                update(to, s)
                self.total[k] = to
                co = self.counts.get(k, 0)
                self.counts[k] = co + 1
                
    def printStats (self, depthFile, sumFile):
        stats = getData(depthFile, sumFile)
        self.aggregate(stats)
        print "#depth\tcount\tavg locks\tmax\tmin\tavg writes\tmax\tmin\tavg reads\tmax\tmin"
        for k, c in self.counts.items():
            if (c > 0) :
                avgl = float(self.total[k].locks)/c
                avgw = float(self.total[k].writes)/c
                avgr = float(self.total[k].reads)/c
                print '%d\t%d\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f' % (k, c,
                             avgl,
                             self.maxs[k].locks,
                             self.mins[k].locks,
                             avgw,
                             self.maxs[k].writes,
                             self.mins[k].writes,
                             avgr,
                             self.maxs[k].reads,
                             self.mins[k].reads,
                            )


def getData (depthFile, sumFile):
    stats = readDepths ({}, depthFile)
    stats = readSizes (stats, sumFile)
    return stats

def printData (depthFile, sumFile):
    stats = getData(depthFile, sumFile)
    sorted = sortStats (stats)
    print "#depths\tlocks\twrites\treads"
    for f, s in sorted:
        if (s.locks >= 0) and (s.depth >= 0):
            print '%d\t%d\t%d\t%d' % (s.depth,
                                      s.locks, s.writes, s.reads)


    

#-- main
def main () :
    # get filename
    if (len(sys.argv) <= 2) :
        print_usage ();
    else :
        depth = sys.argv[1]
        summ = sys.argv[2]
    print 'reading data from ' + depth + ' & ' + summ + '\n'
    #printData (depth, summ)
    agg = Aggregate()
    agg.printStats(depth, summ)
    sys.exit (0)


#-- go!        
main ()
