#!/usr/bin/env python

""" read stdin and check that max scc size is >= MIN (where MIN is given)
    or, if the total number of functions is approaching that bound and
    the max is still 70% of all the functions, allow it...
    i.e., MAX >= 0.7 * TOTAL && 
        let DIFF = TOTAL - MIN where
        DIFF >= 0 && DIFF <= 0.3 * MIN 
"""

import sys
import re


#-- usage
def print_usage () :
    print 'usage: %prog min_size\n'
    sys.exit (127)

def inBounds (total, size, min_size):
    if (size >= min_size) :
        return True
    else :
        diff = total - min_size
        return ((size >= 0.7 * total) and
                (diff >= 0 and diff <= 0.3 * min_size))

#-- main
def main () :
    # get filename
    if (len(sys.argv) != 2) :
        print_usage ();
    else :
        min_size = int (sys.argv[1])
    scc_pattern = re.compile('SCC STATS.*TOTAL: ([0-9]+).*MAX: ([0-9]+)')
    for line in sys.stdin.readlines():
        m = scc_pattern.match(line)
        if m :
            total = int (m.group(1))
            size = int (m.group(2))
            if (inBounds (total, size, min_size)) :
                print "OKAY scc size is %d/%d vs %d\n" % (size, total, min_size)
                sys.exit(0)
            else :
                print "NO GOOD scc size is %d/%d vs %d\n" % (size, total, min_size)
                sys.exit(1)
    sys.stderr.write ('No match?!\n')
    sys.exit (1)

#-- go!        
main ()

