#!/usr/bin/env python

#-- read the warnings file and count frequency of lvals, etc.


import sys
import re

#-- usage
def print_usage () :
    print 'usage: %prog warn.txt\n'
    sys.exit (127)

def sortFreqs (fs) :
    return sorted(fs.items(), lambda x, y: cmp(x[1], y[1]))

def printLvalFreq (filename) :
    freqs = {}
    lvStartPattern = re.compile('Possible race between access to:')
    firstLv = re.compile('(.*) and')
    secondLv = re.compile('(.*)')
    try:
        fd = open (filename, 'r')
    except IOError:
        sys.stderr.write('Can\'t open file\n')
        sys.exit (0)

    line = fd.readline ()
    while (line):
        if (lvStartPattern.match(line)) :
            lv1 = firstLv.match(fd.readline ()).group(1).strip()
            lv2 = secondLv.match(fd.readline ()).group(1).strip()
            if (lv1 == lv2) :
                try:
                    freqs [lv1] = freqs[lv1] + 1
                except KeyError:
                    freqs [lv1] = 1
            else :
                try:
                    freqs [lv1] = freqs[lv1] + 1
                except KeyError:
                    freqs [lv1] = 1
                try:
                    freqs [lv2] = freqs[lv2] + 1
                except KeyError:
                    freqs [lv2] = 1
        line = fd.readline ()

    sorted = sortFreqs (freqs)
    for lv, f in sorted :
        print '%-65s , %6d' % (lv, f)
    fd.close ()
    

#-- main
def main () :
    # get filename
    if (len(sys.argv) <= 1) :
        print_usage ();
    else :
        filename = sys.argv[1]
        print 'reading warnings from ' + filename + '\n'

    printLvalFreq (filename)
    sys.exit (0)


#-- go!        
main ()
