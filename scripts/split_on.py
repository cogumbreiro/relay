#!/usr/bin/env python

# Takes a file and splits it into N files if there are N "divider lines" 
# (divider line defined by a given pattern)


import re
import sys
import optparse

#-- main stuff
def parsePositionalArgs (argv, parser) :
    if (len (argv) <= 2) :
        parser.error("must supply more args")
    else :
        div = argv[0]
        inFname = argv[1]
        outFname = argv[2]
        infile = open (inFname, "r")
    return (div, infile, outFname)

def makeOut (prefix, n):
    target = prefix + str(n)
    print "Writing to file %s\n" % target
    return (open (target, 'w+'))

def doSplit (div, infile, outFprefix):
    matchCount = 0
    outfile = makeOut (outFprefix, matchCount)
    for line in infile:
        if div.match(line) :
            outfile.close ()
            matchCount += 1
            outfile = makeOut (outFprefix, matchCount)
        outfile.write(line)
    return

def main ():
    usage = 'usage: %prog dividerPat infile outdir'
    parser = optparse.OptionParser(usage=usage)
    options, args = parser.parse_args()
    div, i, o = parsePositionalArgs(args, parser)
    doSplit (re.compile(div), i, o)
    return 0

#-- go!
main()
