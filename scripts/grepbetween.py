#!/usr/bin/env python

# Does a kind of grep that prints lines between two given regular expressions
# (where between is inclusive)

import re
import sys
import optparse

#-- main stuff
def parsePositionalArgs (argv, parser) :
    if (len (argv) <= 1) :
        parser.error("must supply more args")
    else :
        sp = argv[0]
        ep = argv[1]
        if (len (argv) >= 3) :
            fn = argv[2]
            infile = open (fn, "r")
        else :
            infile = sys.stdin
    return (sp, ep, infile)
    

def doGrep (sp, ep, infile, options):
    inMatch = False
    endMatches = 0
    if options.endN :
        endMTarget = options.endN
    else :
        endMTarget = 0
    for line in infile:
        if inMatch :
            print line, 
            matchEnd = ep.match(line)
            if matchEnd :
                endMatches += 1
                if endMatches >= endMTarget :
                    inMatch = False
        else :
            inMatch = sp.match(line)
            if inMatch :
                print line,
                endMatches = 0
    return

def main ():
    usage = 'usage: %prog [opts] startPat endPat file'
    parser = optparse.OptionParser(usage=usage)
    parser.add_option("-n", "--endN", action="store", type="int", dest="endN",
                      help="stop only after n matches of end pattern")
    options, args = parser.parse_args()
    sp, ep, infile = parsePositionalArgs(args, parser)
    doGrep (re.compile(sp), re.compile(ep), infile, options)
    return 0

#-- go!
main()
