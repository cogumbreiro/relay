#!/usr/bin/env python

# Take a set of files w/ a common prefix (e.g., "pre_0, pre_1, ...")
# and reduces that set to its uniq representatives 
# (e.g., if "diff file_0 and file_1" return nothing, only keep file_0)

# warning: not bulletproof at all... 
# also, only for unix as it uses "cp" and "diff"


import glob
import os
import os.path
import optparse

#-- main stuff
def parsePositionalArgs (argv, parser) :
    if (len (argv) <= 1) :
        parser.error("must supply more args")
    else :
        prefix = argv[0]
        outdir = argv[1]
        return (prefix, outdir)


# super simple (not the most efficient) disjoint set
class UF():
    reps = {}
    def find(self, x):
        if self.reps.__contains__(x) :
            next = self.reps[x]
            if x == next :
                return x
            final = self.find(next)
            self.reps[x] = final
            return final
        else :
            self.reps[x] = x
            return x

    def unif(self, a, b):
        arep = self.find(a)
        brep = self.find(b)
        self.reps[b] = arep
        
    def iteritems(self):
        return self.reps.iteritems()


def diff(f1, f2):
    x = os.system("diff --brief %s %s" % (f1, f2))
    return x

def copy(f, targdir):
    basename = os.path.basename(f)
    os.system("cp %s %s" % (f, (os.path.join(targdir, basename))))

def doUniq (prefix, outdir, opts):
    allfiles = glob.glob(prefix + "*")
    reps = UF()
    for f1 in allfiles :
        for f2 in allfiles :
            if reps.find (f1) == reps.find(f2):
                continue
            else:
                if not (diff(f1, f2)):
                    reps.unif(f1, f2)
    alreadyCopied = {}
    for (f, frep) in reps.iteritems():
        if alreadyCopied.__contains__(frep) :
            continue
        else :
            copy(frep, outdir)
            alreadyCopied[frep] = 1
    return
        

def main ():
    usage = 'usage: %prog [opts] prefix outdir'
    parser = optparse.OptionParser(usage=usage)
    options, args = parser.parse_args()
    prefix, outdir = parsePositionalArgs(args, parser)
    print ("Getting uniq files with prefix %s to %s\n" % (prefix, outdir))
    doUniq (prefix, outdir, options)
    return 0

#-- go!
main()
