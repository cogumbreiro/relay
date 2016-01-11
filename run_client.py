# TODO: allow more arguments to be sent to the analysis

tempdir = "/tmp/"
statusFile = ""

import os
import commands
import sys   

def printUsage():
    print "usage: %prog cmd cgDir [tempStatusFile]"
    sys.exit(127)

#reserve a file to store checkpoint / status information
def reserveCheck (tempdir):
    statusFile= commands.getoutput("tempfile -d %s -p temp.status" % tempdir)
    print("echo reserved status file: %s" % statusFile)
    return statusFile


# clear the given file (should be the temp status file)
def clearCheck (statusFile):
    os.remove(statusFile)
    print("cleared status file: %s" % statusFile)


def runAnalysis (exe, cgDir, statusFile):
    print("running %s on %s" % (exe, cgDir))
    return os.system ("%s -cg %s -u ravi -st %s >> log 2>&1" % 
                      (exe, cgDir, statusFile))


def loopTryAnalysis ():
    numargs = len(sys.argv)
    print sys.argv[0]
    if (numargs < 3 or numargs > 4):
        printUsage()
    if numargs == 4 :
        statusFile = sys.argv[3]
    else:
        statusFile = reserveCheck ("/tmp/")
    
    if os.path.exists(statusFile):
        print ("using status file: %s" % statusFile)
    else:
        print ("asked to use status file %s and it doesn't exist" % statusFile)
        sys.exit(127)

    cmd = sys.argv[1]
    cgDir = sys.argv[2]
    maxtries=1
    tries=0
    retval=127

    while (retval != 0):
        retval = runAnalysis(cmd, cgDir, statusFile)
        tries = tries + 1
        if tries > maxtries:
            print ("Exhausted tries: %d" % tries)
            break
        
        print("return value is %d" % retval)
        print("ran %d times" % tries)

    clearCheck (statusFile)
    os.system("date >> log")
    return retval

loopTryAnalysis()
