#!/usr/bin/env python

""" 
Module to transfer summary files from and to FW Grid. Create new logs to
indicate new locations as well
"""


import sys
import re
import os



#------------------------ Assumed conventions / formats ------------

ssh_path="/usr/bin/ssh"
scp_path="/usr/bin/scp"

# see distributed.ml for log format/conventions
fname_re = re.compile('f_(\d+)')
field_sep = '$'
fwgrid = "fwg-cs0.ucsd.edu"


# TODO: make this less hardcoded
tempTar = "/tmp/temp.tar"
fwgScratch = "/scratch/jvoung"
fwgDest = "/state/partition2/jvoung/s"
user = "jvoung"



def logname_to_fkey (fname):
    """
    convert function log file name to function/summary 
    (see distributed.ml for conventions)
    """
    m = fname_re.match(fname)
    assert m
    return m.group(1)

def fkey_to_logname (fkey):
    """
    convert an fkey to the name of the log file which holds its summary 
    storage location, etc.
    """
    return 'f_' + str(fkey)

def fkey_to_sumprefix (fkey):
    """
    convert an fkey to the name of the summary file (at least a prefix of it)
    """
    return str(fkey)


def parseLogLine (line):
    """
    parse the line: get the IP, username, directory, and function key of summ.
    """
    return line.split(field_sep)



def writeNewLog (newLD, logname, ip, user, newPath):
    """
    write a new log entry to indicate new location of summary
    """
    try:
        fname = os.path.join(newLD, logname)
        fd = open (fname, 'w')
        fd.write(ip + field_sep + user + field_sep + newPath)
        fd.close()
    except IOError:
        print "Couldn't write new log " + logname + "\n"
        raise


class CopyError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)


#--------------------------------------------------------------
# Copy from FW Grid

def sshCopy(user, rhost, src, dest):
    """ 
    Fork a child process and have it scp files
    1) ssh to front end
    2) copy from inner fwgrid node to front end
    3) copy from front end to local disk

    Crazy Example:
    ssh jvoung@fwg-cs0.ucsd.edu 'scp jvoung@10.1.254.146:/state/partition1/jvoung/s/40502.\* . && tar -czf temp.tar.gz 40502.* && rm 40502.*'
    scp jvoung@fwg-cs0.ucsd.edu:~/temp.tar.gz . && tar -xzf temp.tar.gz -C /tmp && rm temp.tar.gz

Example usage:
sshCopy("jvoung", "10.1.254.146", "/state/partition2/jvoung/s/405133", "/tmp")
    """
    #TODO: probably shorter if the first copy also used os.system("ssh...")
    pid = os.fork()
    if pid == 0:
        # If Child; execute first ssh copy
        basefiles = os.path.basename(src)
        scp1cmd = "scp " + user + "@" + rhost + ":" + src + ".\* . && tar -czvf temp.tar.gz " + (basefiles + ".*") + " && rm " + basefiles + ".*"
        os.execv(ssh_path, [ssh_path, "-l", user, fwgrid, scp1cmd])
    else:
        # parent waits for completion
        _, retVal = os.waitpid(pid, 0)
        if retVal <> 0:
            print "Error doing ssh copy to frontend for " + src + "\n"
            assert 0
        else:
            #first scp is done, do a second one
            scp2cmd = user + "@" + fwgrid + ":~/temp.tar.gz . && tar -xzvf temp.tar.gz -C " + dest + " && rm temp.tar.gz"
            if os.system("scp " + scp2cmd) <> 0:
                print "Error doing scp from frontend for " + src + "\n"
                assert 0
    return



def doCopy (fkey, ip, user, oldPath, newPath):
    """
    do the scp to copy summary into the new directory
    """
    sum_name = fkey_to_sumprefix(fkey)
    src = os.path.join(oldPath, sum_name)
    sshCopy(user, ip, src, newPath)


def bulkCopy (ip, user, oldPath, newPath):
    """
    # try to tar everything in the directory 2 hops away and bring it back
    # the directory structure will still be there... so we need to unwind that

    ssh jvoung@fwg-cs0.ucsd.edu 'ssh jvoung@10.1.254.146 'tar czf - -C "/state/partition1/jvoung/s/ ."'' | `cd /tmp ; tar xzf -`
    """
    firstSSH = "ssh %s@%s " % (user, fwgrid)
    secondSSH = "'ssh %s@%s 'tar czf - -C \"%s .\"'' " % (user, ip, oldPath)
    untar = "| `cd %s ; tar xzf -`" % (newPath)
    fullCMD = firstSSH + secondSSH + untar
    if os.system(fullCMD) <> 0:
        raise CopyError ("failed to copy from " + ip)
    return


# TODO: use bulkCopy instead: search through logs for all the 
# (user, srcIP, srcPath) combos and bulk copy from each. 
# If there are multiple copies of the "same" summary, update
# the log with the last copied version


# copy summaries and log file for a function (assuming summaries are on
# fwgrid right now). The remote summary files are copied and the new
# log entry points to the summary's new location
#-- see "distributed.ml" for the log file format. example:
#       127.0.0.1$user$/path/
def copyLogFile (oldLogDir, newLogDir, targetDir, fname) :
    try:
        fd = open (os.path.join(oldLogDir, fname), 'r')
    except IOError:
        sys.stderr.write('Can\'t open file ?\n')
        raise
    else:
        line = fd.readline().strip()
        # parse the log information
        ip, user, path = parseLogLine (line)
        fd.close ()
        # do the copy
        fkey = logname_to_fkey(fname)
        doCopy (fkey, ip, user, path, targetDir)
        # write a new log entry
        writeNewLog (newLogDir, fname, ip, user, targetDir) 
    return



#------------------------------------------------------------
# Copy to FW Grid

def stringOfList(list, sep=" "):
    """ Convert a list of items to a string of items, separated by sep """
    result = ""
    for it in list:
        result += sep + str(it)
    return result


def tarList (dest, listOfFiles):
    """ Tar everything in the listOfFiles to the tar file specified as dest """
    fileArgs = stringOfList(listOfFiles)
    print "tar'ing "
    if os.system("tar -cvf " + dest + " " + fileArgs) <> 0:
        print "Error tar'ing to " + dest + "\n"
        assert 0
        # would be nice to be able to pick up where we left off...
    return


def copyTo (srcTar, destNode, destPath):
    """ Copy the srcTar to front-end, then copy from there to 
    the destNode's destPath, then untar. Finally, clean up the 
    two intermediate tar copies. Example:

    scp /tmp/temp.tar jvoung@fwg-cs0.ucsd.edu:/scratch/jvoung/. && ssh jvoung@fwg-cs0.ucsd.edu 'scp /scratch/jvoung/temp.tar jvoung@fwg-c4-18:/state/partition2/jvoung/s/. && ssh jvoung@fwg-c4-18 "cd /state/partition2/jvoung/s/ ; tar -xvf temp.tar; rm temp.tar" && rm /scratch/jvoung/temp.tar'

    fwg-c4-18
    10.1.254.189
    """
    baseTar = os.path.basename(srcTar)
    firstScp = "scp %s %s@%s:%s/. && " % (srcTar, user, fwgrid, fwgScratch)
    secondScp = ("ssh %s@%s 'scp %s/%s %s@%s:%s/. && " % 
                 (user, fwgrid, fwgScratch, baseTar,
                  user, destNode, destPath))
    untarAndClean = ("ssh %s@%s \"cd %s ; tar -xvf %s; rm %s\" && " %
                     (user, destNode, destPath, baseTar, baseTar))
    cleanFirst = ("rm %s/%s'" % (fwgScratch, baseTar))
    fullCMD = firstScp + secondScp + untarAndClean + cleanFirst
    if (os.system(fullCMD) == 0):
        raise CopyError ("failed to copy to " + destNode)
    return



def copyAllTo (fwgList, files, newLogPath):
    """
    Look at all the summaries in the source directory. Partition them and
    send the partitions to different nodes in fwgList. Generate a new set
    of logs that point to these new summaries in the directory newLogPath.
    """
    # try to determine how many to copy to each fwgrid node
    # be careful about nodes that are down
    nodesLeft = len(fwgList)
    totalFiles = len(files)
    curFile = 0
    # filesLeft = totalFiles - curFile
    filesPerNode = ((totalFiles - curFile) / nodesLeft) + 1 # round up
    for node in fwgList:
        nodesLeft -= 1
        if (curFile < filesLeft):
            a_slice = files[curFile:filesPerNode]
            tarList (tempTar, a_slice)
            try:
                copyTo (tempTar, node, fwgDest)
            except CopyError, e:
                # do not advance current file, try again w/ more burden
                print "Couldn't copy to node: %s" % (str(e))
                filesPerNode = ((totalFiles - curFile) / nodesLeft) + 1
                pass
            # assume all of them made it, record copied files in the log
            for f in a_slice:
                writeNewLog (newLogPath, f, node, user, fwgDest)
            # advance the curFile index
            curFile += filesPerNode
        else:
            # done!
            pass
    # Did we run out of nodes or did we really finish?
    if (curFile < filesLeft):
        print "Didn't finish -- remaining: %s" % (stringOfList(files[curFile:]))
    return


def copyAllDirTo (fwgList, src, newLogPath):
    # first chdir so that tar won't keep the directory structure
    os.chdir(src)
    files = os.listdir(".")
    copyAllTo (fwgList, files, newLogPath)


def generateFWGList (minI = 7, maxI = 13, minJ = 3, maxJ = 23):
    """ Generate a list of fwgrid IPs that can be used """
    return ["fwg-c%s-%s" % (i, j) 
            for i in range(minI, maxI) 
            for j in range(minJ, maxJ)]



#----------------------- Entry point ---------------------

usage = 'usage: %prog [options] old_log_dir new_log_dir storage_dir\n'

#-- usage
def print_usage () :
    print usage 
    sys.exit (127)

# TODO: support copy in both directions



#-- main
def main () :
    usage = 'usage: %prog [options] old_log_dir new_log_dir storage_dir\n'
    parser = optparse.OptionParser(usage=usage)
    parser.add_option("-f", "--from", action="store_true", dest="copyfrom",
                      help="copy from fw grid to the storage dir")
    parser.add_option("-t", "--to", action="store false", dest="copyfrom",
                      help="copy to fw grid (files from the storage dir)", metavar="FILE")
    parser.add_option("-i", "--ip_file", dest="ip_file",
                      help="write ip to given file", metavar="FILE")
    (options, args) = parser.parse_args()
    
    if options.hostname :
        hostname = options.hostname
    elif options.fwg_file :
        hostname = host_from_log (options.fwg_file)
    else :
        parser.error ("must supply hostname w/ one of the options")

    if (len(sys.argv) <= 3) :
        print_usage ();
    else :
        logDir = sys.argv[1]
        newLogDir = sys.argv[2]
        targetDir = sys.argv[3]
        print 'reading logs from ' + logDir + '\n'
        print 'making new logs in ' + newLogDir + '\n'
        print 'copying summaries to ' + targetDir + '\n'
    
    for roots, dirs, files in os.walk(logDir):
        for f in files:
            copyLogFile (logDir, newLogDir, targetDir, f)
    print "Done!\n"
    sys.exit (0)


#-- go!        
main ()
