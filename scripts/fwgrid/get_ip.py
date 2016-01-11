#!/usr/bin/python

#-- get a hostname (from arg or by parsing the fwgrid logs),
#   then write the ip to a file


import sys
import optparse
import socket

#-- parse fwgrid file :
def host_from_log (logname) :
    print 'reading hostname from ' + logname + '\n'
    fd = open (logname, "r")
    fd.readline()
    name = fd.readline().strip()
    print 'read (' + name + ') as the hostname\n'
    return name
    
#-- main
def main () :
    # get hostname
    usage = "usage: %prog [options]"
    parser = optparse.OptionParser(usage=usage)
    parser.add_option("-o", "--host", dest="hostname",
                      help="supply hostname directly")
    parser.add_option("-f", "--fwg_log", dest="fwg_file",
                      help="parse fwg log file for hostname", metavar="FILE")
    parser.add_option("-i", "--ip_file", dest="ip_file",
                      help="write ip to given file", metavar="FILE")
    (options, args) = parser.parse_args()
    
    if options.hostname :
        hostname = options.hostname
    elif options.fwg_file :
        hostname = host_from_log (options.fwg_file)
    else :
        parser.error ("must supply hostname w/ one of the options")
    
    # get the ip
    ip = socket.gethostbyname (hostname)

    print ip

    # overwrite old ip file w/ the incremented value
    if options.ip_file :
        ipf = options.ip_file
    else :
        ipf = "server_ip.txt"
    fd = open (ipf, "w+")
    fd.write (ip)
    fd.close ()
    sys.exit (0)


#-- go!        
main ()
