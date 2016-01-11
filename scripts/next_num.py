#!/usr/bin/env python

#-- read an integer from a text file, increment it and overwrite the old file


import sys

#-- usage
def print_usage () :
    print 'usage: next_num file\n'
    sys.exit (127)
    
#-- main
def main () :
    # get filename
    if (len(sys.argv) <= 1) :
        print_usage ();
    else :
        filename = sys.argv[1]
        print 'incrementing value in ' + filename + '\n'

    # get value from file
    try:
        fd = open (filename, 'rw+')
    except IOError:
        sys.stderr.write('Can\'t open file, resetting\n')
        fd = open (filename, 'w+')
        val = 0
    except:
        sys.stderr.write ('Unexpected error:', sys.exc_info()[0])
        raise
    else:
        try:
            val = int (fd.readline().strip())
        except ValueError:
            sys.stderr.write ('Couldn\'t convert data to an int. Resetting')
            val = 0

    # overwrite file w/ the incremented value
    fd.seek(0)
    fd.truncate()
    fd.write (str (val + 1) + "\n")
    
    fd.close ()
    sys.exit (0)


#-- go!        
main ()
