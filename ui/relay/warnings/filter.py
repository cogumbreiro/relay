#!/usr/bin/env python

#------------------------------------------------------------------

# Set up environment to work when script is symlink
import sys
import os
sys.path.insert(0, os.getcwd())


# copy Django shell stuff
import django
from django.core.management import setup_environ
try:
    import settings # Assumed to be in the same directory.
except ImportError:
    import sys
    sys.stderr.write("Error: Can't find the file 'settings.py' in the directory containing %r. It appears you've customized things.\nYou'll have to run django-admin.py, passing it your settings module.\n(If the file settings.py does indeed exist, it's causing an ImportError somehow.)\n" % __file__)
    sys.exit(1)

if __name__ == "__main__":
    setup_environ(settings)


#----------------------------------------------------------------


from relay.warnings.stats import *
from relay.warnings.models import *
from relay.warnings.labeled import *
from relay.warnings.loading import *

remainingFILE = "remaining_warnings.txt"
removedFILE = "removed_warnings.txt"
filterStatsFILE = "filter_stats.txt"

# Run the filters, and write out the race cluster ids that were removed 
# and / those that remain. (Funcs for printing the counts is in stats.py)

def write_warnings (fname, warnings):
    """ Write the RELAY-based cluster_ids and the database ids to a file """
    try:
        fd = open (fname, 'w')
        for w in warnings:
            fd.write ("%s\t%s\n" % (w.cluster_id, w.id))
    except IOError:
        print "Error writing to: %s\n" % fname
    finally:
        fd.close ()
    return


def write_filtered(run, removed_file, remaining_file, filter_stats_file):
    """ Filter the warnings from the given run and write the 
        cluster ids of the removed warnings to the removed_file, 
        and the remaining warnings to its file """
    try:
        filterFD = open (filter_stats_file, 'w')
        t = time.time()
        incl = {}
        excl = {}
        origRaces = Race_cluster.objects.filter(run=run)
        filterFD.write("No filters:\t%d\n" % origRaces.count())

        print "Running filters, see %s for stats\n" % filter_stats_file
        print "filter 1"
        races = origRaces.exclude(both_alloc()).distinct()
        filterFD.write ("& not both alloced in thread:\t%d\n" % races.count())

        print "filter 2"
        races = races.exclude(some_alloc()).distinct()
        filterFD.write ("& not some alloced in thread:\t%d\n" % races.count())
        
        print "filter 3"
        blob_size = 20
        races = races.exclude(uses_blob_lt_filter(blob_size)).distinct()
        filterFD.write ("& not blobbier than %d thread:\t%d\n" \
                            % (blob_size, races.count()))
#        races = filter_more(origRaces, all_of_list(
#                [(lval_match_pat1(lval_alloc_re), False),
#                 (lval_match_pat2(lval_alloc_re), False)]), 5)
#        filterFD.write ("& not alloced in thread:\t%d\n" % len(races))

#        print "filter 2"
#        races = filter_more(races, isnt(uses_blob(20)))
#        filterFD.write ("& not blobbier than 20:\t%d\n" % len(races))
        print "Time spent filtering: %.3f" % (time.time() - t)
        print "Now writing results to %s and %s\n" % \
            (removed_file, remaining_file)
        remaining = races
        removed = [ r for r in origRaces if (not r in remaining) ]
        write_warnings(removed_file, removed)
        write_warnings(remaining_file, remaining)
    except IOError:
        print "Error writing to: %s\n" % filter_stats_file
    finally:
        filterFD.close ()
    return


def load_filter_save(callgraph_file):
    """ Load the warnings from the callgraph_dir. Filter them, then
        dump the IDs out to a file. Assume the callgraph file is loaded. """
    loadProgInfo(callgraph_file)
    dir = os.path.dirname(callgraph_file)
    run = loadWarnings(dir)
    removeF = os.path.join(dir, removedFILE)
    remainF = os.path.join(dir, remainingFILE)
    filterStF = os.path.join(dir, filterStatsFILE)
    write_filtered (run, removeF, remainF, filterStF)
    return

#----------------------- Entry point ---------------------

usage = 'usage: %prog callgraph_file\n'

def print_usage () :
    print usage 
    sys.exit (127)

def main ():
    if (len(sys.argv) <= 1):
        print_usage()
    load_filter_save(sys.argv[1])
    sys.exit(0)

main()
