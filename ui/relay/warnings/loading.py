from relay.warnings.models import *
from datetime import datetime
import sys

def warn(msg):
    sys.stderr.write(msg)

#----Sync labels / notes to all races occuring at a certain location ----
def syncLabels(lab, loc):
    pass

def syncNotes(lab, loc):
    pass


#--------------- 'Bulk-load' code base info & function info -------------
codebase_info_file = 'info.txt'
warnings_file = 'warnings2.xml'

def fieldCheck(field, expected):
    if (field != expected):
        warn('expected %s field, not %s\n' % (expected, field))
        raise AssertionError

def loadCodebase(dir):
    from os.path import join
    cb_file = open (join (dir, codebase_info_file), 'r')
    try:
        field, prog_name = cb_file.readline().split(':')
        fieldCheck(field.strip(), 'name')
        field, version = cb_file.readline().split(':')
        fieldCheck(field.strip(), 'version')
        field, comp_opts = cb_file.readline().split(':')
        fieldCheck(field.strip(), 'options')
        cb, created = getCodeBase(prog_name.strip(), version.strip(), comp_opts.strip())
        if (created) :
            print 'Successfully added code base %s' % cb
        else:
            print 'Code base already existed: %s' % cb
    finally:
        cb_file.close()
    return (cb, created)

def loadProgInfo(callgraph_file):
    from os.path import *
    dir = dirname (callgraph_file)
    cb, created = loadCodebase(dir)
    if (not cb):
        warn ('Could not load code base\n')
        sys.exit(1)
    if (not created):
        warn ('Since code-base already existed, not reloading the callgraph\n')
        return cb
    try:
        print 'Trying to load call graph data from %s' % callgraph_file
        fun_file = open (callgraph_file, 'r')
    except IOError :
        warn ('call graph file not found %s\n' % callgraph_file)
        raise
    try:
        for line in fun_file:
            parts = line.split('$')
            finfo = parts[1].split(':')
            name = finfo[0].strip()
            cil_id = finfo[2]
            func, created = getFunc(cil_id, name, cb)
            if (created) :
                print 'Successfully added function %s' % func
            else:
                print 'Function already existed: %s' % func
    finally:
        fun_file.close()
    return cb
 
    
#------------------ 'Bulk-load' Warnings ---------------

#---- Load from XML

class WarningsParser:
    # make a new one for each code base
    
    codeb = None
    run = None
    num_clusters = 0
    start_time = None
    acc1 = None
    acc2 = None
    races = None
    rw_lval = None
    occurs_at = None
    locks = None
    cluster_atts = None
    cp = None
    cp_atts = None
    spawned_at = None
    emptied_at = None
    pp = None
    pp_atts = None
    lval = None
    lval_atts = None
    call_edges = None
    call_edge_atts = None
    prev_tags = None

    #-- db caches
    funcCache = None
    ppCache = None
    lvalCache = None
    accessCache = None
    callpathCache = None

    def __init__(self):
        self.prev_tags = []
        self.funcCache = {}
        self.ppCache = {}
        self.lvalCache = {}
        self.accessCache = {}
        self.callpathCache = {}
        return

    # --- Cached results (avoid hitting DB)

    def getFunc(self, fid):
        fid = int(fid)
        try:
            return self.funcCache[fid]
        except KeyError:
            f = findFunc(fid, self.codeb)
            self.funcCache[fid] = f
            return f

    def getPP(self, f, line, parent):
        line = int(line)
        try:
            return self.ppCache[(line, f)]
        except KeyError:
            pp = getPP(f, line, parent)
            self.ppCache[(line, f)] = pp
            return pp

    def getLval(self, vid, printed, size, decl, glob):
        if(vid):
            vid = int(vid)
        try:
            return self.lvalCache[(vid, printed)]
        except KeyError:
            lv = getLval(vid, printed, size, decl, glob)
            self.lvalCache[(vid, printed)] = lv
            return lv

    def getAccess(self, lv, cp, pp, locks):
        lock_str = str(locks)
        key = (lv, cp, pp, lock_str)
        try:
            return self.accessCache[key]
        except KeyError:
            acc = createAccess(lv, cp, pp, locks)
            self.accessCache[key] = acc
            return acc

    def getCallpath(self, func, spawn_pp, empty_pp, edges):
        edge_str = str(edges)
        key = (func, spawn_pp, empty_pp, edge_str)
        try:
            return self.callpathCache[key]
        except KeyError:
            cp = getCallpath(func, spawn_pp, empty_pp, edges)
            self.callpathCache[key] = cp
            return cp

    # --- tag-end-Event notifications 

    def final_handler(self, endAttr):
        try:
            handler = getattr (self, "catch_%s_%s" %
                               (endAttr, self.prev_tags[-2]))
            handler()
        except AttributeError:
            pass
        except IndexError:
            warn ("no parent tag for %s\n" % endAttr)
        return

    def start_pp (self, atts):
        self.pp_atts = atts
        return

    def end_pp (self):
        if ('parent' in self.pp_atts):
            parent = self.getFunc(self.pp_atts['parent'])
        else:
            parent = None
        f = self.pp_atts['file'].strip().lower()
        line = self.pp_atts['line']
        self.pp = self.getPP(f, line, parent)
        # see if anyone needs to catch this pp before starting the next tag
        self.final_handler("pp")
        return

    def catch_pp_acc(self):
        self.occurs_at.append(self.pp)
        return

    catch_pp_acc1 = catch_pp_acc
    catch_pp_acc2 = catch_pp_acc

    def start_lval (self, atts):
        self.pp = None
        self.lval_atts = atts
        return

    def end_lval (self):
        vid = self.lval_atts.get('vid', None)
        size = self.lval_atts.get('size', None)
        if (self.lval_atts.get('global', "false").lower() == "true") :
            glob = True
        else :
            glob = False
        self.lval = self.getLval (vid, self.lval_atts['printed'],
                                  size, self.pp, glob)
        self.final_handler("lval")
        return

    def catch_lval_acc(self):
        self.rw_lval = self.lval
        return

    catch_lval_acc1 = catch_lval_acc
    catch_lval_acc2 = catch_lval_acc

    def start_spawned_at(self, atts):
        self.pp = None
        return
        
    def end_spawned_at(self):
        self.spawned_at = self.pp
        return

    def start_empty_at(self, atts):
        self.pp = None
        return
        
    def end_empty_at(self):
        self.emptied_at = self.pp
        return

    def start_call_edge(self, atts):
        self.call_edge_atts = atts
        return

    def end_call_edge(self):
        self.call_edges.append(self.getFunc(self.call_edge_atts['caller']),
                               self.getFunc(self.call_edge_atts['callee']))
        return

    def start_cp(self, atts):
        self.cp = None
        self.cp_atts = atts
        self.spawned_at = None
        self.emptied_at = None
        self.call_edges = []

    def end_cp(self):
        self.cp = self.getCallpath(self.getFunc(self.cp_atts['root']),
                                   self.spawned_at, self.emptied_at,
                                   self.call_edges)
        return

    def start_locks(self, atts):
        self.locks = []
        return

    def end_locks(self):
        return

    def catch_lval_locks(self):
        self.locks.append(self.lval)

    def start_acc1 (self, atts):
        self.occurs_at = []
        self.locks = []
        self.cp = None
        self.rw_lval = None
        self.acc1 = []
        return

    def start_acc2 (self, atts):
        self.occurs_at = []
        self.locks = []
        self.cp = None
        self.rw_lval = None
        self.acc2 = []
        return

    # TODO: try "createAccess" or "getAccesses"... should be a bit better?
    def end_acc1 (self):
        self.acc1 = [self.getAccess(self.rw_lval, self.cp, pp, self.locks)
                     for pp in self.occurs_at]
        return
        
    def end_acc2 (self):
        self.acc2 = [self.getAccess(self.rw_lval, self.cp, pp, self.locks)
                     for pp in self.occurs_at]
        return

    def start_race(self, atts):
        self.races = []
        return

    def end_race(self):
        self.races = [createRace(a1, a2)
                      for a1 in self.acc1
                      for a2 in self.acc2]
        return

    def start_cluster(self, atts):
        self.cluster_atts = atts
        return

    def end_cluster(self):
        cluster_id = self.cluster_atts.get(u'id', None)
        cluster = createRaceCluster(cluster_id, self.races, self.run)
        self.num_clusters += 1
        if (self.num_clusters % 20 == 0):
            print "Total race (clusters) so far: %d" % self.num_clusters
        return

    def start_run(self, atts):
        from time import time
        self.run = Run.objects.create(code = self.codeb)
        self.num_clusters = 0
        self.start_time = time()
        return

    def end_run(self):
        from time import time
        elapsed = time() - self.start_time
        print "Done processing warnings in %.3f secs" % elapsed 
        return

    def set_codebase(self, cb):
        self.codeb = cb
        return


    def pushTag(self, tag):
        self.prev_tags.append(tag)
        return

    def popTag(self):
        self.prev_tags.pop()
        return


#-- end class

# TODO fix this ugliness of a global warnings parser

wp = WarningsParser()

def eltStart (name, atts):
    tag = name.strip().lower()
    try:
        handler = getattr (wp, "start_%s" % tag)
    except AttributeError:
        warn ("Ignoring start XML tag: %s\n" % tag)
        return
    wp.pushTag(tag)
    handler(atts)
    return
    
def eltEnd (name):
    tag = name.strip().lower()
    try:
        handler = getattr (wp, "end_%s" % tag)
    except AttributeError:
        warn ("Ignoring end XML tag: %s\n" % tag)
        return
    handler()        
    wp.popTag()
    return

def handleCharData (data):
    pass

def loadWarnings(dir):
    from os.path import join
    import xml.parsers.expat
    codeb, created = loadCodebase(dir)
    if (not codeb):
        return
    f = open (join (dir, warnings_file), 'r')
    try:
        try:
            p = xml.parsers.expat.ParserCreate()
            wp.set_codebase (codeb)
            p.StartElementHandler = eltStart
            p.EndElementHandler = eltEnd
            p.CharacterDataHandler = handleCharData
            p.ParseFile (f)
        except (xml.parsers.expat.ExpatError, TypeError):
            warn ("loadWarn error: parsing line %s col %s\n" %
                  (p.ErrorLineNumber, p.ErrorColumnNumber))
            raise
    finally:
        f.close()
    return wp.run


#----------- TEST Objects -------------
"""
from relay.warnings.models import *

linux, created = getCodeBase('Linux', '2.6.17', 'make allyesconfig, no module support')
foo, created = getFunc(0, 'foo', linux)
bar, created = getFunc(1, 'bar', linux)
baz, created  = getFunc(2, 'baz', linux)
call_e1 = [(foo, bar), (bar, baz)]
call_e2 = [(bar, baz), (foo, bar)]
pp1 = getPP('main.c', 10, foo)
pp2 = getPP('foo.c', 10, foo)
cp  = getCallpath(foo, pp2, pp1, call_e1)

lv1 = getLval(None, 'REP->v', 1, pp1)
lock1 = getLval(10, 'd->l', None, None)
lock2 = getLval(10, 'd->l2', None, None)
lock3 = getLval(11, 'kernel_sem', None, None)

locks1 = [lock1, lock2, lock3]
locks2 = [lock2, lock1, lock3]
locks3 = [lock3, lock2, lock1]

ac1 = getAccess (lv1, cp, pp1, locks1)
ac2 = getAccess (lv1, cp, pp2, locks2)

run = Run.objects.create()

race1 = getRace(ac1, ac1, run)
race2 = getRace(ac1, ac2, run)
race3 = getRace(ac1, ac1, run)

rc1 = [race1, race2, race3]
rc2 = [race2, race3, race1]

cluster = getRaceCluster(rc1, run)
cluster = getRaceCluster(rc2, run)

from relay.warnings.loading import *
loadWarnings('/home/jan/research/linux-2.6.15/ciltrees')

from relay.warnings.loading import *
loadProgInfo('/home/jan/research/relay-race/nulltests/httpd-2.2.6/ciltrees/calls.steens')
loadWarnings('/home/jan/research/relay-race/nulltests/httpd-2.2.6/ciltrees')
"""

