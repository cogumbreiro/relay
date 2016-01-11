from django.db import models
from django.db.models import permalink
import re
import sys

def warn(msg):
    sys.stderr.write(msg)

class Code_base(models.Model):
    program_name = models.CharField(max_length=50, core=True)
    version = models.CharField(max_length=40, core=True)
    compile_options = models.CharField(max_length=100)
    def __str__(self):
        return "%s v.%s" % (self.program_name, self.version)
    def get_absolute_url(self):
        return ('relay.warnings.views.code_detail', [str(self.id)])
    get_absolute_url = permalink(get_absolute_url)
    class Admin:
        # Admin options go here
        pass

class Note(models.Model):
    explaination = models.TextField(core=True)
    last_updated = models.DateTimeField(core=True, auto_now=True)
    def __str__(self):
        return "%s %s" % (self.explaination, self.last_updated)
    class Admin:
        # Admin options go here
        pass

class Label(models.Model):
    label = models.CharField(max_length=50, core=True)
    example = models.TextField(core=True)
    def __str__(self):
        return self.label
    class Admin:
        # Admin options go here
        pass

def first_labels(n, ls):
    reduce(lambda x, y: x + ", " + y, ls.all()[0:n], "")

class Function(models.Model):
    cil_id = models.IntegerField(core=True, db_index=True)
    name = models.CharField(max_length=50, core=True)
    labels = models.ManyToManyField(Label, filter_interface=models.VERTICAL)
    program = models.ForeignKey(Code_base, limit_choices_to={})
    def __str__(self):
        return "%s (%d)" % (self.name, self.cil_id)
    def first_labels(self):
        return first_labels(2, self.labels)
    class Admin:
        list_display = ('cil_id', 'name', 'program', 'first_labels')

class Program_point(models.Model):
    file_name = models.CharField(max_length=100, core=True)
    line_num = models.IntegerField(core=True, db_index=True)
    parent_function = models.ForeignKey(Function,
                                        null=True, limit_choices_to={})
    def __str__(self):
        return "%d in %s" % (self.line_num, self.file_name)
    class Admin:
        # Admin options go here
        pass
    
class Lval(models.Model):
    var_id = models.IntegerField(null=True, db_index=True)
    printed = models.CharField(max_length=100, core=True)
    rep_size = models.IntegerField(null=True)
    declared_at = models.ForeignKey(Program_point, null=True)
    is_global = models.BooleanField()
    def __str__(self):
        s = self.printed
        if (self.var_id):
            s += " (%d)" % self.var_id
        if (self.rep_size):
            s += " |%d|" % self.rep_size
        return s
    class Admin:
        # Admin options go here
        pass

class Call_path(models.Model):
    root_function = models.ForeignKey(Function, limit_choices_to={})
    spawn_site = models.ForeignKey(Program_point,
                                   related_name="spawns", limit_choices_to={})
    empty_ls = models.ForeignKey(Program_point,
                                 related_name="empty_ls", null=True,
                                 limit_choices_to={})
    # not including the edges right now...
    def __str__(self):
        return self.root_function.__str__() + " -> ..."
    def program(self):
        return str(self.root_function.program)
    class Admin:
        list_display = ('program', 'root_function', 'spawn_site')


class Call_edge(models.Model):
    path = models.ForeignKey(Call_path, limit_choices_to={})
    caller = models.ForeignKey(Function, related_name="is_caller",
                               limit_choices_to={})
    callee = models.ForeignKey(Function, related_name="is_callee",
                               limit_choices_to={})
    def __str__(self):
        return str(self.caller) + " -> " + str(self.callee)
    class Admin:
        # Admin options go here
        pass

class Access(models.Model):
    lval = models.ForeignKey(Lval, related_name="reads_writes")
    accessed_through = models.ForeignKey(Call_path)
    occurs_at = models.ForeignKey(Program_point)
    locks = models.ManyToManyField(Lval, filter_interface=models.VERTICAL)
    def __str__(self):
        return str(self.lval) + " @ " + str(self.occurs_at)
    def has_lock(self):
        return len(self.locks.all()[:1]) != 0
    has_lock.boolean = True
    class Admin:
        list_display = ('lval', 'occurs_at', 'has_lock')
        search_fields = ['occurs_at']

class Run(models.Model):
    time_of_run = models.DateTimeField(editable=True, auto_now_add=True)
    code = models.ForeignKey(Code_base, limit_choices_to={})
    changes_to_analysis = models.TextField(core=True)
    analysis_settings = models.TextField(core=True)
    def __str__(self):
        return str(self.code) + " " + str(self.time_of_run)
    def get_absolute_url(self):
        return ('relay.warnings.views.run_detail', [str(self.id)])
    get_absolute_url = permalink(get_absolute_url)
    class Admin:
        list_display = ('id', 'code', 'time_of_run')
        list_filter = ('code', 'time_of_run')

class Race(models.Model):
    access1 = models.ForeignKey(Access, core=True, related_name="racy1")
    access2 = models.ForeignKey(Access, core=True, related_name="racy2")
    note = models.ForeignKey(Note, core=True, null=True)
    labels = models.ManyToManyField(Label, filter_interface=models.VERTICAL)
    def __str__(self):
        return str(self.access1) + " [X] " + str(self.access2)
    def first_labels(self):
        return first_labels(2, self.labels)
    def add_label(self, label):
        self.labels.add(label)
    def remove_label(self, label):
        self.labels.remove(label)
    class Admin:
        list_display = ('access1', 'access2', 'first_labels')

class Race_cluster(models.Model):
    races = models.ManyToManyField(Race, filter_interface=models.VERTICAL)
    run = models.ForeignKey(Run)
    cluster_id = models.IntegerField(null=True, core=True)
    def program(self):
        return str(self.run.code)
    def first_race(self):
        return str(self.races.all()[0])
    def get_absolute_url(self):
        return ('relay.warnings.views.warn_detail', [str(self.id)])
    get_absolute_url = permalink(get_absolute_url)
    def add_label(self, label):
        for r in self.races.all():
            r.add_label(label)
    def remove_label(self, label):
        for r in self.races.all():
            r.remove_label(label)
    class Admin:
        list_display = ('program', 'run', 'first_race')
        list_filter = ('run',)


#---- Constructors that either get old matches, or creates new objs -----

def getCodeBase(name, ver, opt):
    return Code_base.objects.get_or_create (program_name=name, version=ver, compile_options = opt)

def getFunc(c_id, n, prog):
    c_id = int(c_id)
    return Function.objects.get_or_create (cil_id=c_id, name=n, program=prog)

def findFunc(c_id, prog):
    try:
        c_id = int(c_id)
        f = Function.objects.get(cil_id=c_id, program=prog)
        return f
    except:
        warn('Function not found %d\n' % c_id)
        return None

def getPP(f, line, parent):
    args = {'line_num' : int(line), 'file_name' : f}
    # NULL != NULL in SQL sucks...
    if (parent == None):
        args['parent_function__isnull'] = True
    else:
        args['parent_function'] = parent
    obj, created = Program_point.objects.get_or_create(**args)
    return obj

def getLval(vid, p_rep, size, decl, glob):
    # NULL != NULL in SQL sucks...
    args = {'printed' : p_rep , 'is_global' : glob }
    if (vid == None):
        args['var_id__isnull'] = True
    else:
        args['var_id'] = int(vid)
    if(size == None):
        args['rep_size__isnull'] = True
    else:
        args['rep_size'] = int(size)
    if(decl == None):
        args['declared_at__isnull'] = True
    else:
        args['declared_at'] = decl
    obj, created = Lval.objects.get_or_create(**args)
    return obj


def getCallpath(root, spawn, empty_at, edges):
    found = None
    args = { 'root_function' : root, 'spawn_site' : spawn }
    filt_args = {}
    create_args = {}
    filt_args.update(args)
    if (empty_at == None):
        filt_args['empty_ls__isnull'] = True
    else:
        filt_args['empty_ls'] = empty_at
    matches = Call_path.objects.select_related().filter(**filt_args)
    edges.sort()
    # see which of the old call paths have the same set of edges
    for o in matches:
        db_edges = Call_edge.objects.filter(path=o).order_by(
            'caller', 'callee')
        e = [(e.caller, e.callee) for e in db_edges]
        if (e == edges) :
            found = o
            break
    # if it didn't find any call paths w/ the same set of edges
    if (not found) :
        create_args.update(args)
        create_args['empty_ls'] = empty_at
        found = Call_path.objects.create(**create_args)
        for (f1, f2) in edges :
            Call_edge.objects.create(path=found, caller=f1, callee=f2)
    return found

#-------- Access factories

def matchLocksAccesses (accessMatches, locks):
    found = None
    for old in accessMatches:
        db_l = list(old.locks.all())
        if (db_l == locks) :
            found = old
            break
    if (not found) :
        found = Access.objects.create(lval=lv,accessed_through=cp,occurs_at=pp)
        found.locks = locks
        found.save()
    return found


def getAccess(lv, cp, pp, locks):
    # make sure lists are in sorted order before comparing
    locks.sort()
    matches = Access.objects.select_related().filter(lval=lv, accessed_through=cp, occurs_at=pp)
    # see which of the old accesses have the same set of locks
    found = matchLocksAccesses (matches, locks)
    return found


def createAccess(lv, cp, pp, locks):
    acc = Access.objects.create(lval=lv, accessed_through=cp, occurs_at=pp)
    acc.locks = locks
    acc.save()
    return acc


def getAccesses(lv, cp, pps, locks):
    res = []
    locks.sort()
    outer_matches = Access.objects.filter(lval=lv, accessed_through=cp)
    for pp in pps:
        matches = outer_matches.select_related().filter(occurs_at=pp)
        # see which of the old accesses have the same set of locks
        found = matchLocksAccesses (matches, locks)
        res.append(found)
    return res

#----------

def getRace(acc1, acc2):
    new, created = Race.objects.get_or_create(access1=acc1, access2=acc2)
    return new    

def createRace(acc1, acc2):
    race = Race(access1=acc1, access2=acc2)
    race.save()
    return race

# Not useful -- use createRaceCluster instead
def getRaceCluster(races, _run):
    found = None
    races.sort()
    matches = Race_cluster.objects.select_related().filter(run = _run)
    for old in matches:
        o_r = list(old.races.all())
        if (o_r == races):
            found = old
            break
    if (not found):
        found = Race_cluster.objects.create(run = _run)
        found.races = races
        found.save()
    return found

# Just create a new cluster and allow duplicates (won't happen, unless)
# you try to re-use an old "run"
def createRaceCluster(cid, races, run):
    # None != NULL in the Django mapping sucks...
    r = Race_cluster.objects.create(run = run)
    r.races = races
    if (cid != None):
        r.cluster_id = int(cid)
    else:
        print "Didn't have cluster_id"
    r.save()
    return r


# Add labels to races
def getLabel(labName):
    lab, created = Label.objects.get_or_create(label=labName)
    return lab


# TODO add label to any race clusters that match a certain location
