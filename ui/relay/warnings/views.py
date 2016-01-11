# Create your views here.

from relay.warnings.models import *
from django.shortcuts import render_to_response, get_object_or_404
from django.http import HttpResponseRedirect, Http404
from django.views.generic.list_detail import object_list, object_detail

def code_list(*args, **kwargs):
    return object_list(*args, **kwargs)

def code_detail(*args, **kwargs):
    return object_detail(*args, **kwargs)

def run_detail(*args, **kwargs):
    return object_detail(*args, **kwargs)

def race_warn_list(request, run_id):
    args =[request]
    kwargs = {}
    try:
        r = Run.objects.get(id=run_id)
        kwargs['queryset'] = Race_cluster.objects.filter(run = r)
        kwargs['paginate_by'] = 50
        return object_list(*args, **kwargs)
    except KeyError, Run.DoesNotExist:
        raise Http404


def warn_detail(request, object_id):
    warn = Race_clusters.objects.get(id=object_id)
    labels = warn.races.first_race().labels().all
    return object_detail(request, queryset=warn, 
                         extra_context={'labels' : labels,
                                        'someInt' : 10})


def search(request, run_id):
    from relay.warnings.search import SearchForm
    if request.method == 'POST':
        form = SearchForm(request.POST)
        if form.is_valid():
            # Do form processing here...
            raise Http404
    form = SearchForm()
    print run_id
    r = Run.objects.get(id = run_id)
    rcs = Race_cluster.objects.filter(run=r)
    return render_to_response('warnings/search.html', 
                              {'form': form , 'run' : r , 'races' : rcs ,
                               'run_id' : run_id})

