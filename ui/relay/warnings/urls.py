from django.conf.urls.defaults import *
from relay.warnings.models import *

codebases = {
    'queryset': Code_base.objects.all(),
    }

runs = {
    'queryset': Run.objects.all(),
    }

raceclusters = {
    'queryset': Race_cluster.objects.all(),
    }

races = {
    'queryset': Race.objects.all(),
    }

from relay.warnings import views
from django.contrib import databrowse



urlpatterns = patterns(
    '',
    (r'^$', views.code_list, dict(codebases, paginate_by=30)),
    (r'^dbrowse/(.*)', databrowse.site.root),
    (r'^program/(?P<object_id>\d+)/$', views.code_detail,
     codebases),
    (r'^run/(?P<object_id>\d+)/$', views.run_detail, runs),
    (r'^run/(?P<run_id>\d+)/race_warn/$', views.race_warn_list),
    (r'^race_warn/(?P<object_id>\d+)/$', views.warn_detail,
     raceclusters),
    (r'^run/(?P<run_id>\d+)/search/$', views.search),
    
)

databrowse.site.register(Run)
databrowse.site.register(Race_cluster)
databrowse.site.register(Label)


