from django.conf.urls.defaults import *

urlpatterns = patterns(
    '',
    (r'^relay/admin/', include('django.contrib.admin.urls')),
    (r'^relay/warnings/', include('relay.warnings.urls')),
    )
