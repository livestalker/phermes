# django imports
from django.conf.urls.defaults import patterns
from pgermes.tracker.views import index
from pgermes.tracker.views import logout
from pgermes.tracker.views import tracker
from pgermes.tracker.views import list_devices

urlpatterns = patterns('',
    # index
    (r'^$', index),

    # tracker accounts urls
    (r'^logout/$', logout),     # exit from system
    
    # tracker main urls
    (r'^tracker/$', tracker),           # main window of application

    # tracker AJAX urls
    (r'^list/$', list_devices),          # get list of devices
)