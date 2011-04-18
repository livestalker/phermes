# django imports
from django.conf.urls.defaults import patterns
from phermes.tracker.views import index
from phermes.tracker.views import logout
from phermes.tracker.views import tracker
from phermes.tracker.views import list_devices

urlpatterns = patterns('',
    # index
    (r'^$', index),

    # tracker accounts urls
    (r'^logout/$', logout),     # exit from system
    
    # tracker main urls
    (r'^tracker/$', tracker),           # main window of application

    # tracker AJAX urls
    (r'^listdevices/$', list_devices),          # get list of devices
)