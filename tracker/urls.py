# django imports
from django.conf.urls.defaults import patterns
from phermes.tracker.views import index
from phermes.tracker.views import logout
from phermes.tracker.views import tracker
from phermes.tracker.views import list_devices
from phermes.tracker.views import add_device
from phermes.tracker.views import edit_device
from phermes.tracker.views import del_device
from phermes.tracker.views import list_markers

urlpatterns = patterns('',
    # index
    (r'^$', index),

    # tracker accounts urls
    (r'^logout/$', logout),     # exit from system
    
    # tracker main urls
    (r'^tracker/$', tracker),           # main window of application

    ### AJAX

    # tracker AJAX urls
    (r'^listdevices/$', list_devices),         # get list of devices
    (r'^adddevice/$',   add_device),           # add new device
    (r'^editdevice/$',  edit_device),          # edit info about device
    (r'^deldevice/$',   del_device),           # del info device
    (r'^listmarkers/$', list_markers),         # get list of devices
)