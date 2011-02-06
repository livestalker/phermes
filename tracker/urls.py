# django imports
from django.conf.urls.defaults import patterns
from pgermes.tracker.views import account
from pgermes.tracker.views import login
from pgermes.tracker.views import logout
from pgermes.tracker.views import signup
from pgermes.tracker.views import tracker
from pgermes.tracker.views import list_devices

urlpatterns = patterns('',

    # tracker accounts urls
    (r'^login/$', login),       # login in system
    (r'^logout/$', logout),     # exit from system
    (r'^account/$', account),   # account info
    (r'^signup/$', signup),     # registration in system

    # tracker main urls
    (r'^tracker/$', tracker),           # main window of application

    # tracker AJAX urls
    (r'^list/$', list_devices),          # get list of devices
)