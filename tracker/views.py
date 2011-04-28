from django.http import HttpResponse, HttpResponseRedirect
from django.core.context_processors import csrf
from django.contrib import auth
from django.shortcuts import render_to_response
from django.utils.simplejson.encoder import JSONEncoder
from phermes.tracker.models import Device
from phermes.tracker.models import MapMarker

# account functions
def index(request):
    """
    Main page. If request not AJAX open index page
    """
    c = {}
    # TODO csrf
    #c.update(csrf(request))
    if request.method == 'POST':
        if request.is_ajax():
            username = request.POST.get('username', '')
            password = request.POST.get('password', '')
            json = {}
            user = auth.authenticate(username = username, password = password)
            if user is not None and user.is_active:
                auth.login(request, user)
                json['success'] = True
                return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
            else:
                json['success'] = False
                json['errors'] = {}
                json['errors']['reason'] = 'Login failed. Try again.'
                return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    return render_to_response('index.html', c)

def logout(request):
    """
    Logout from system
    """
    auth.logout(request)
    return HttpResponseRedirect('/')

# tracker function
def tracker(request):
    if not request.user.is_authenticated():
        return HttpResponseRedirect('/')
    else:
        return render_to_response('tracker.html')

## AJAX

# tracker AJAX functions: get list of devices
def list_devices(request):
    json = []
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            for d in Device.objects.filter(user_id = request.user.id):
                json.append({'device_id' : d.device_id, 'imei' : d.imei, 'name' : d.name, 'text' : d.text, 'long' : str(d.long), 'lat' : str(d.lat), 'ts_time' : str(d.ts_time)})
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: add new device
def add_device(request):
    json = []
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            # TODO add functional
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: edit existing device
def edit_device(request):
    json = []
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            # TODO edit functional
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: delete existing device
def del_device(request):
    json = []
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            # TODO del functional
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: load marker list
def list_markers(request):
    json = []
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            for m in MapMarker.objects.all():
                json.append({'marker_id' : m.marker_id, 'width' : m.width, 'height' : m.height, 'url' : m.url, 'name' : m.name})
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')