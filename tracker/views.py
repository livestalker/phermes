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
            user = auth.authenticate(username=username, password=password)
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
            for d in Device.objects.filter(user_id=request.user.id):
                json.append({'device_id': d.device_id, 'marker_id': d.marker_id_id, 'imei': d.imei, 'name': d.name,
                             'text': d.text, 'long': str(d.long), 'lat': str(d.lat), 'ts_time': str(d.ts_time)})
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: add new device
def add_device(request):
    json = {}
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            imei = request.POST.get('imei', '')
            name = request.POST.get('name', '')
            text = request.POST.get('text', '')
            marker_id = request.POST.get('marker_id', '')
            if marker_id:
                marker = MapMarker.objects.filter(marker_id=marker_id)[0]
            else:
                marker = None
            if len(Device.objects.filter(imei=imei)) > 0:
                json['success'] = False
                json['errors'] = {}
                json['errors']['reason'] = 'Device with this IMEI already exist.'
            else:
                device = Device(imei=imei, name=name, text=text, marker_id=marker, user_id=request.user)
                device.save()
                json['success'] = True
                json['test'] = {}
                json['test']['marker_id'] = marker_id
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: edit existing device
def edit_device(request):
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            # TODO Test if fields changed?
            # Get base parameters of device
            json = {'success' : False}
            imei = request.POST.get('imei', '')
            name = request.POST.get('name', '')
            text = request.POST.get('text', '')
            device_id = request.POST.get('device_id', '')
            # Get device from DB
            # TODO try..catch
            device = Device.objects.get(device_id=device_id, user_id=request.user)
            device.imei = imei
            device.name = name
            device.text = text
            # Get marker map parameter
            marker_id = request.POST.get('marker_id', '')
            if marker_id:
                marker = MapMarker.objects.get(marker_id=marker_id)
                device.marker_id = marker
            try:
                # Save device in DB
                device.save();
                json['success'] = True
            except:
                json['errors'] = {}
                json['errors']['reason'] = 'Error'
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
                json.append(
                        {'marker_id': m.marker_id, 'width': m.width, 'height': m.height, 'url': m.url, 'name': m.name})
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')