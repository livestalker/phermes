from django.http import HttpResponse, HttpResponseRedirect
from django.core.context_processors import csrf
from django.contrib import auth
from django.shortcuts import render_to_response
from django.utils.simplejson.encoder import JSONEncoder
import django.utils.simplejson as simplejson
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
    # TODO change True in success
    json = {}
    json['success'] = True
    json['devices'] = []
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            for d in Device.objects.filter(user_id=request.user.id):
                #json['devices'].append({'device_id': d.device_id, 'marker_id': d.marker_id_id, 'imei': d.imei, 'name': d.name,
                #             'text': d.text, 'long': str(d.long), 'lat': str(d.lat), 'ts_time': str(d.ts_time)})
                json['devices'].append({'device_id': d.device_id, 'marker_id': d.marker_id_id, 'imei': d.imei, 'name': d.name,
                             'text': d.text})
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: add new device
def add_device(request):
    json = {}
    json['devices'] = []
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            json_request = simplejson.loads(request.raw_post_data)
            for element in json_request['devices']:
                if 'marker_id' in element:
                    marker = MapMarker.objects.filter(marker_id=element['marker_id'])[0]
                else:
                    marker = None
                if len(Device.objects.filter(imei=element['imei'])) > 0:
                    json['success'] = False
                    json['errors'] = {}
                    json['errors']['reason'] = 'Device with this IMEI already exist.'
                else:
                    device = Device(imei=element['imei'],
                                    name=element['name'],
                                    text=element['text'],
                                    marker_id=marker,
                                    user_id=request.user)
                    device.save()
                    json['success'] = True
                    json['devices'].append({'device_id': device.device_id,
                                            'marker_id': device.marker_id_id,
                                            'imei': element['imei'],
                                            'name': element['name'],
                                            'text': element['text']})
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')

# tracker AJAX functions: edit existing device
def edit_device(request):
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            json_request = simplejson.loads(request.raw_post_data)
            json = {'success' : False}
            json['devices'] = []
            for element in json_request['devices']:
                device = Device.objects.get(device_id=element['device_id'], user_id=request.user)
                device.name = element['name']
                device.text = element['text']
                if 'marker_id' in element:
                    marker = MapMarker.objects.get(marker_id=element['marker_id'])
                    device.marker_id = marker
                    try:
                        # Save device in DB
                        device.save();
                        json['success'] = True
                        json['devices'].append({'device_id': device.device_id,
                                                'marker_id': device.marker_id_id,
                                                'imei': element['imei'],
                                                'name': element['name'],
                                                'text': element['text']})
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

def current_geo(request):
    json = {'success': False}
    if not request.user.is_authenticated():
        return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    else:
        if request.method == 'POST' and request.is_ajax():
            json['currentgeos'] = []
            for d in Device.objects.filter(user_id=request.user.id):
                cg = d.current_geo
                json['currentgeos'].append({'device_id': cg.id, 'lat': str(cg.lat), 'lng': str(cg.lng)})
            json['success'] = True
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')