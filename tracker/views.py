from django.http import HttpResponse, HttpResponseRedirect
from django.core.context_processors import csrf
from django.contrib import auth
from django.shortcuts import render_to_response
from django.utils.simplejson.encoder import JSONEncoder
from pgermes.tracker.models import Device

# account functions
def index(request):
    """
    Main page. If request not AJAX open index page
    """
    c = {}
    c.update(csrf(request))
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
                #return HttpResponseRedirect('/tracker/')
            else:
                json['success'] = False
                json['errors'] = {}
                json['errors']['reason'] = 'Login failed. Try again.'
                return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
                #return HttpResponseRedirect('/')
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

# tracker AJAX functions
def list_devices(request):
    if not request.user.is_authenticated():
        return HttpResponseRedirect('/tracker/login/')
    else:
        if request.method == 'POST' and request.is_ajax():
            json = []
            for d in Device.objects.filter(user_id = request.user.id):
                json.append({'device_id' : d.device_id, 'imei' : d.imei, 'name' : d.name, 'text' : d.text, 'long' : d.long, 'lat' : d.lat, 'long' : d.long, 'ts_time' : str(d.ts_time)})
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
        else:
            return HttpResponseRedirect('/tracker/')


