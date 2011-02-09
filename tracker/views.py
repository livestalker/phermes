from django.http import HttpResponse,HttpResponseRedirect
from django.core.context_processors import csrf
from django.contrib import auth
from django.shortcuts import render_to_response
from django.utils.simplejson.encoder import JSONEncoder
from pgermes.tracker.models import Device

def index(request):
    return render_to_response('index.html')

# account functions
def login(request):
    """
    Login page. If request not AJAX open window for login, else auth user
    """
    c = {}
    c.update(csrf(request))
    # TODO redirect if user already auth
    if request.method == 'POST':
    # TODO remove test ajax
        if not request.is_ajax():
            username = request.POST.get('username', '')
            password = request.POST.get('password', '')
            user = auth.authenticate(username = username, password = password)
            if user is not None and user.is_active:
                auth.login(request, user)
                return HttpResponseRedirect('/tracker/')
            else:
                return HttpResponseRedirect('/login/')
    return render_to_response('login.html', c)

def logout(request):
    """
    Logout from system
    """
    auth.logout(request)
    return HttpResponseRedirect('/tracker/login/')

def signup(request):
    return render_to_response('login.html')

def account(request):
    return render_to_response('login.html')

# tracker function
def tracker(request):
    if not request.user.is_authenticated():
        return HttpResponseRedirect('/tracker/login/')
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


