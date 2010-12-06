from django.http import HttpResponse
from django.shortcuts import render_to_response
from django.utils.simplejson.encoder import JSONEncoder

# account functions
def login(request):
    if request.method == 'POST':
        if request.is_ajax():            
            #username = request.POST.get('username')
            #password = request.POST.get('password')
            # TODO make login into system
            json = {}
            json['success'] = True
            json['errors'] = {}
            json['errors']['reason'] = 'Login failed. Try again.'
            return HttpResponse(JSONEncoder().encode(json), mimetype='application/json')
    return render_to_response('login.html')

def logout(request):
    return render_to_response('login.html')

def signup(request):
    return render_to_response('login.html')

def account(request):
    return render_to_response('login.html')

# tracker function
def tracker(request):
    return render_to_response('login.html')

