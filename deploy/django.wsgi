import os
import sys

path = '/home/aleksio/dev/web'
if path not in sys.path:
    sys.path.append(path)

os.environ['DJANGO_SETTINGS_MODULE'] = 'phermes.settings'

import django.core.handlers.wsgi
application = django.core.handlers.wsgi.WSGIHandler()