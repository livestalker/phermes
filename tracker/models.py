from django.contrib.admin.models import User
from django.db import models

# http://docs.djangoproject.com/en/1.2/ref/models/fields/#model-field-types

# devices of users
# device_id - ID device
# user_id   - ID user
# type      - Model of device
# imei      - IMEI
# name      - name of object
# text      - description
# longitude - long
# latitude  - lat
# ts_time   - last time of request

class Device(models.Model):
    device_id = models.AutoField(primary_key=True, verbose_name='ID')
    user_id   = models.ForeignKey(User, db_column='user_id', verbose_name='User ID')
    type      = models.CharField(max_length=30)
    imei      = models.CharField(max_length=30, verbose_name = 'IMEI')
    name      = models.CharField(max_length=30)
    text      = models.CharField(max_length=255)
    long      = models.FloatField(verbose_name = 'Longitude')
    lat       = models.FloatField(verbose_name = 'Latitude')
    ts_time   = models.DateTimeField(verbose_name = 'Last time request')

# history of move object
# device_id - ID device
# long      - logitude
# lat       - latitude
# ts_time   - time stamp
class Track(models.Model):
    device_id = models.ForeignKey(User, db_column='device_id', verbose_name='Device ID')
    long      = models.FloatField(verbose_name = 'Longitude')
    lat       = models.FloatField(verbose_name = 'Latitude')
    ts_time   = models.DateTimeField(verbose_name = 'Time stamp')