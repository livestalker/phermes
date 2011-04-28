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
    user_id = models.ForeignKey(User, db_column='user_id', verbose_name='user ID')
    marker_id = models.ForeignKey('MapMarker', db_column='marker_id')
    type = models.CharField(max_length=30)
    imei = models.CharField(max_length=30, verbose_name='IMEI')
    name = models.CharField(max_length=30)
    text = models.CharField(max_length=255)
    long = models.DecimalField(verbose_name='longitude', max_digits=10, decimal_places=6)
    lat = models.DecimalField(verbose_name='latitude', max_digits=10, decimal_places=6)
    ts_time = models.DateTimeField(verbose_name='last time request')

# history of move object
# device_id - ID device
# long      - logitude
# lat       - latitude
# ts_time   - time stamp
class Track(models.Model):
    device_id = models.ForeignKey(User, db_column='device_id', verbose_name='Device ID')
    long = models.DecimalField(verbose_name='longitude', max_digits=10, decimal_places=6)
    lat = models.DecimalField(verbose_name='latitude', max_digits=10, decimal_places=6)
    ts_time = models.DateTimeField(verbose_name='Time stamp')

# different markers for map
# marker_id - ID marker
# height    - height of marker
# width     - width of marker
# url       - relative/absolute url of marker
# name      - name
class MapMarker(models.Model):
    marker_id = models.AutoField(primary_key=True)
    width = models.IntegerField()
    height = models.IntegerField()
    url = models.CharField(max_length=200)
    name = models.CharField(max_length=15)

    def __unicode__(self):
        return self.name
