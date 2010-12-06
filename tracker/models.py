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
    device_id = models.AutoField(primary_key=True)
    user_id   = models.ForeignKey(User, db_column='user_id')
    type      = models.CharField(max_length=30)
    imei      = models.CharField(max_length=30)
    name      = models.CharField(max_length=30)
    text      = models.CharField(max_length=255)
    long      = models.FloatField()
    lat       = models.FloatField()
    ts_time   = models.DateTimeField(auto_now=True)




