from django.contrib import admin
from phermes.tracker.models import Device
from phermes.tracker.models import Track
from phermes.tracker.models import MapMarker

class DeviceAdmin(admin.ModelAdmin):
    list_display = ('device_id', 'user_id', 'name', 'ts_time',)
    search_fields = ('name', 'imei',)
    list_filter = ('user_id', 'ts_time',)
    date_hierarchy = 'ts_time'
    ordering = ('device_id',)

class TrackAdmin(admin.ModelAdmin):
    list_display = ('device_id', 'long', 'lat', 'ts_time',)
    list_filter = ('device_id',)
    ordering = ('device_id',)

class MapMarkerAdmin(admin.ModelAdmin):
    list_display = ('marker_id', 'width', 'height', 'url',)
    ordering = ('marker_id',)

admin.site.register(Device, DeviceAdmin)
admin.site.register(Track, TrackAdmin)
admin.site.register(MapMarker, MapMarkerAdmin)