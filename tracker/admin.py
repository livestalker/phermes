from django.contrib import admin
from phermes.tracker.models import Device
from phermes.tracker.models import Track
from phermes.tracker.models import MapMarker
from phermes.tracker.models import CurrentGeo

class DeviceAdmin(admin.ModelAdmin):
    list_display = ('device_id', 'user_id', 'name',)
    search_fields = ('name', 'imei',)
    list_filter = ('user_id',)
    ordering = ('device_id',)

class TrackAdmin(admin.ModelAdmin):
    list_display = ('device_id', 'long', 'lat', 'ts_time',)
    list_filter = ('device_id',)
    ordering = ('device_id',)

class MapMarkerAdmin(admin.ModelAdmin):
    list_display = ('marker_id', 'width', 'height', 'url',)
    ordering = ('marker_id',)

class CurrentGeoAdmin(admin.ModelAdmin):
    list_display = ('lat', 'lng', 'ts_time',)

admin.site.register(Device, DeviceAdmin)
admin.site.register(Track, TrackAdmin)
admin.site.register(MapMarker, MapMarkerAdmin)
admin.site.register(CurrentGeo, CurrentGeoAdmin)