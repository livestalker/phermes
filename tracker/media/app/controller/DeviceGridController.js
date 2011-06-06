/**
 * @class Tracker.controller.DevicesController
 * @extends Ext.app.Controller
 *
 Controller for handle login and register forms

 * @constructor
 * @param {Object} config The config object
 */
Ext.require([
    'Tracker.store.Devices',
    'Tracker.store.MarkersImg',
    'Tracker.store.CurrentGeos',
    'Tracker.model.Device',
    'Tracker.model.MarkerImg',
    'Tracker.model.CurrentGeo'
    ]);

Ext.define('Tracker.controller.DeviceGridController', {
    extend: 'Ext.app.Controller',
    
    views: ['panel.DeviceGrid', 'map.MapPanel'],
    stores: ['Devices', 'MarkersImg', 'CurrentGeos'],
    models: ['Device', 'MarkerImg', 'CurrentGeo'],
    refs: [
    {
        ref: 'deviceGrid',
        selector: 'devicegrid'
    },
    {
        ref: 'mapPanel',
        selector: 'mappanel'
    }
    ],
    init: function() {
        this.control({
            'devicegrid': {
                itemclick: this.deviceClick
            }
        });
        var devices = this.getDevicesStore(),
        currentGeos = this.getCurrentGeosStore(),
        markersImg = this.getMarkersImgStore();
        
        devices.on('load', this.devicesStoreLoad, this);
        currentGeos.load();
        markersImg.load();
        devices.load();
    },
    deviceClick: function(dv, record, item, index, e) {
    // TODO select marker on map
    },
    devicesStoreLoad: function(store, records, successful, operation) {
        var currentGeos = this.getCurrentGeosStore(),
        markersImg = this.getMarkersImgStore(),
        mapPanel = this.getMapPanel(),
        marker = null,
        icon   = null,
        device_id,
        marker_id,
        size_icon,
        offset_icon,
        markerImg = null,
        currentGeo = null,
        lonlat = null;
        if(successful) {
            for(i in records){
                device_id = records[i].get('device_id');
                marker_id = records[i].get('marker_id');
                markerImg = markersImg.getById(marker_id);
                currentGeo = currentGeos.getById(device_id);
                // create icon
                size_icon = new OpenLayers.Size(markerImg.get('width'), markerImg.get('height'));
                offset_icon = new OpenLayers.Pixel(-(size_icon.w/2), -size_icon.h);
                icon = new OpenLayers.Icon(markerImg.get('url'), size_icon, offset_icon);
                // create marker
                lonlat = mapPanel.createLonLat(currentGeo.get('lng'), currentGeo.get('lat'));
                marker = new OpenLayers.Marker(lonlat, icon);
                records[i].set('marker', marker);
                mapPanel.getMarkerLayer().addMarker(marker);
            }
        }
    }
});