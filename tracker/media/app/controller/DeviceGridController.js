/**
 * @class Tracker.controller.DeviceGridController
 * @extends Ext.app.Controller
 *
 Controller for handle loading stores and device grid

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
    },
    {
        ref: 'viewport',
        selector: 'viewport'
    }
    ],
    /**
     * Init controller
     */
    init: function() {
        // bind event handlers to components
        this.control({
            'devicegrid': {
                itemclick: this.deviceClick
            },
            'viewport': {
                afterrender: this.startLoad
            }
        });
        // link events handlers to stores
        this.getDevicesStore().on('load', this.devicesStoreLoad, this);
        this.getCurrentGeosStore().on('load', this.currentGeosStoreLoad, this);
        this.getMarkersImgStore().on('load', this.markersImgStoreLoad, this);        
    },
    /**
     * Fire when user click on device grid item
     */
    deviceClick: function(dv, record, item, index, e) {
        var marker = record.get('marker');
        this.getMapPanel().setCenter(marker.lonlat);
    },
    /**
     * Start loading stores, fire after viewport rendered
     */
    startLoad: function() {
        this.deviceGridSetMask(true, 'Loading marker images...');
        this.getMarkersImgStore().load();
    },
    /**
     * Fire when markersImg store loaded
     */
    markersImgStoreLoad: function(store, records, successful, operation) {
        if(successful) {
            this.deviceGridSetMask(false);
            this.deviceGridSetMask(true, 'Loading current geo information...');
            this.getCurrentGeosStore().load();
        }
    },
    /**
     * Fire when currentGeos store loaded
     */
    currentGeosStoreLoad: function(store, records, successful, operation) {
        if(successful) {
            this.deviceGridSetMask(false);
            this.deviceGridSetMask(true, 'Loading devices...');
            this.getDevicesStore().load();
        }
    },
    /**
     * Fire when devices store loaded
     */
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
            this.deviceGridSetMask(false);
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
    },
    /**
     * Set/Unset load mask on viewport
     */
    deviceGridSetMask: function(setmask, msg){
        var dg = this.getViewport();
        if (setmask)
            dg.setLoading(msg);
        else
            dg.setLoading(setmask);
    }
});