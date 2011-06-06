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
            },
            'store Devices': {
                load: this.deviceClick
            }
        });
        var devices = this.getDevicesStore(),
            currentgeos = this.getCurrentGeosStore(),
            markersImg = this.getMarkersImgStore();
        
        devices.on('load', this.deviceClick, this);
        devices.load();        
    },
    deviceClick: function(dv, record, item, index, e) {
        alert(1);
    }
});