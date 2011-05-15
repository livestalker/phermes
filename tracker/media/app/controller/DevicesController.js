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
    'Tracker.model.Device',
    'Tracker.model.MarkerImg'
    ]);

Ext.define('Tracker.controller.DevicesController', {
    extend: 'Ext.app.Controller',
    
    views: ['panel.DeviceGrid'],
    stores: ['Devices', 'MarkersImg'],
    models: ['Device', 'MarkerImg']
    
});