/**
 * @class Tracker.view.window.AddDeviceWindow
 * @extends Tracker.view.window.DeviceWindow
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */

Ext.require([
    'Tracker.view.window.DeviceWindow'
    ]);

Ext.define('Tracker.view.window.AddDeviceWindow', {
    extend: 'Tracker.view.window.DeviceWindow',
    alias: 'widget.adddevicewindow',

    title: 'Add device',
    actionUrl: '/adddevice/'

});