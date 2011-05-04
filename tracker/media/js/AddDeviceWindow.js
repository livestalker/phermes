/**
 * @class Ext.app.AddDeviceWindow
 * @extends Ext.app.DeviceWindow
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */

Ext.define('Ext.app.AddDeviceWindow', {
            extend: 'Ext.app.DeviceWindow',
            alias: 'widget.adddevicewindow',

            title: 'Add device',
            actionUrl: '/adddevice/'

        });