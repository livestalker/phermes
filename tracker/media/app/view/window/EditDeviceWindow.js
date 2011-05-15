/**
 * @class Tracker.view.window.EditDeviceWindow
 * @extends Tracker.view.window.DeviceWindow
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */

Ext.require([
    'Tracker.view.window.DeviceWindow'
    ]);

Ext.define('Tracker.view.window.EditDeviceWindow', {
    extend: 'Tracker.view.window.DeviceWindow',
    alias: 'widget.editdevicewindow',

    title: 'Edit device options',
    actionUrl: '/editdevice/',
    initComponent : function() {
        this.callParent();
        // Add hide field for device_id
        this.getForm().add([{
            xtype : 'hiddenfield',
            name : 'device_id'
        }]);
    },
    getForm: function() {
        return this.down('form');
    }
});