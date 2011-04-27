/**
 * @class Ext.app.EditDeviceWindow
 * @extends Ext.app.DeviceWindow
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Ext.app.AddDeviceWindow', {
            extend: 'Ext.app.DeviceWindow',
            alias: 'widget.editdevicewindow',

            title: 'Edit device parameters',

            getForm: function() {
                return this.down('form').getForm();
            }
        });