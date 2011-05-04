/**
 * @class Ext.app.EditDeviceWindow
 * @extends Ext.app.DeviceWindow
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Ext.app.EditDeviceWindow', {
            extend: 'Ext.app.DeviceWindow',
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