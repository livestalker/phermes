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
    models: ['Device', 'MarkerImg'],
    refs: [
    {
        ref: 'deviceGrid',
        selector: 'devicegrid'
    }
    ],
    init: function() {
        this.control({
            'devicegrid button[action=add]': {
                click: this.addDevice
            },
            'devicegrid button[action=edit]': {
                click: this.editDevice
            },
            'devicegrid button[action=delete]': {
                click: this.deleteDevice
            }
        });
    },
    /**
     * 
     */
    addDevice: function() {
        var win = Ext.widget('adddevicewindow', {
            deviceStore: this.getDevicesStore()
        });
        win.getMarkerComboBox().setData(this.getMarkersImgStore().data);
        win.show();
    },
    /**
     * 
     */
    editDevice: function() {
        var grid = this.getDeviceGrid();
        var records = grid.getSelectionModel().getSelection();
        if (records.length > 0) {
            var win = Ext.widget('editdevicewindow', {
                deviceStore: this.getDevicesStore()
            });
            var form = win.getForm();
            win.getMarkerImgComboBox().setData(this.getMarkersImgStore().data);
            form.loadRecord(records[0]);
            win.show();
        }
        else
            Ext.Msg.alert('Error!', 'Please select device.');   
    },
    /**
     * 
     */
    deleteDevice: function() {
        
    }
});