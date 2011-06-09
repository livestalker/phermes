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
    'Tracker.model.MarkerImg',
    ]);

Ext.define('Tracker.controller.DeviceGridToolBarController', {
    extend: 'Ext.app.Controller',
    
    views: ['panel.DeviceGrid', 'window.DeviceWindow'],
    stores: ['Devices', 'MarkersImg'],
    models: ['Device', 'MarkerImg'],
    refs: [
    {
        ref: 'deviceGrid',
        selector: 'devicegrid'
    },
    {
        ref: 'deviceWindow',
        selector: 'devicewindow'
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
            },
            'devicewindow button[action=save]': {
                click: this.saveDevice
            },
            'devicewindow button[action=cancel]': {
                click: this.cancelSave  
            }
        });
    },
    /**
     * 
     */
    addDevice: function() {
        var win = Ext.widget('devicewindow', {
            title: 'Add device',
            action: 'add'
        });
        win.getMarkerImgComboBox().setData(this.getMarkersImgStore().data);
        win.show();
    },
    /**
     * 
     */
    editDevice: function() {
        var grid = this.getDeviceGrid();
        var records = grid.getSelectionModel().getSelection();
        if (records.length > 0) {
            var win = Ext.widget('devicewindow', {
                title: 'Edit device options',
                action: 'edit'
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
        this.showMsg('title', 'test');
    },
    /**
     * Save new Device or save changes
     */
    saveDevice: function() {
        var win = this.getDeviceWindow(),
        form = win.getForm(),
        record = form.getRecord(),        
        values = form.getValues();
        if (win.action == 'edit')
            record.set(values);
        else
            this.getDevicesStore().add(values);
        win.close();
        this.getDevicesStore().sync();
    },
    cancelSave: function() {
        var win = this.getDeviceWindow();
        win.close();
    },
    showMsg: function(title, text, format) {
        var msgCt = Ext.get('msg-div');
        if(!msgCt){
            msgCt = Ext.core.DomHelper.insertFirst(document.body, {
                id:'msg-div'
            }, true);
        }
        //var s = Ext.String.format.apply(String, Array.prototype.slice.call(arguments, 1));
        var m = Ext.core.DomHelper.append(msgCt, 
            '<div class="msg"><h3>' + 
            title + 
            '</h3><p>' +
            text + 
            '</p></div>', true);
        m.hide();
        m.slideIn('t').ghost('t', {
            delay: 1000, 
            remove: true
        });
    }
});