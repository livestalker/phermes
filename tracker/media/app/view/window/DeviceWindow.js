/**
 * @class Tracker.view.window.DeviceWindow
 * @extends Ext.window.Window
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */

Ext.require([
    'Tracker.view.controls.MarkerImgComboBox'
    ]);

Ext.define('Tracker.view.window.DeviceWindow', {
    extend: 'Ext.window.Window',
    alias: 'widget.devicewindow',
    initComponent : function() {
        this.callParent();
        if (this.action == 'edit'){
            // Add hide field for device_id
            this.getForm().add([{
                xtype : 'hiddenfield',
                name : 'device_id'
            }]);
        }
    },
    height: 200,
    width: 400,
    layout: 'fit',
    modal: true,
    resizable: false,
    deviceStore: undefined,
    items: [
    {
        xtype: 'form',
        itemId: 'form',
        bodyPadding : 10,
        border: 0,
        bodyCls: 'bg-std',
        defaults : {
            anchor : '100%'
        },
        fieldDefaults : {
            labelAlign : 'left',
            msgTarget : 'none',
            invalidCls : ''
        },
        items : [
        {
            xtype : 'textfield',
            name : 'imei',
            fieldLabel : 'IMEI',
            allowBlank : false
        },
        {
            xtype : 'textfield',
            name : 'name',
            fieldLabel : 'Short name',
            allowBlank : false
        },
        {
            xtype : 'textfield',
            name : 'text',
            fieldLabel : 'Description',
            allowBlank : false
        },
        {
            xtype : 'markerimgcombobox',
            itemId: 'markerimgcombobox',
            name : 'marker_id',
            fieldLabel : 'Marker',
            allowBlank : true
        }
        ]
    }
    ],
    buttons: [
    {
        text: 'Save',
        action: 'save'
    },
    {
        text: 'Cancel',
        action: 'cancel'
    }
    ],
    getMarkerImgComboBox: function() {
        // TODO optimize me
        return this.getComponent('form').getComponent('markerimgcombobox');
    },
    getForm: function() {
        return this.down('form');
    }
});