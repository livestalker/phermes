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
        text: 'OK',
        handler: function() {
            var win = this.up('window');
            var form = win.down('form').getForm();
            form.submit({
                // TODO client validation?
                //clientValidation : true,
                url : win.actionUrl,
                success : function(form, action) {
                    if (win.deviceStore)
                        win.deviceStore.load();
                    win.close();
                },
                failure : function(form, action) {
                    if (action.failureType == 'server') {
                        obj = Ext.JSON.decode(action.response.responseText);
                        Ext.Msg.alert('Error!', obj.errors.reason);
                    } else {
                        Ext.Msg.alert('Warning!', 'Server is unreachable.');
                    }
                }
            });
        }
    },
    {
        text: 'Cancel',
        handler: function() {
            this.up('window').close();
        }
    }
    ],
    getMarkerComboBox: function() {
        // TODO optimize me
        return this.getComponent('form').getComponent('markercombobox');
    }
});