/**
 * @class Ext.app.DeviceWindow
 * @extends Ext.window.Window
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Ext.app.DeviceWindow', {
            extend: 'Ext.window.Window',
            alias: 'widget.devicewindow',

            height: 200,
            width: 400,
            layout: 'fit',
            modal: true,
            resizable: false,
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
                            allowBlank : false,
                            minLength : 30
                        },
                        {
                            xtype : 'textfield',
                            name : 'name',
                            fieldLabel : 'Short name',
                            allowBlank : false,
                            minLength : 10
                        },
                        {
                            xtype : 'textfield',
                            name : 'text',
                            fieldLabel : 'Description',
                            allowBlank : false,
                            minLength : 30
                        },
                        {
                            // TODO marker select
                            xtype : 'markercombobox',
                            itemId: 'markercombobox',
                            name : 'marker',
                            fieldLabel : 'Marker',
                            allowBlank : true,
                            minLength : 30
                        }
                    ]
                }
            ],
            buttons: [
                {
                    text: 'OK',
                    handler: function() {
                        // TODO add ok code
                        var form = this.up('window').down('form').getForm();
                        form.submit({
                                    clientValidation : true,
                                    url : '/adddevice/',
                                    success : function(form, action) {
                                        // TODO action after device added
                                        this.up('window').close();
                                    },
                                    failure : function(form, action) {
                                        if (action.failureType == 'server') {
                                            obj = Ext.JSON.decode(action.response.responseText);
                                            Ext.Msg.alert('Error!', obj.errors.reason);
                                        } else {
                                            Ext.Msg.alert('Warning!', 'Server is unreachable : ' + action.response.responseText);
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