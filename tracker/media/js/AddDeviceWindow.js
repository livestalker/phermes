Ext.define('Ext.app.AddDeviceWindow', {
            extend: 'Ext.window.Window',
            alias: 'widget.adddevicewindow',
            height: 200,
            width: 400,
            layout: 'fit',
            modal: true,
            resizable: false,
            items: [
                {
                    xtype: 'form',
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
                            name : 'description',
                            fieldLabel : 'Description',
                            allowBlank : false,
                            minLength : 30
                        },
                        {
                            xtype : 'textfield',
                            name : 'marker',
                            fieldLabel : 'Marker',
                            allowBlank : true,
                            minLength : 30
                        }
                    ]
                }
            ]
            /**            buttons: [
             {
             text: 'OK',
             handler: function() {
             this.up('window').close();
             // TODO add ok code
             }
             },
             {
             text: 'Cancel',
             handler: function() {
             this.up('window').close();
             // TODO add cancel code
             }
             }
             ]**/
        });