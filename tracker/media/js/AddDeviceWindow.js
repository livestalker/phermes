Ext.define('Ext.app.AddDeviceWindow', {
            extend: 'Ext.window.Window',
            alias: 'widget.adddevicewindow',
            height: 200,
            width: 400,
            layout: 'fit',
            modal: true,
            items: [
                {xtype: 'form'}
            ],
            buttons: [
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
            ]
        });