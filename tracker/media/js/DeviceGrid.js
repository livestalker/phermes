Ext.define('Ext.app.DeviceGrid', {
            extend: 'Ext.grid.Panel',
            alias: 'widget.devicegrid',
            title: 'Tracked objects',
            autoScroll: true,
            columnLines: true,
            map: undefined,
            viewConfig: {
                stripeRows: true
            },
            store: {
                // TODO long issue
                fields: ['device_id', 'imei', 'name', 'text', 'long', 'lat', 'ts_time'],
                proxy: {
                    type: 'ajax',
                    actionMethods: {
                        create : 'POST',
                        read   : 'POST',
                        update : 'POST',
                        destroy: 'POST'
                    },
                    url : '/listdevices/'
                },
                autoLoad: true,
                listeners: {
                    load: function(store, records, successful) {
                        // TODO create markers
                        alert(1);
                    }
                }
            },
            columns: [
                {header: 'Name',  dataIndex: 'name'},
                {header: 'Text', dataIndex: 'text'},
                {header: 'IMEI', dataIndex: 'imei'}
            ],
            listeners: {
                itemclick: function (dv, record, item, index, e) {
                    this.map.selectMarker(record);
                }
            },
            initComponent : function() {
                this.callParent();
            },
            afterRender : function() {
                this.callParent();
                this.map = this.ownerCt.ownerCt.getComponent('mappanel');
            }
        }
)