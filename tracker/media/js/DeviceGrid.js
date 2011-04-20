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
                autoLoad: true
            },
            columns: [
                {header: 'Name',  dataIndex: 'name'},
                {header: 'Text', dataIndex: 'text'},
                {header: 'IMEI', dataIndex: 'imei'}
            ],
            listeners: {
                itemclick: function (dv, record, item, index, e) {
                    // TODO optimize me
                    // TODO long issue
                    var lat = record.data['lat'];
                    var lon = record.data['long'];
                    this.map.refreshMarker(record);
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