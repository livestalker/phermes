Ext.define('Ext.app.DeviceGrid', {
            extend: 'Ext.grid.Panel',
            alias: 'widget.devicegrid',
            title: 'Tracked objects',
            autoScroll: true,
            columnLines: true,
            viewConfig: {
                stripeRows: true
            },
            store: {
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
            ]
        }
)