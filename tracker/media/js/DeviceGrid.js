/**
 * @class Ext.app.DeviceGrid
 * @extends Ext.grid.Panel
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Ext.app.DeviceGrid', {
            extend: 'Ext.grid.Panel',
            alias: 'widget.devicegrid',

            title: 'Tracked objects',
            autoScroll: true,
            columnLines: true,
            map: undefined,
            bbar: [
                {
                    xtype: 'button',
                    text: 'Add',
                    title: 'Add new device',
                    iconCls: 'add',
                    handler: function() {
                        var win = Ext.widget('adddevicewindow', {
                                    title: 'Add new device'                                    
                                });
                        win.show();
                    }
                },
                {
                    xtype: 'button',
                    text: 'Edit',
                    title: 'Edit device parameters',
                    iconCls: 'edit'
                },
                {
                    xtype: 'button',
                    text: 'Delete',
                    title: 'Delete device',
                    iconCls: 'del'
                }
            ],
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
                /**
                 * Fire after click on grid item
                 * @param {Ext.DataView} dv
                 * @param {Ext.data.Model} record
                 * @param {HTMLElement} item
                 * @param {Number} index
                 * @param {Ext.EventObject} e
                 */
                itemclick: function (dv, record, item, index, e) {
                    this.map.selectMarker(record);
                }
            },
            /**
             * Init DeviceGrid component
             */
            initComponent : function() {
                this.callParent();
            },
            /**
             * Fires after the component rendering is finished.
             */
            afterRender : function() {
                this.callParent();
                // Get map pointer
                this.map = this.ownerCt.ownerCt.getComponent('mappanel');
                // Set event listener 'on load' for data store
                this.store.on('load', this.loadStore, this);
            },
            /**
             * Fires after data loaded into store
             * @param {Ext.data.Store} store
             * @param {Array} records
             * @param {Boolean} successful
             */
            loadStore: function(store, records, successful) {
                if (successful)
                // For every record(device) create marker on map
                    for (index in records)
                        this.map.refreshMarker(records[index]);
            }
        }
)