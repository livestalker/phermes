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
            initComponent : function() {
                // TODO Redifine model when using MVC
                Ext.define('Marker', {
                            extend: 'Ext.data.Model',
                            fields: [
                                {type: 'int', name: 'marker_id'},
                                {type: 'int', name: 'width', defaultValue: 16},
                                {type: 'int', name: 'height', defaultValue: 16},
                                {type: 'string', name: 'url'},
                                {type: 'string', name: 'name'}
                            ]
                        });
                this.markerStore = new Ext.data.Store({
                            model: 'Marker',
                            proxy: {
                                type: 'ajax',
                                url : '/listmarkers/',
                                actionMethods: {
                                    create : 'POST',
                                    read   : 'POST',
                                    update : 'POST',
                                    destroy: 'POST'
                                }
                            }
                        });
                this.markerStore.load();
                this.callParent();
            },
            bbar: [
                {
                    xtype: 'button',
                    text: 'Add',
                    title: 'Add new device',
                    iconCls: 'add',
                    handler: function() {
                        var grid = this.up('panel');
                        var win = Ext.widget('adddevicewindow', {});
                        win.getMarkerComboBox().setData(grid.markerStore.data);
                        win.show();
                    }
                },
                {
                    xtype: 'button',
                    text: 'Edit',
                    title: 'Edit device parameters',
                    iconCls: 'edit',
                    handler: function() {
                        var grid = this.up('panel');
                        var records = grid.getSelectionModel().getSelection();
                        if (records.length > 0) {
                            var win = Ext.widget('editdevicewindow', {});
                            var grid = this.up('panel');
                            var form = win.getForm();
                            form.loadRecord(records[0]);
                            win.getMarkerComboBox().setData(grid.markerStore.data);
                            // TODO select current marker
                            win.show();
                        }
                        else
                            Ext.Msg.alert('Error!', 'Please select device.');
                    }
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