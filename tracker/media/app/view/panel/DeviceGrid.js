/**
 * @class Tracker.view.panel.DeviceGrid
 * @extends Ext.grid.Panel
 *
 Class for display and work with tracker devices.

 * @constructor
 * @param {Object} config The config object
 */
Ext.define('Tracker.view.panel.DeviceGrid', {
    extend: 'Ext.grid.Panel',
    alias: 'widget.devicegrid',

    title: 'Tracked objects',
    autoScroll: true,
    columnLines: true,
    map: undefined,
    initComponent : function() {
        this.callParent();
    },
    bbar: [
    {
        xtype: 'button',
        text: 'Add',
        iconCls: 'add',
        handler: function() {
            var grid = this.up('panel');
            var win = Ext.widget('adddevicewindow', {
                deviceStore: grid.store
            });
            win.getMarkerComboBox().setData(grid.markerStore.data);
            win.show();
        }
    },
    {
        xtype: 'button',
        text: 'Edit',
        iconCls: 'edit',
        handler: function() {
            var grid = this.up('panel');
            var records = grid.getSelectionModel().getSelection();
            if (records.length > 0) {
                var win = Ext.widget('editdevicewindow', {
                    deviceStore: grid.store
                });
                var grid = this.up('panel');
                var form = win.getForm();
                win.getMarkerComboBox().setData(grid.markerStore.data);
                form.loadRecord(records[0]);
                win.show();
            }
            else
                Ext.Msg.alert('Error!', 'Please select device.');
        }
    },
    {
        xtype: 'button',
        text: 'Delete',
        iconCls: 'del'
    }
    ],
    viewConfig: {
        stripeRows: true
    },
    store: 'Devices',
    columns: [
    {
        header: 'Name',  
        dataIndex: 'name'
    },

    {
        header: 'Text', 
        dataIndex: 'text'
    },

    {
        header: 'IMEI', 
        dataIndex: 'imei'
    }
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
);