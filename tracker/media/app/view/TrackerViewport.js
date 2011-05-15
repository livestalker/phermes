Ext.require([
    'Tracker.view.map.MapPanel',
    'Tracker.view.panel.DeviceGrid',
    'Tracker.view.window.AddDeviceWindow',
    'Tracker.view.window.EditDeviceWindow'
    ]);

Ext.define('Tracker.view.TrackerViewport', {
    extend : 'Ext.container.Viewport',
    
    layout: {
        type: 'border',
        padding: 5
    },
    items: [{
        region: 'center',
        xtype: 'mappanel',
        itemId: 'mappanel',
        border: false
    },{
        region: 'east',
        width: 300,
        minWidth: 200,
        maxWidth: 600,
        border: false,
        frame: true,
        split: true,
        layout: {
            type: 'vbox',
            padding: 5,
            align: 'stretch'                
        },
        items: [{            
            xtype: 'devicegrid',
            itemId: 'devicegrid',
            flex: 2
        }, {
            xtype: 'panel',
            itemId: 'infopanel',
            flex: 1
        }]
    }]
});