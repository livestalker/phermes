Ext.require([
    'Tracker.view.map.MapPanel',
    'Tracker.view.panel.DeviceGrid',
    'Tracker.view.panel.InfoPanel',
    'Tracker.view.window.DeviceWindow'    
    ]);

Ext.define('Tracker.view.TrackerViewport', {
    extend : 'Ext.container.Viewport',
    
    layout: {
        type: 'border',
        padding: 5
    },
    items: [{
        region: 'center',
        xtype: 'mappanel'        
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
            flex: 2
        }, {
            xtype: 'infopanel',            
            flex: 3
        }]
    }]
});