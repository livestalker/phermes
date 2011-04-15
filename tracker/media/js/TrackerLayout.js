Ext.require(['*']);
Ext.onReady(function() {
    Ext.create('Ext.Viewport', {
        layout: {
            type: 'border',
            padding: 5
        },
        items: [{
            region: 'center',
            xtype: 'mappanel',
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
                flex: 2
            }, {
                xtype: 'panel',
                flex: 1
            }]
        }]
    });
});