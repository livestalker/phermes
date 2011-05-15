Ext.define('Tracker.store.Devices', {
    extend : 'Ext.data.Store',
    
    model: 'Tracker.model.Device',    
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
});