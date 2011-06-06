Ext.define('Tracker.store.Devices', {
    extend : 'Ext.data.Store',
     alias: 'store.devices',
    
    model: 'Tracker.model.Device',    
    proxy: {
        type: 'ajax',
        api: {
            read: '/listdevices/',
            create: '/adddevice/',
            update: '/editdevice/'
        },
        actionMethods: {
            create : 'POST',
            read   : 'POST',
            update : 'POST',
            destroy: 'POST'
        },
        reader: {
            type: 'json',
            root: 'devices',
            successProperty: 'success'
        },
        writer: {
            allowSingle: false,
            writeAllFields: true,
            root: 'devices'
        }
    }
});