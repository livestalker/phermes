Ext.define('Tracker.store.CurrentGeos', {
    extend : 'Ext.data.Store',
    alias: 'store.currentgeos',
    
    model: 'Tracker.model.CurrentGeo',
    proxy: {
        type: 'ajax',
        url : '/currentgeo/',
        actionMethods: {
            create : 'POST',
            read   : 'POST',
            update : 'POST',
            destroy: 'POST'
        }
    },
    reader: {
        type: 'json',
        root: 'currentgeos',
        successProperty: 'success'
    }
});