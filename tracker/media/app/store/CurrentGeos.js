Ext.define('Tracker.store.CurrentGeos', {
    extend : 'Ext.data.Store',
    
    model: 'Tracker.model.CurrentGeo',
    proxy: {
        type: 'ajax',
        url : '/currentgeos/',
        actionMethods: {
            create : 'POST',
            read   : 'POST',
            update : 'POST',
            destroy: 'POST'
        },
        reader: {
            type: 'json',
            root: 'currentgeos',
            successProperty: 'success'
        }
    }    
});